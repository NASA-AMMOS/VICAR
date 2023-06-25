CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C First pass polygon labeling on a classified image.
C
      SUBROUTINE FLABL(LROW,NROW,CLASS1,ICLAB,MAXPOLY)
      IMPLICIT NONE
      INTEGER*4 LROW(1),NROW(1)		!Last and new image lines
      INTEGER*4 CLASS1(1),ICLAB,MAXPOLY

      COMMON/CINP/SLINE,SSAMP,NLINE,NSAMP
      INTEGER*4 SLINE,SSAMP,NLINE,NSAMP         !Input image size field

      COMMON/FILES/INFILE,LABELEDF
      INTEGER*4 INFILE,LABELEDF		!input is INFILE, output is LABELEDF

      INTEGER*4 ADJLBL,ADJCLS,ITEMP
      INTEGER*4 I,L,IND,IBCOL,IECOL,LABEL,ICLAS
      LOGICAL FIRST,EOR
  101 FORMAT(' Line=',I5)

      ICLAB=0
      IECOL=0

C     ....Label first line
      CALL XVREAD(INFILE,NROW,IND,'LINE',SLINE,'SAMP',SSAMP,
     .             'NSAMPS',NSAMP,' ')

   10 CALL CRUN(NROW,NSAMP,IBCOL,IECOL,ICLAS,EOR)
      IF (EOR) GOTO 15
      IF (ICLAB.GE.MAXPOLY) GOTO 990
      ICLAB=ICLAB+1                ! ICLAB is the polygon number (label).
      CLASS1(ICLAB) = ICLAS
      DO I=IBCOL,IECOL
         NROW(I)=ICLAB
      ENDDO
      GOTO 10

   15 CALL XVWRIT(LABELEDF,NROW,IND,'LINE',1,' ')

C     ....Process the remaining lines
      DO 50 L=2,NLINE
      IF (MOD(L,100).EQ.0) WRITE(6,101) L
      CALL MVE(4,NSAMP,NROW,LROW,1,1)
      CALL XVREAD(INFILE,NROW,IND,'LINE',SLINE+L-1,'SAMP',SSAMP,
     .             'NSAMPS',NSAMP,' ')

   20 CALL CRUN(NROW,NSAMP,IBCOL,IECOL,ICLAS,EOR)
      IF (EOR) GOTO 50
      FIRST=.TRUE.
      ADJLBL=0

C     ....Establish a label for the run
      DO 40 I=IBCOL,IECOL
      IF (LROW(I).EQ.ADJLBL) GOTO 40
C     ....A new run encountered on the adjacent(last) row
      ADJLBL = LROW(I)
      ITEMP = ADJLBL

   22 ADJCLS = CLASS1(ITEMP)
      IF (ADJCLS.GE.0) GOTO 25
      ITEMP = IABS(ADJCLS)		!Follow the link to a class
      GOTO 22

   25 IF(ADJCLS.NE.ICLAS) GOTO 40

      IF (FIRST) THEN
         LABEL = ADJLBL
         FIRST = .FALSE.
         GOTO 40
      ENDIF

C     ..Same class but possibly another label.  Link label to current label.
      IF (ADJLBL.LT.LABEL) THEN
         CLASS1(LABEL) = -ADJLBL	!Negative value denotes link.
         LABEL=ADJLBL
      ELSE
         IF(LABEL.NE.ADJLBL) CLASS1(ADJLBL)=-LABEL
      ENDIF
   40 CONTINUE

C     ....Update run
      IF (FIRST) THEN
         IF (ICLAB.GE.MAXPOLY) GOTO 990
         ICLAB=ICLAB+1
         LABEL=ICLAB
         CLASS1(LABEL) = ICLAS
      ENDIF
      DO I=IBCOL,IECOL
         NROW(I)=LABEL
      ENDDO
      GOTO 20

   50 CALL XVWRIT(LABELEDF,NROW,IND,'LINE',L,' ')

      RETURN

  990 CALL XVMESSAGE('***Maximum number of polygons exceeded',' ')
      CALL ABEND
      END
