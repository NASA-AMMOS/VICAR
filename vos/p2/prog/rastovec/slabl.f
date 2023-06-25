CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Second pass of region labeling.
C The first pass output LABELEDF where each pixel is labeled with a
C preliminary region id.  In the second pass, the regions that need to be
C merged are merged.  Because of the, all regions are relabeled.
C
      SUBROUTINE SLABL(IROW,CLASS1,CLASS2,LBUF,PIXCNT,NLABL)
      IMPLICIT NONE
      INTEGER*4 IROW(1),CLASS1(1),CLASS2(1),LBUF(1),PIXCNT(1)
      INTEGER*4 NLABL
      LOGICAL INDIR,EOR

      COMMON/CINP/SLINE,SSAMP,NLINE,NSAMP
      INTEGER*4 SLINE,SSAMP,NLINE,NSAMP         !Input image size field

      COMMON/FILES/INFILE,LABELEDF	!INFILE not used
      INTEGER*4 INFILE,LABELEDF		!LABELEDF updated

      INTEGER I,L,IND,IBCOL,IECOL,LABEL,NEWL,ICLAS,ITEMP
  101 FORMAT(' Line=',I5)

      CALL XVMESSAGE('Begin second pass of region labeling',' ')
      NLABL=0

      DO 50 L=1,NLINE
      IF (MOD(L,100).EQ.0) WRITE(6,101) L
      CALL XVREAD(LABELEDF,IROW,IND,'LINE',L,' ')
      IECOL=0

   20 CALL CRUN(IROW,NSAMP,IBCOL,IECOL,LABEL,EOR)
      IF (EOR) GOTO 50		!End-Of-Row (line) encountered
      NEWL = LBUF(LABEL)
      IF(NEWL.GT.0) GOTO 30

C     ....Establish new label if required
C     ....Get class
      NEWL=0
      INDIR=.FALSE.
      ITEMP=LABEL
   25 ICLAS = CLASS1(ITEMP)
      ITEMP=IABS(ICLAS)
      IF(ICLAS.GE.0) GOTO 27
      INDIR=.TRUE.
      LABEL=ITEMP
      GOTO 25

C     ....If indirect,use current label
   27 IF (INDIR) THEN
         NEWL = LBUF(LABEL)
         IF(NEWL.GT.0) GOTO 29
      ENDIF
      NLABL = NLABL + 1		!Create new label
      NEWL = NLABL
      CLASS2(NEWL) = ICLAS

   29 LBUF(LABEL) = NEWL

C     ....UPDATE PIXEL COUNT
   30 PIXCNT(NEWL) = PIXCNT(NEWL) + (IECOL-IBCOL) + 1
      DO I=IBCOL,IECOL
         IROW(I)=NEWL
      ENDDO
      GOTO 20

   50 CALL XVWRIT(LABELEDF,IROW,IND,'LINE',L,' ')

      RETURN
      END
