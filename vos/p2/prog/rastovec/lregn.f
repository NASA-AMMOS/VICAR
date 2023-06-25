C**************************************************************
C*                                                            *
C*        STEP 1: REGION LABELING                             *
C*                                                            *
C**************************************************************

C INPUT:   INFILE
C OUTPUT:  LABELEDF, POLYDESF

      SUBROUTINE LREGN(LROW,L1,NROW,L2,CLASS1,L3,CLASS2,L4,
     +		LBUF,L5,PIXCNT,L6,TOTAL,nlabl)
      IMPLICIT NONE
      INTEGER*4 LROW(1),NROW(1)		!Image line buffers
      INTEGER*4 CLASS1(1),LBUF(1)	!Labels and preliminary classes (FLABL)
      INTEGER*4 CLASS2(1),PIXCNT(1)	!Final labels and pixel areas (SLABL)
      INTEGER*4 L1,L2,L3,L4,L5,L6,TOTAL	!Buffer lengths (in bytes)
      INTEGER*4 NLABL			!Number of polygon regions (output)

      COMMON /FILES/ INFILE,LABELEDF,POLYDESF
      INTEGER*4      INFILE,LABELEDF,POLYDESF

      COMMON/CINP/SLINE,SSAMP,NLINE,NSAMP
      INTEGER*4 SLINE,SSAMP,NLINE,NSAMP         !Input image size field

      COMMON/CMAX/MAXPOLY
      INTEGER MAXPOLY

      INTEGER*4 I,IND

      IF (L1+L2+L3+L4+L5+L6.NE.TOTAL) CALL MABEND('***STACKA ERROR')

      DO I=1,MAXPOLY
         CLASS1(I) = 0
         CLASS2(I) = 0
         LBUF(I) = 0
         PIXCNT(I) = 0
      ENDDO

C     ....First step of region labeling
      CALL XVUNIT(LABELEDF,'OUT',1,IND,' ')
      CALL XVOPEN(LABELEDF,IND,'OP','WRITE',
     .          'U_NL',NLINE,'U_NS',NSAMP,
     .          'U_FORMAT','FULL','O_FORMAT','FULL',
     .          'OPEN_ACT','SA','IO_ACT','SA',' ')
      CALL FLABL(LROW,NROW,CLASS1,NLABL,MAXPOLY)
      CALL XVCLOSE(INFILE,IND,' ')
      CALL XVCLOSE(LABELEDF,IND,' ')

C     ....Second step of region labeling
      CALL XVOPEN(LABELEDF,IND,'OP','UPDATE',
     .		'OPEN_ACT','SA','IO_ACT','SA',' ')
      CALL SLABL(LROW,CLASS1,CLASS2,LBUF,PIXCNT,NLABL)
      CALL XVCLOSE(LABELEDF,IND,' ')

      CALL OUTPUT_POLYDESF(NLABL,CLASS2,PIXCNT,POLYDESF)
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Determine the beginning and ending sample of a sequence (run) of pixels
C with the same DN value.
C
      SUBROUTINE CRUN(IROW,NSAMP,IBCOL,IECOL,ICLAS,EOR)
      IMPLICIT NONE
      INTEGER*4 IROW(1)		!Input image line
      INTEGER*4 NSAMP		!Input number of samples
      INTEGER*4 IBCOL,IECOL	!Beginning and ending sample (returned)
      INTEGER*4 ICLAS		!Class for run
      LOGICAL EOR		!End-of-line flag (returned)

C     .... IECOL must be initialized to 0 before first call
      IF (IECOL.GE.NSAMP) THEN
         EOR = .TRUE.
         IECOL = 0
         RETURN
      ENDIF

      EOR = .FALSE.
      IBCOL = IECOL + 1
      ICLAS = IROW(IBCOL)
      IECOL = IBCOL

   10 IF (IECOL.GE.NSAMP) RETURN
      IF (IROW(IECOL+1).NE.ICLAS) RETURN
      IECOL = IECOL + 1
      GOTO 10
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Write class and area to POLYDESF
C
      SUBROUTINE OUTPUT_POLYDESF(NLABL,CLASS2,PIXCNT,POLYDESF)
      IMPLICIT NONE
      INTEGER*4 NLABL,CLASS2(NLABL),PIXCNT(NLABL),POLYDESF

      COMMON/IBIS_FILES/IBIS_PD
      INTEGER*4 IBIS_PD

      INTEGER*4 NCOL,NROW,STATUS
      CHARACTER*4 FMT(2)/'FULL','FULL'/

      CALL XVMESSAGE('Writing polygon description file',' ')
      CALL PRNT(4,1,NLABL,' Number of polygon regions=.')

      CALL XVUNIT(POLYDESF,'OUT',2,STATUS,' ')
      NCOL = 2
      NROW = NLABL
      CALL IBIS_FILE_OPEN(POLYDESF,IBIS_PD,'WRITE',NCOL,NROW,
     +	FMT,' ',STATUS)
      IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS_PD,STATUS,1)
      CALL IBIS_COLUMN_WRITE(IBIS_PD,CLASS2,1,1,NROW,STATUS)
      IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS_PD,STATUS,1)
      CALL IBIS_COLUMN_WRITE(IBIS_PD,PIXCNT,2,1,NROW,STATUS)
      IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS_PD,STATUS,1)
      CALL IBIS_FILE_CLOSE(IBIS_PD,' ',STATUS)
      IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS_PD,STATUS,1)
      RETURN
      END
