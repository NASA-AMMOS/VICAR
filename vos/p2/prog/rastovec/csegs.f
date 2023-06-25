
C**************************************************************
C*                                                            *
C*        STEP 2: Compute segments                            *
C*                                                            *
C**************************************************************

C INPUT:   LABELEDF
C OUTPUT:  SERLFILE

      SUBROUTINE CSEGS(LROW,L1,NROW,L2,VLBUF,L3,HLBUF,L4,
     +		TOTAL,iscnt,ivcnt)
      IMPLICIT NONE
      INTEGER*4 LROW(1),NROW(1)		!Image line buffers
      INTEGER*2 VLBUF(1),HLBUF(1)	!Vertex sampl and line coordinates
      INTEGER*4 L1,L2,L3,L4,TOTAL	!Buffer lengths (in bytes)
      INTEGER*4 ISCNT			!Number of segments (output)
      INTEGER*4 IVCNT			!Number of vertices (output)

      COMMON/CINP/SLINE,SSAMP,NLINE,NSAMP
      INTEGER*4 SLINE,SSAMP,NLINE,NSAMP         !Input image size field

      COMMON/CMAX/MAXPOLY,MAXVERT,MAXSIZE,MAXSEG
      INTEGER MAXPOLY,MAXVERT,MAXSIZE,MAXSEG

      COMMON /FILES/ INFILE,LABELEDF,POLYDESF,SERLFILE
      INTEGER*4      INFILE,LABELEDF,POLYDESF,SERLFILE

      COMMON/IBIS_FILES/IBIS_PD,IBIS_S
      INTEGER*4 IBIS_PD,IBIS_S

      COMMON/IBIS_RECORDS/PD_REC,SERL_REC
      INTEGER*4 PD_REC,SERL_REC

      INTEGER*4 I,IND,IHIGH,COL(10),NCOL,NR,STATUS

      IF (L1+L2+L3+L4.NE.TOTAL) CALL MABEND('***STACKA ERROR')

      DO I=1,MAXVERT
         VLBUF(I) = 0
         HLBUF(I) = 0
      ENDDO
C     ....Open labeled image file
      CALL XVOPEN(LABELEDF,IND,'OPEN_ACT','SA','IO_ACT','SA',' ')

C     ...Open IBIS SERLFILE
      NCOL = 10
      NR = MAXVERT
      CALL XVUNIT(SERLFILE,'OUT',3,STATUS,' ')
      CALL IBIS_FILE_OPEN(SERLFILE,IBIS_S,'WRITE',NCOL,NR,
     +          ' ','ROW',STATUS)
      IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS_S,STATUS,1)
      MAXSEG = NR

      DO I=1,NCOL
         COL(I) = I
         CALL IBIS_COLUMN_SET(IBIS_S,'FORMAT','FULL',I,STATUS)
      ENDDO

      CALL IBIS_RECORD_OPEN(IBIS_S,SERL_REC,' ',COL,NCOL,'FULL',STATUS)
      IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS_S,STATUS,1)

      CALL COMPS(LABELEDF,NLINE,NSAMP,LROW,NROW,VLBUF,HLBUF,
     +		ISCNT,IVCNT,IHIGH)

C     ....Update NROW in SERLFILE label
      CALL IBIS_FILE_SET(IBIS_S,'NR',ISCNT,STATUS)
      IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS_S,STATUS,1)
      CALL IBIS_FILE_CLOSE(IBIS_S,' ',STATUS)
      IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS_S,STATUS,1)

      CALL PRNT(4,1,ISCNT,' Number of segments=.')
      CALL PRNT(4,1,IVCNT,' Number of vertices=.')
      CALL PRNT(4,1,IHIGH,' Number of high order vertices=.')
      CALL XVCLOSE(LABELEDF,IND,' ')
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Copy segment to IB buffer.  If buffer gets full, output to SERLFILE.
C
      SUBROUTINE WSERL(VLBUF,HLBUF,NLINE,ISCNT,ISTART,IEND,
     +          ICLS1,ICLS2)
      IMPLICIT NONE
      INTEGER*2 VLBUF(1),HLBUF(1)
      INTEGER*4 NLINE,ISCNT,ISTART,IEND
      INTEGER*4 ICLS1,ICLS2

      COMMON/IBIS_FILES/IBIS_PD,IBIS_S
      INTEGER*4 IBIS_PD,IBIS_S

      COMMON/IBIS_RECORDS/PD_REC,SERL_REC
      INTEGER*4 PD_REC,SERL_REC

      COMMON/CMAX/MAXPOLY,MAXVERT,MAXSIZE,MAXSEG
      INTEGER MAXPOLY,MAXVERT,MAXSIZE,MAXSEG

      INTEGER*4 IVL,IHL,IXS,IYS,IXE,IYE,IB(10),STATUS

C Calculate coordinates from start and end vertex #s
C     ..Starting vertex
      IVL = VLBUF(ISTART)	!Sample
      IHL = HLBUF(ISTART)	!Line
      IXS = IVL - 1
      IYS = NLINE - (IHL-1)

C     ..End vertex
      IVL = VLBUF(IEND)
      IHL = HLBUF(IEND)
      IXE = IVL-1
      IYE = NLINE - (IHL-1)

      IB(1) = ISCNT	!Segment number (index)
      IB(2) = ISTART	!Starting vertex
      IB(3) = IEND	!Ending vertex
      IB(4) = ICLS1	!Label to the right of segment
      IB(5) = ICLS2	!Label to the left of segment
      IB(6) = IXS	!(x,y) coordinates of starting vertex
      IB(7) = IYS
      IB(8) = IXE	!(x,y) coordinates of ending vertex
      IB(9) = IYE
      IB(10) = 0
      IF (ISCNT.GT.MAXSEG) THEN
         MAXSEG = 1.1*MAXSEG
         CALL IBIS_FILE_SET(IBIS_S,'NR',MAXSEG,STATUS)
         IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS_S,STATUS,1)
         CALL XVMESSAGE('WSERL increased MAXSEG',' ')
      ENDIF
      CALL IBIS_RECORD_WRITE(SERL_REC,IB,ISCNT,STATUS)
      IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS_S,STATUS,1)
      RETURN
      END
