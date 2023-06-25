C**************************************************************
C*                                                            *
C*        STEP 3: POLYGON CYCLING                             *
C*                                                            *
C**************************************************************

C INPUTS:  POLYDESF, SERLFILE
C OUTPUTS: VERTICES, POLYINFO

      SUBROUTINE POLYC(SERL,L1,VXREF,L5,XYA,L9,
     +		IVBUF,L11,IVPTR,L13,XYBUF,L16,
     +		TOTAL,NPOLY,NSEGS,NVERT)
      IMPLICIT NONE
      INTEGER*4 L1,L5,L9,L11,L13,L16,TOTAL
      INTEGER*4 SERL(4,1)			!Segment start,end,right,left
      INTEGER*4 VXREF(4,1)			!Up to 4 segments per vertex
      INTEGER*2 XYA(2,1)			!Vertices x-y coordinates
      INTEGER*4 IVBUF(1),IVPTR(1)		!Buffers for current polygon
      INTEGER*2 XYBUF(2,1)
      INTEGER*4 NPOLY,NSEGS,NVERT

      COMMON/CSAVE/IEDGE,ISLCNT,IPVCNT,IPPCNT,LVPTR,IPCNT
      INTEGER*4 IEDGE,ISLCNT,IPVCNT,IPPCNT,LVPTR,IPCNT

      INTEGER*4 NVERT2
      LOGICAL XVPTST,GR1

      IF (L1+L5+L9+L11+L13+L16.NE.TOTAL) CALL MABEND('***STACKA ERROR')
      CALL ZIA(VXREF,4*NVERT)

C     ....Initialize OUTPOL variables
      IEDGE = 0
      IPVCNT = 0
      IPPCNT = 0
      LVPTR = 0
      IPCNT = 0

      GR1 = XVPTST('GR1')
      CALL OPEN_FILES(NPOLY,NVERT,GR1)

      CALL READ_SERLFILE(NSEGS,NVERT,SERL,VXREF,XYA)

      CALL CYCLE(NPOLY,NSEGS,NVERT,SERL,VXREF,XYA,
     +		IVBUF,IVPTR,XYBUF,NVERT2,GR1)

      CALL CLOSE_FILES(NVERT2,NPOLY,GR1)
      NVERT = NVERT2
      RETURN
      END

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Read a polygon record from POLYDESF.
C
      SUBROUTINE GET_POLYGON(IPOLY,iclas,iarea)
      IMPLICIT NONE
      INTEGER*4 IPOLY
      INTEGER*4 ICLAS,IAREA     !Returned polygon class, # of pixels

      COMMON/IBIS_FILES/IBIS_PD
      INTEGER*4 IBIS_PD

      COMMON/IBIS_RECORDS/PD_REC
      INTEGER*4 PD_REC

      INTEGER*4 IB(2),STATUS

      CALL IBIS_RECORD_READ(PD_REC,IB,IPOLY,STATUS)
      IF (STATUS.NE.1) CALL IBIS_SIGNAL(IBIS_PD,STATUS,1)
      ICLAS = IB(1)
      IAREA = IB(2)
      RETURN
      END
