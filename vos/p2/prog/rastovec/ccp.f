CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Add segment to vertex cross-reference table. 
C
      SUBROUTINE CCP(IVRTX,ISEG,VXREF)
      IMPLICIT NONE
      INTEGER*4 IVRTX		!Vertex number
      INTEGER*4 ISEG		!Segment number
      INTEGER*4 VXREF(4,1)	!2 to 4 segments for each vertex

      INTEGER*4 J

C     ....Search for an unused slot
      DO J=1,4
         IF (VXREF(J,IVRTX).EQ.0) GOTO 20
      ENDDO
      CALL XVMESSAGE('***Error in CCP',' ')	!Corrupted X-reference table
      CALL PRNT(4,1,IVRTX,' IVRTX=.')		!or bad SERL file
      CALL PRNT(4,4,VXREF(1,IVRTX),' VXREF=.')
      CALL ABEND

   20 VXREF(J,IVRTX) = ISEG
      IF (J.GT.2) VXREF(J,IVRTX)=-ISEG		!Flag vertices as high-order
      IF (J.EQ.3) THEN
         VXREF(1,IVRTX) = -VXREF(1,IVRTX)
         VXREF(2,IVRTX) = -VXREF(2,IVRTX)
      ENDIF
      RETURN
      END
