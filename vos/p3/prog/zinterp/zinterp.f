C  PROGRAM "zinterp"

	INCLUDE 'VICMAIN_FOR'
	SUBROUTINE MAIN44
	IMPLICIT NONE

C  PROGRAM ZINTERP
C  PURPOSE ---
C
C	Interpolate elevation values from random control points
C	into a rectangular grid (A "surface" image).
C
C  31 OCT 1994 ...CRI... MSTP S/W CONVERSION (VICAR PORTING)
C
C  INPUT ---
C	NLINES	 Depth of grid/picture
C	NSAMPS	 Width of grid/picture
C	FORMAT	 Specify output format (BYTE,HALF,FULL)
C	EXPONENT Interpolation exponent
C	NUMN	 Sample size (Number of neighbors)
C	RADIUS	 Search radius
C	RADEXP	 Search radius expansion
C	LINECOL	 Column in input that contains LINE values
C	SAMPCOL	 Column in input that contains SAMPLE values
C	ZCOL	 Column in input that contains the Z values
C	LSCALE	 Scale factor applied to LINE before adding LOFFSET
C	SSCALE	 Scale factor applied to SAMPLE before adding SOFFSET
C	ZSCALE	 Scale factor applied to the Z value before adding ZOFFSET
C	LOFFSET	 Offset added to LINE after scale factor LSCALE
C	SOFFSET	 Offset added to SAMPLE after scale factor SSCALE
C	ZOFFSET	 Offset added to the Z value after scale factor ZSCALE
C	TRON	 TRace ON value used for N% or N pixels computed
C	TRONMODE TRON mode [ 1 = % /// 2 = N ]
C
C  OUTPUT ---
C	An output "surface" image is generated
C
C  RESTRICTIONS ---
C	NPTS must be >= sample size
C	Sample size must be >= 3
C	Radius must be >= 0
C	Radius expansion must be > 0 if radius > 0
C
C  SUBROUTINES CALLED ---
C	ABEND		 Stops execution and makes a clean exit
C	GETDIST 	 Computes distance between pixel and control pts
C	IBIS_RECORD_READ Reads row of data from interface file
C	GETZ		 Interpolates Z with distance and Z values
C	NEWPTRS		 Rearrange low/high pointers after each line
C	XVMESSAGE	 Print a message to the user
C	IBIS_FILE_READ	 Opens interface file + misc.
C	INDSRTR		 Sort in an in-core array
C	XLADD		 Add comments to history label
C	IBIS_FILE_CLOSE	 Close an IBIS file from within VICAR/TAE
C	IBIS_FILE_OPEN	 Open an IBIS file from within VICAR/TAE
C	XVP		 Parameter acquisition routine
C	XVUNIT		 Get the unit number
C       XVOPEN           Open a file from within VICAR/TAE
C	XVWRITE 	 Writes an array of data to a file
C
C  COMMENTS ---
C
C	It is recommended that control points not form clusters or be
C	positioned in obvious linear allignments (such as gathering
C	control points close together along a contour line from
C	a topographic map). The control points sould be scattered
C	evenly throughout the region of interest.
C
C	The interpolated values are stored in a local cache. The
C	cache consists of two arrays, NHDIST and NHELEV. The
C	interpolation uses these values for computing the Z value.
C
C	Sorting of elevation values by distance takes increasingly
C	more time as the number of control points increases.
C
C	If a radial search is used, the maximum size of the cache is a
C	function of the desired sample size.  The value for NMAX is set
C	to SFACTR * NUMN.
C
C
C  MODE DECLARATIONS ---
	INTEGER MAXNH, SFACTR, BUFSZ, BYTE, HALF, REAL, FULL, DEFAULT
	PARAMETER (MAXNH = 512)
	PARAMETER (DEFAULT = 1)
	PARAMETER (BYTE = 1)
	PARAMETER (HALF = 2)
	PARAMETER (FULL = 3)
	PARAMETER (REAL = 4)
	PARAMETER (SFACTR = 4)
	PARAMETER (BUFSZ = 5000)
	LOGICAL RADIAL, TRON, EXPAND
	CHARACTER*4 FORMAT
	CHARACTER*72 STRING
	INTEGER TRACE, TOTZ, TRMODE, NBUFF, KEY(BUFSZ), NUMVALID
	INTEGER ROW, NMAX, K, IMAX, I, JK, KJ, NNH
	INTEGER RUNIT, OUTUNIT, COLS(3), STATUS, COUNT, TCLIP
	INTEGER NUMN, LCOL, SCOL, ZCOL, NLINES, NSAMPS
	INTEGER IS, IL, NPTS, NCOL, IBIS, RECORD, KEY2(BUFSZ)
	INTEGER BTM, LOW, HIGH, TOP, OBTM, IP(BUFSZ)
        INTEGER OLOW, OHIGH, OTOP, OUTLIST(MAXNH), DKEY2(MAXNH)
	INTEGER INLIST(MAXNH), DKEY(MAXNH), INSIDE, FRINGE, OUTFORM
	REAL TRCOUNT, TRPERC, TRINC, BTMVAL, LOWVAL, HIGHVAL, TOPVAL
	REAL LS(2), ZMIN, ZMAX, EXPONENT, RADIUS, RADEXP, RDSQR, RDXSQR
	REAL LOFF, SOFF, ZOFF, LSF, SSF, ZSF, CUTOFF, CPL(BUFSZ)
	REAL CPS(BUFSZ), CPZ(BUFSZ), DBUFF(BUFSZ), DBUFF2(BUFSZ)
	REAL Z, CPL2(BUFSZ), BAND(10000), CP(3), ZBUFF(BUFSZ)
	REAL NHDIST(MAXNH), NHELEV(MAXNH), NHDIST2(MAXNH)
C
C  COMMON STATEMENTS ---
C	None
C
C  LOCAL VARIABLE DESCRIPTIONS ---
C	None
C
C-----------*** BEGINNING OF EXECUTABLE CODE ***-----------------

        CALL IFMESSAGE('ZINTERP version 31-OCT-94')
        CALL XVEACTION('SA',' ')

C		Initializations
        RADIAL=.FALSE.

C		Get the size of the output image
C		We will make a rectangle starting
C		at SL=1 SS=1 and ending with NL NS

	CALL XVP ('NL', NLINES, COUNT)
	CALL XVP ('NS', NSAMPS, COUNT)

C		Extract the interpolation parameters

	CALL XVP ('EXPONENT', EXPONENT, COUNT)
	CALL XVP ('NUMN', NUMN, COUNT)
	CALL XVP ('RADIUS', RADIUS, COUNT)
	CALL XVP ('RADEXP', RADEXP, COUNT)
	EXPONENT = EXPONENT / 2.0
	IF (RADIUS.GT.0.0) RADIAL=.TRUE.

C		Open up the IBIS interface file.

        CALL XVUNIT(RUNIT, 'INP', 1, STATUS, ' ')
	CALL IBIS_FILE_OPEN(RUNIT,IBIS,'READ',0,0,' ',' ',STATUS)
        IF (STATUS.NE.1) CALL IBIS_SIGNAL_U(RUNIT,STATUS,1)
        CALL IBIS_FILE_GET(IBIS,'NC',NCOL,1,1)
        CALL IBIS_FILE_GET(IBIS,'NR',NPTS,1,1)

C		Check for too many control points

	IF (NPTS.GT.BUFSZ) GOTO 9100

C		Check for not enough control points

	IF (NPTS.LT.NUMN) GOTO 9300

C		Set the number of nearest neighbors to 5.
C		Also set a default value for RADEXP if none given.

	IF (NUMN.EQ.0) NUMN = 5
	NMAX = NUMN

C		Set up a default radius search if none
C		has been requested (nearest neighbor).
C		This formula assumes even distribution of points.

	IF (.NOT.RADIAL) THEN
	    RADIUS = SQRT(FLOAT(NUMN)/(FLOAT(NPTS)/(NLINES*NSAMPS)))
	ENDIF
	IF (RADEXP.LE.0.0) RADEXP = RADIUS / 3.14159264

C		There should be	three columns given:
C		LINECOL, SAMPCOL, and ZCOL.

	CALL XVP ('LINECOL', LCOL, COUNT)
	CALL XVP ('SAMPCOL', SCOL, COUNT)
	CALL XVP ('ZCOL', ZCOL, COUNT)

C		Check for valid column numbers

	IF (LCOL.GT.NCOL) GOTO 9200
	IF (SCOL.GT.NCOL) GOTO 9200
	IF (ZCOL.GT.NCOL) GOTO 9200

C		Allow for L, S, and Z offsets

	CALL XVP ('LOFFSET',LOFF,COUNT)
	CALL XVP ('SOFFSET',SOFF,COUNT)
	CALL XVP ('ZOFFSET',ZOFF,COUNT)

C		Also allow for L, S, and Z scaling

	CALL XVP ('LSCALE',LSF,COUNT)
	CALL XVP ('SSCALE',SSF,COUNT)
	CALL XVP ('ZSCALE',ZSF,COUNT)

C		A TRace ON would be nice

	CALL XVP ('TRON',TRACE,COUNT)
	CALL XVP ('TRONMODE',TRMODE,COUNT)
	TRON = TRACE.GE.1

C	Read in the Line, Sample and Z values of the control points

	COLS(1) = LCOL
	COLS(2) = SCOL
	COLS(3) = ZCOL
	ZMAX = -1.0E20
        CALL IBIS_RECORD_OPEN(IBIS,RECORD,'FORMAT:REAL',
     &                        COLS,NCOL,'REAL',STATUS)
        IF (STATUS.NE.1) CALL IBIS_SIGNAL_U(RUNIT,STATUS,1)
	DO ROW = 1, NPTS
	    CALL IBIS_RECORD_READ(RECORD, CP, ROW, STATUS)
            IF (STATUS.NE.1) CALL IBIS_SIGNAL_U(IBIS,STATUS,1)
	    CPL(ROW) = (CP(1) * LSF) + LOFF
	    CPS(ROW) = (CP(2) * SSF) + SOFF
	    CPZ(ROW) = (CP(3) * ZSF) + ZOFF
	    IF(CPZ(ROW).GT.ZMAX) ZMAX = CPZ(ROW)
	ENDDO

C		Output format would be appropriate too

	CALL XVP ('FORMAT', FORMAT, COUNT)
	IF (FORMAT(1:4).EQ.'----') THEN
	    OUTFORM = DEFAULT
	ELSEIF (FORMAT(1:4).EQ.'BYTE') THEN
	    OUTFORM = BYTE
	    ZMAX = 255.
	    CUTOFF = 255.
	ELSEIF (FORMAT(1:4).EQ.'HALF') THEN
	    OUTFORM = HALF
	    ZMAX = 32767.
	    CUTOFF = 32767.
	ELSEIF (FORMAT(1:4).EQ.'FULL') THEN
	    OUTFORM = FULL
	    ZMAX = 2147483647.
	    CUTOFF = ZMAX
	ELSEIF (FORMAT(1:4).EQ.'REAL') THEN
	    OUTFORM = REAL
	    ZMAX = 1.0E30
	    CUTOFF = ZMAX
	ENDIF
C		Open the output image

	CALL XVUNIT (OUTUNIT,'OUT',1,STATUS,' ')

C		BYTE output for MAXZ <= 255
C		HALF > 255 <= 32767
C		FULL > 32767

	IF (ZMAX.LE.255) THEN
	    CALL XVOPEN (OUTUNIT,STATUS,'OP','WRITE','U_NL',NLINES,
     *	         'U_NS',NSAMPS,'U_FORMAT','REAL','O_FORMAT','BYTE',' ')
	    CALL XVMESSAGE(' ',' ')
	    CALL XVMESSAGE
     *           ('    The output data set is a BYTE image.',' ')
	    CALL XVMESSAGE(' ',' ')
	    GOTO 50
	ENDIF

	IF (ZMAX.LE.32767) THEN
	    CALL XVOPEN (OUTUNIT,STATUS,'OP','WRITE','U_NL',NLINES,
     *	         'U_NS',NSAMPS,'U_FORMAT','REAL','O_FORMAT','HALF',' ')
	    CALL XVMESSAGE(' ',' ')
	    CALL XVMESSAGE
     *           ('    The output data set is a HALFWORD image.',' ')
	    CALL XVMESSAGE(' ',' ')
	ELSE IF (ZMAX.EQ.2147483647.0) THEN
	    CALL XVOPEN (OUTUNIT,STATUS,'OP','WRITE','U_NL',NLINES,
     *	         'U_NS',NSAMPS,'U_FORMAT','REAL','O_FORMAT','FULL',' ')
	    CALL XVMESSAGE(' ',' ')
	    CALL XVMESSAGE
     *           ('    The output data set is a FULLWORD image.',' ')
	    CALL XVMESSAGE(' ',' ')
	ELSE
	    CALL XVOPEN (OUTUNIT,STATUS,'OP','WRITE','U_NL',NLINES,
     *	         'U_NS',NSAMPS,'U_FORMAT','REAL','O_FORMAT','REAL',' ')
	    CALL XVMESSAGE(' ',' ')
	    CALL XVMESSAGE
     *           ('    The output data set is a REAL image.',' ')
	    CALL XVMESSAGE(' ',' ')
	END IF

C		Set up for TRON if active

50	IF (TRON) THEN
	    TRPERC = TRACE
	    TRCOUNT = TRACE
	    COUNT = 0
	    TOTZ = 0
	    IF (TRMODE.EQ.1) THEN	! % complete
		TRCOUNT = ( (NLINES*NSAMPS) * (FLOAT(TRACE)/100.) )
	    ELSE			! N pixels computed
		TRPERC = ( (FLOAT(TRACE) / (NLINES*NSAMPS)) * 100.)
	    ENDIF
	    TRINC = TRPERC
	ENDIF

C ----------------------------------------------------------------
C --------------     M A I N    R O U T I N E     ----------------
C ----------------------------------------------------------------

	RDSQR = RADIUS ** 2
	RDXSQR = (RADIUS + RADEXP) ** 2
	TCLIP = 0
	ZMIN = +1.0E20
	ZMAX = -1.0E20

C		Sort the control points in the line direction

	DO I = 1, NPTS
	    KEY (I) = I
	END DO
        CALL MVE(7,NPTS,CPL,CPL2,1,1)
        CALL MVE(7,NPTS,KEY,KEY2,1,1)
	CALL INDSRTR(CPL,IP,NPTS)
        DO K = 1, NPTS
           CPL(K) = CPL2(IP(K))
           KEY(K) = KEY2(IP(K))
        END DO

C		Set up the upper and lower boundaries of a search swath

	BTM = 1
	LOW = 1
	HIGH = 2
	TOP = 2
	LOWVAL = 1. - RADIUS
	BTMVAL = LOWVAL - RADEXP
	HIGHVAL = 1. + RADIUS
	TOPVAL = HIGHVAL + RADEXP

	CALL NEWPNTRS(CPL,BTM,LOW,HIGH,TOP,
     *                BTMVAL,LOWVAL,HIGHVAL,TOPVAL,NPTS)

C			----------  BTM         -----
C						  ^
C						  |
C						RADEXP (DEFAULT - EXPANDS
C						  |     IF NECESSARY)
C						  V
C			----------  LOW         -----
C						  ^
C						  |
C						  |
C						  |
C			 S W A T H	      RADIUS * 2
C						  |
C						  |
C						  |
C						  V
C			----------  HIGH        -----
C						  ^
C						  |
C						RADEXP (DEFAULT - EXPANDS
C						  |     IF NECESSARY)
C						  V
C			----------  TOP         -----

C			Save pointers for restore later

		OBTM = BTM
		OLOW = LOW
		OHIGH = HIGH
		OTOP = TOP

C		Loop through the grid points (pixels)
C		one band (line) at a time ...
C		(from upper left to lower right)

	DO IL = 1, NLINES
	   DO IS = 1, NSAMPS


C			GET the line and sample value
C			for the current pixel

		LS(1) = FLOAT(IS)
		LS(2) = FLOAT(IL)
		NNH = 0
		IMAX = 1

C			Build up a list of squared distances and elevations.

		DO I = LOW, HIGH
		    CALL GETDIST(I,NMAX,LS,NNH,IMAX,KEY,CPL,CPS,CPZ,
     *				NHDIST,NHELEV)
		END DO

		NBUFF = (HIGH - LOW) + 1	! # Control pts in swath
		EXPAND = .FALSE.		! (Not # within range !)

C			Check to make sure that enough points were found
C			in the swath. If not, increase and check above
C			and below the strip for more points.

510		IF ((NNH.LT.NMAX).OR.(EXPAND)) THEN
		  IF (BTM.EQ.LOW) GOTO 560  ! Don't go below the bottom

		  DO I = BTM, (LOW - 1)
		      CALL GETDIST(I,NMAX,LS,NNH,IMAX,KEY,
     *				CPL,CPS,CPZ,NHDIST,NHELEV)
		  END DO

		  IF (EXPAND) THEN
		    IF(BTM.NE.OBTM) NBUFF = NBUFF + 1
		  ELSE
		    NBUFF = NBUFF + (LOW - BTM)
		  END IF

560		  IF (HIGH.EQ.TOP) GOTO 580 ! Don't go over the top

		  DO I = (HIGH + 1), TOP
		      CALL GETDIST(I,NMAX,LS,NNH,IMAX,KEY,
     *				CPL,CPS,CPZ,NHDIST,NHELEV)
		  END DO

		  IF (EXPAND) THEN
		    IF(TOP.NE.OTOP) NBUFF = NBUFF + 1
		  ELSE
		    NBUFF = NBUFF + (TOP - HIGH)
		  END IF
		ENDIF


C		Find all control points within RADIUS
C		and those within RADEXP

580		INSIDE = 0	! Within radius
		FRINGE = 0	! Within radius expansion
		JK = 0
		KJ = 0

		DO I = 1, NNH
		    IF (NHDIST(I).LE.RDSQR) THEN
			INSIDE = INSIDE + 1
			JK = JK + 1
			INLIST(JK)=I
		    ELSE
			IF(NHDIST(I).LE.RDXSQR) THEN
			    FRINGE = FRINGE + 1
			    KJ = KJ + 1
			    OUTLIST(KJ)=I
			ENDIF
		    END IF
		END DO

C			If we still don't have enough points
C			expand the window into the control
C			points and try again.

		NUMVALID = INSIDE + FRINGE

		IF (((NUMVALID.LT.NMAX).AND.(NBUFF.LT.NPTS)) .OR.
     *		    ((NHDIST(IMAX).LT.RDXSQR).AND.(RADIAL))) THEN
C			! Check for no more points
		    IF((BTM.EQ.1).AND.(TOP.EQ.NPTS)) GOTO 600
		    EXPAND = .FALSE.
		    IF(BTM.GT.1)THEN
			LOW=BTM
			IF(LOW.GT.1) THEN
			    BTM=LOW - 1
			    EXPAND=.TRUE.
			END IF
		    END IF
		    IF(TOP.LT.NPTS)THEN
			HIGH=TOP
			IF(HIGH.LT.NPTS) THEN
			    TOP=HIGH+1
			    EXPAND=.TRUE.
			END IF
		    END IF
		    IF(EXPAND)GOTO 510
		END IF

C -----------------------------------------------------------------------

C		    Sort by distance

600		IF (NUMVALID.LT.NMAX) THEN
		    DO I = 1, NNH
			DKEY(I) = I
		    END DO
                    CALL MVE(7,NNH,NHDIST,NHDIST2,1,1)
                    CALL MVE(7,NNH,DKEY,DKEY2,1,1)
		    CALL INDSRTR(NHDIST,IP,NNH)
                    DO K = 1, NNH
                       NHDIST(K) = NHDIST2(IP(K))
                       DKEY(K) = DKEY2(IP(K))
                    END DO
		    DO I = 1, NNH		    ! Stuff 1 through NNH back
			DBUFF(I) = NHDIST(I)	    ! into DKEY because DBUFF
			ZBUFF(I) = NHELEV(DKEY(I))  ! is already sorted. NOTE:
		    END DO			    ! the ELSE sorts DBUFF and
		    DO I = 1, NNH		    ! later refers to DKEY ...
			DKEY(I) = I		    ! so we have to fill DKEY
		    END DO			    ! the ELSE sorts DBUFF and
		    NUMVALID = NNH		    ! compatiblity later ...
		ELSE
		    IF (JK.GT.0) THEN
			DO I=1, JK
			    DBUFF(I) = NHDIST(INLIST(I))
			    ZBUFF(I) = NHELEV(INLIST(I))
			    DKEY(I) = I
			END DO
			NUMVALID = JK
		    END IF
		    IF((JK.LT.NMAX) .AND. (KJ.NE.0)) THEN  ! We need to include
			DO I = 1, KJ                       ! RADEXP points
			    DBUFF(I+JK) = NHDIST(OUTLIST(I))
			    ZBUFF(I+JK) = NHELEV(OUTLIST(I))
			    DKEY(I+JK) = I + JK
			END DO
			NUMVALID = JK + KJ
		    END IF
                    CALL MVE(7,NUMVALID,DBUFF,DBUFF2,1,1)
                    CALL MVE(7,NUMVALID,DKEY,DKEY2,1,1)
		    CALL INDSRTR(DBUFF,IP,NUMVALID)
                    DO K = 1, NUMVALID
                       DBUFF(K) = DBUFF2(IP(K))
                       DKEY(K) = DKEY2(IP(K))
                    END DO
		END IF

C		Establish upper pointer depending on whether we
C		are using nearest neighbor or a search criteria

		IF ((.NOT.RADIAL).AND.(NUMVALID.GT.NMAX)) NUMVALID=NMAX

C		Also check for enough points within RADIUS excluding
C		those points found within RADEXP(s). (Remember that
C		if RADIAL is true then NMAX = NMAX * 4.)

		IF ((RADIAL).AND.(INSIDE.GE.NMAX)) NUMVALID = INSIDE

C			We are finally ready to interpolate the Z

		CALL GETZ(NUMVALID,DBUFF,ZBUFF,DKEY,EXPONENT,Z)

		IF (Z.LT.ZMIN) ZMIN=Z
		IF (Z.GT.ZMAX) ZMAX=Z

C			Prevent Z roundoffs

		IF (OUTFORM.NE.REAL) Z = ANINT(Z)

C			Check to see if Z will fit in the requested
C			output image data format. Clip if need be.	

		IF (OUTFORM.NE.DEFAULT) THEN
		    IF (Z.GT.CUTOFF) THEN
			Z = CUTOFF
			TCLIP = TCLIP + 1
		    END IF			! add lower cuttoff here later 
		END IF

C			Stuff the Z value into the next pixel in the line

		BAND(IS)=Z
C			TRON fights for the user - tell my user what's up

		IF (TRON) THEN
		    COUNT = COUNT + 1
		    IF (TOTZ.EQ.0) THEN
			CALL XVMESSAGE(' ',' ')
			CALL XVMESSAGE
     *                   ('      ++++++++++  TRace ON  ++++++++++',' ')
			CALL XVMESSAGE(' ',' ')
		    ENDIF
		    TOTZ = TOTZ + 1
		    IF (COUNT.GE.TRCOUNT) THEN
			COUNT = 0
			WRITE (STRING,'(A,I6,A,I6,A,I7,F6.1,A)') 
     *                        'Line: ',IL,' Sample: ',
     *			      IS,' Pixel #',TOTZ, TRPERC, '% done'
			CALL XVMESSAGE(STRING,' ')
			TRPERC = TRPERC + TRINC
		    ENDIF
		ENDIF
	    END DO

C		Write out a line at a time

	    CALL XVWRIT (OUTUNIT,BAND,STATUS,' ')

C		Step to the next line ... add 1 to everybody

	    BTMVAL = BTMVAL + 1.0
	    LOWVAL = LOWVAL + 1.0
	    HIGHVAL = HIGHVAL + 1.0
	    TOPVAL = TOPVAL + 1.0

C		    Restore pointers

	    BTM = OBTM
	    LOW = OLOW
	    HIGH = OHIGH
	    TOP = OTOP

C		Recompute the range strip for searching

	    CALL NEWPNTRS (CPL,BTM,LOW,HIGH,TOP,BTMVAL,
     *		LOWVAL,HIGHVAL,TOPVAL,NPTS)

	    OBTM = BTM
	    OLOW = LOW
	    OHIGH = HIGH
	    OTOP = TOP

	END DO

C -----------------------------------------------------------------
C ------------   E N D    O F    M A I N   ------------------------
C -----------------------------------------------------------------

C		Report all vital program statistics

	CALL XVMESSAGE(' ',' ')
	CALL XVMESSAGE
     *       ('  Interpolated surface generated successfully.',' ')
	CALL XVMESSAGE(' ',' ')

	IF (TCLIP.GT.0) THEN
	    WRITE (STRING,'(A,F8.0,A,I10)')
     *         'Total number of pixels over ',CUTOFF, ': ', TCLIP
	    CALL XVMESSAGE(STRING,' ')
	END IF

C		Chuck info into the history label

	WRITE (STRING,'(A,F7.2,A,F7.2,A,I5)') 'MINZ:', ZMIN, ' MAXZ:',
     *        ZMAX, '   # Control points:', NPTS
	CALL XLADD(OUTUNIT,'HISTORY','COMMENTS',STRING,STATUS,
     *	      'FORMAT','STRING',' ')
	CALL XVMESSAGE(STRING,' ')
	EXPONENT = EXPONENT * 2.0
	WRITE (STRING,'(A,I4,A,F5.1,A,F5.1,A,F4.1)') 'NUMN:', NMAX, 
     *	' RADIUS:', RADIUS, ' RADEXP:', RADEXP, ' EXPONENT:', EXPONENT
	CALL XLADD(OUTUNIT,'HISTORY','PARAMETER',STRING,
     *	STATUS,'FORMAT','STRING',' ')
	CALL XVMESSAGE(STRING,' ')
	CALL XVMESSAGE(' ',' ')

		GOTO 9900

C	********* E R R O R   T R A P S **********

C		Too may control points !!!

9100	CALL XVMESSAGE('Too many control points !!!',' ')
	CALL XVMESSAGE('Maximum # is 3000',' ')
		GOTO 9800

C		Invalid column number given

9200	CALL XVMESSAGE
     *        ('An invalid column number has been requested',' ')
		GOTO 9800

C		Not enough control points given

9300	CALL XVMESSAGE('Not enough control points given !',' ')
     	WRITE (STRING,'(A,I6,A,I5,A)') 'Must be >= ', NUMN, ' Only ', 
     *			NPTS, ' control points found.'
	CALL XVMESSAGE(STRING,' ')
		GOTO 9800

C		Something terrible has happened ... STOP !!!

9800	CALL ABEND

C		Close things up and go home

9900	CALL IBIS_FILE_CLOSE (IBIS,' ',STATUS)
        IF (STATUS.NE.1) CALL IBIS_SIGNAL_U(RUNIT,STATUS,1)
	CALL XVCLOSE (OUTUNIT,STATUS,' ')

        RETURN
	END

C------------------  G E T D I S T  ----------------------------

	SUBROUTINE GETDIST(I,NMAX,LS,NNH,IMAX,KEY,CPL,CPS,CPZ,
     *                     NHDIST,NHELEV)
	IMPLICIT NONE
	INTEGER NNH, IMAX, INH, KEY(*), NMAX, I
	REAL DIST, CPL(*), CPS(*), CPZ(*), LS(2), NHDIST(*), NHELEV(*) 

	DIST = (CPS(KEY(I))-LS(1))**2 + (CPL(I)-LS(2))**2
	IF(NNH.LT.NMAX) THEN
	    NNH = NNH + 1
	    NHDIST(NNH) = DIST
	    NHELEV(NNH) = CPZ(KEY(I))
	    IF(DIST.GT.NHDIST(IMAX)) IMAX = NNH
	ELSE         !The following line has been changed to use 1.00001 to
	    IF(1.00001*DIST .LT. NHDIST(IMAX)) THEN    !breaks ties
		NHDIST(IMAX) = DIST
 		NHELEV(IMAX) = CPZ(KEY(I))
		DO INH = 1, NNH  ! Find the furthest point away
		    IF(NHDIST(INH).GT.NHDIST(IMAX)) IMAX = INH
		END DO
	    END IF
	END IF
	RETURN
	END
C------------------ G E T Z ----------------------

	SUBROUTINE GETZ (NUMVALID,DBUFF,ZBUFF,DKEY,EXPONENT,Z)

	IMPLICIT NONE
	INTEGER NUMVALID, DKEY(*), I, IATCP
	REAL DBUFF(*), ZBUFF(*), EXPONENT, Z
	REAL SUMNUM, SUMDEN, WTDDIST

C		Compute the sums needed for interpolation

	SUMNUM = 0.0
	SUMDEN = 0.0
	IATCP = 0

	DO 100 I = 1, NUMVALID

C		If the current grid point is at one or more
C		control points, then average the control point's
C		Z values

	    IF (DBUFF(I) .LE. 1.0E-12) GO TO 75
	    WTDDIST = DBUFF(I)**EXPONENT
	    IF (WTDDIST .GT. 1.0E-12) GO TO 80
75	    IF (IATCP.EQ.0) THEN
		IATCP = 1
		SUMNUM = 0.0
		SUMDEN = 0.0
	    END IF
	    SUMNUM = SUMNUM + ZBUFF(DKEY(I))
	    SUMDEN = SUMDEN + 1.0
		GO TO 100
80	    IF(IATCP.NE.0) GO TO 100
	    SUMNUM = SUMNUM + ZBUFF(DKEY(I))/WTDDIST
	    SUMDEN = SUMDEN + 1.0/WTDDIST
100	CONTINUE

C	    Calculate Z the value

	Z = SUMNUM / SUMDEN

	RETURN
	END
C----------------  N E W P N T R S  -----------------------

	SUBROUTINE NEWPNTRS (CPL,BTM,LOW,HIGH,TOP,BTMVAL,
     *	LOWVAL,HIGHVAL,TOPVAL,NPTS)

	IMPLICIT NONE
	INTEGER BTM, LOW, HIGH, TOP, J, NPTS
	REAL CPL(*), BTMVAL, LOWVAL, HIGHVAL, TOPVAL

100	IF (CPL(LOW).LT.LOWVAL) THEN
	    LOW = LOW + 1
	ELSE
	    IF (LOW.GT.1)THEN
		LOW = LOW - 1
		GOTO 100
	    END IF
	END IF
	BTM = LOW

	DO J = (LOW - 1), 1, -1
	    IF (CPL(J).LT.BTMVAL) THEN
		BTM = J + 1
		GOTO 300
	    END IF
	END DO

300	IF (CPL(HIGH).GT.HIGHVAL) THEN
	    HIGH = HIGH - 1
	ELSE
	    IF (HIGH.LT.NPTS)THEN
		HIGH = HIGH + 1
		GOTO 300
	    END IF
	END IF
	TOP = HIGH
	DO J = (HIGH + 1), NPTS
	    IF (CPL(J).GT.TOPVAL) THEN
		TOP = J - 1
		GOTO 5000
	    END IF
	END DO

5000	RETURN
	END

C============ E N D === O F === S O U R C E === C O D E ===========

