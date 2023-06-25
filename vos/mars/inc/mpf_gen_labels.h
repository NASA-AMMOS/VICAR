#ifndef MIPS_MPF_GEN_LBLS_INCLUDED
#define MIPS_MPF_GEN_LBLS_INCLUDED 1

/**  Copyright (c) 1995, California Institute of Technology		**/
/**  U. S. Government sponsorship under NASA contract is acknowledged	**/

#define  CAMERA_LBL_NAME	"CAMERA_MODEL"
#define  DECOMP_LBL_NAME	"DECOMPRESSION"
#define  OBS_LBL_NAME		"OBSERVATION"
#define  MOS_LBL_NAME		"MOSAIC"
#define  TELEM_LBL_NAME		"TELEMPROC"
#define  PDS_LBL_NAME		"PDS"

#define  LBL_UNDEFINED		  0
#define  LBL_IMP		  1
#define  LBL_RVR		  2
#define  LBL_APXS		  3
#define  LBL_MET		  4

#ifndef LBL_READ		/* prevent conflicts with new label API */
#define  LBL_READ		  1
#endif
#ifndef LBL_WRITE		/* prevent conflicts with new label API */
#define  LBL_WRITE		  0
#endif

/***  Constant Label Values  ***/
/* Generic string lengths */
#define  LBL_LOGICAL_LTH	  8
#define  LBL_MTHD_LTH		 32
#ifndef MIPS_LBL_GEN_API_INCLUDED   /* prevent conflicts with new label API */
#define  LBL_TYPE_LTH		 32
#define  LBL_TIME_LTH		 32
#define  LBL_NAME_LTH		 64
#define  LBL_LONG_NAME_LTH	128
#define  LBL_DESC_LTH		256

/* Generic array sizes */
#define  LBL_VECTOR_ARRAY	  3
#define  LBL_POSITION_ARRAY	  3
#define  LBL_QUATERNION_ARRAY	  4
#define  LBL_TRANSFORM_ARRAY	 12
#endif
#define  LBL_COORD_SYSTEM	"PLANETOCENTRIC"
#define  LBL_MISSION_NAME	"MARS PATHFINDER"
#define  LBL_PRODUCER		"Allan J. Runkle"
#define  LBL_PRODUCER_ID	"MIPL of JPL"
#define  LBL_PRODUCER_INST	"Multimission Image Processing Laboratory, Jet Propulsion Lab"
#define  LBL_HISTORY		"CODMAC Level 1 to Level 2 conversion via JPL/MIPL MPFTELEMPROC"
#define  LBL_INTERCHANGE_FMT	"BINARY"
#define  LBL_PDS_VERSION_ID	"PDS3"

typedef struct
	{ /***	Internal Processing Controls - Not Label Items	***/
	int		ErrorStatus;		/* Last one processed */
	short		ErrorCount;
	short		ErrorLocation;		/* Last one processed */
	int		Instrument;
	int		ProceedOnError;
	} LblCntrl_typ;

typedef struct
	{ /***  General label processing table  ***/
	char		*Key;
	char		*Format;
	int		Elements;
	void		*Value;
	} MpfLabelTable_typ;

typedef struct
	{
	MpfLabelTable_typ	*Fields;
	char			*Type;
	char			*Name;
	char			*PropertyName;
	} MpfLabelProcess_typ;

typedef	struct
	{
	char		EarthReceivedStartTime[LBL_TIME_LTH];
	char		EarthReceivedStopTime[LBL_TIME_LTH];
	int		ExpectedPackets;
	char		InstrumentId[LBL_TYPE_LTH];
	char		InstrumentName[LBL_NAME_LTH];
	char		MissionName[LBL_NAME_LTH];
	char		ProducerId[LBL_NAME_LTH];
	char		ProductCreationTime[LBL_TIME_LTH];
	char		ProductId[LBL_DESC_LTH];
	int		ReceivedPackets;
	char		SoftwareName[LBL_NAME_LTH];
	char		SoftwareVersionId[LBL_TYPE_LTH];
	char		SourceProductId[LBL_LONG_NAME_LTH];
	char		SpacecraftName[LBL_NAME_LTH];
	int		TlmCmdDiscrepancy;		/* 0-FALSE, 1-TRUE */
	} MpfTelemProperty_typ;

/***  Function Prototypes  ***/
char	*MpfLblErrorMsg();
int	MpfLblProcessor( int, int, LblCntrl_typ *, MpfLabelProcess_typ *);
int	MpfTelemProperty( int, int, LblCntrl_typ *, MpfTelemProperty_typ *);

#endif
