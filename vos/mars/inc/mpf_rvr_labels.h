#ifndef MIPS_MPF_RVR_LBLS_INCLUDED
#define MIPS_MPF_RVR_LBLS_INCLUDED 1

/**  Copyright (c) 1995, California Institute of Technology		**/
/**  U. S. Government sponsorship under NASA contract is acknowledged	**/

#include "mpf_gen_labels.h"

/***  Constant Label Values  ***/
/* Array sizes */
#define  LBL_RVR_POSITION	 2
#define  LBL_RVR_TEMPERATURES	13
#define  LBL_RVR_ACCELEROMETER	 2
#define  LBL_RVR_SOURCE_NUM	16
/* Label Values */
#define  LBL_RVR_COORD_SYSTEM	LBL_COORD_SYSTEM
#define  LBL_RVR_DATA_SET_ID	"MPFR-M-RVRCAM-2-EDR-V1.0"
#define  LBL_RVR_DATA_SET_NAME	"MARS PATHFINDER ROVER MARS ROVER CAMERA 2 EDR VERSION 1.0"
#define  LBL_RVR_INSTRUMENT	"ROVER CAMERA"
#define  LBL_RVR_INSTRUMENT_ID	"RVRC"	/* Elizabeth won't like this one */
#define  LBL_RVR_INST_HOST	"MICROROVER FLIGHT EXPERIMENT"
#define  LBL_RVR_INST_HOST_ALIAS	"SOJOURNER"
#define  LBL_RVR_INST_HOST_ID	"MPFR"
#define  LBL_RVR_PRODUCER	LBL_PRODUCER
#define  LBL_RVR_PRODUCER_ID	LBL_PRODUCER_ID
#define  LBL_RVR_PRODUCER_INST	LBL_PRODUCER_INST
#define  LBL_RVR_RECORD_TYPE	"FIXED_LENGTH"
#define  LBL_RVR_SPACECRAFT	"MARS PATHFINDER ROVER"
#define  LBL_RVR_COMPRESS_BTC	"BTC"
#define  LBL_RVR_BTC_NAME	"Block Truncation Coding (BTC)"
#define  LBL_RVR_COMPRESS_RAW	"RAW"
#define  LBL_RVR_RAW_NAME	"Raw-data; uncompressed"
#define  LBL_RVR_COMP_RATE	0.6125
#define  LBL_RVR_COMP_RATIO	4.9
#define  LBL_RVR_DETECT_H	13.6	/* microns */
#define  LBL_RVR_DETECT_W	11.6	/* microns */
#define  LBL_RVR_AZ_FOV		2.2	/* radians */
#define  LBL_RVR_EL_FOV		1.6	/* radians */
#define  LBL_RVR_PXL_FOV	0.120666

/***  CAMERA_MODEL  ***/
typedef struct
	{
	float		AzimuthFOV;
	float		ElevationFOV;
	float		FocalCenterC[LBL_VECTOR_ARRAY];
	float		HorizontalImagePlaneH[LBL_VECTOR_ARRAY];
	float		PointingDirectionA[LBL_VECTOR_ARRAY];
	float		VerticalImagePlaneV[LBL_VECTOR_ARRAY];
	} RvrCameraProperty_typ;

/***  BTC_DECOMPRESSION  ***/
typedef struct
	{
	int		InstCmprsBlkSize[2];
	int		InstCmprsBlocks;
	char		InstCmprsName[LBL_TYPE_LTH];
	float		InstCmprsRate;
	float		InstCmprsRatio;
	} RvrCompProperty_typ;

/***  ROVER COMMANDS  ***/
typedef struct
	{
	int		CommandSequenceNumber;
	int		Lines;
	int		LineSamples;
	char		CommandDescription[LBL_DESC_LTH];
	char		CommandName[LBL_NAME_LTH];
	} RvrCmndProperty_typ;

/***  OBSERVATION  ***/
typedef struct
	{
	int		APID;
	int		CommandSequenceNumber;
	float		ExposureDuration;
	char		ExposureType[LBL_TYPE_LTH];
	int		FirstLine;
	int		FirstLineSample;
	char		FrameId[LBL_TYPE_LTH];
	char		ImageId[LBL_TYPE_LTH];
	char		ImageTime[LBL_TIME_LTH];
	float		InstrumentTemperature;
	float		LinearAccelerometer[LBL_RVR_ACCELEROMETER];
	char		LocalTime[LBL_TIME_LTH];
	int		Maximum;
	float		Mean;
	int		Median;
	int		Minimum;
	char		ObservationName[LBL_NAME_LTH];
	int		PlanetDayNumber;
	int		RoverHeading;
	float		RoverPosition[LBL_RVR_POSITION];
	int		SCLK_StartCount;
	float		StandardDeviation;
	char		TargetName[LBL_NAME_LTH];
	} RvrObsProperty_typ;

/***  PDS (other ?)  ***/
typedef struct
	{
	char		ApplicationPacketName[LBL_NAME_LTH];
	char		BandSequence[LBL_TYPE_LTH];
	char		BandStorage[LBL_TYPE_LTH];
	int		Bands;
	unsigned int	Checksum;
	char		CommandDesc[LBL_DESC_LTH];
	char		DataSetId[LBL_NAME_LTH];
	char		DataSetName[LBL_LONG_NAME_LTH];
	float		DetectorPixelHeight;
	float		DetectorPixelWidth;
	char		InstCmprsDesc[LBL_DESC_LTH];
	char		InstrumentHostId[LBL_NAME_LTH];
	char		InstrumentHostName[LBL_NAME_LTH];
	char		InterchangeFormat[LBL_TYPE_LTH];
	int		Lines;
	int		LineSamples;
	char		PdsVersionId[LBL_TYPE_LTH];
	char		ProcessingHistoryText[LBL_DESC_LTH];
	char		ProducerFullName[LBL_LONG_NAME_LTH];
	char		ProducerInstitutionName[LBL_DESC_LTH];
	int		SampleBits;
	char		SampleBitMask[LBL_DESC_LTH];
	char		SampleType[LBL_NAME_LTH];
	float		SolarAzimuth;
	float		SolarElevation;
/***  Cool to have  ***/
	float		BodyCenterDistance;
	char		CoordinateSystemName[LBL_NAME_LTH];
	float		IncidenceAngle;
	float		InertialAreocentricMatrix[LBL_TRANSFORM_ARRAY];
	float		SubSpacecraftLatitude;
	float		SubSpacecraftLongitude;
	float		SurfaceNormalAzimuth;
	float		SurfaceNormalElevation;
	} RvrPdsProperty_typ;

/***  Rover mosaic  ***/
typedef	struct
	{
	int		SourceCommandSeqNum[LBL_RVR_SOURCE_NUM];
	int		SourceSclkStart;
	int		SourceSclkStop;
	float		OutputExposure;
	int		OutputFirstLine;
	int		OutputFirstLineSample;
	float		AvgDarkLevel[LBL_VECTOR_ARRAY];
	char		DarkCurrentCorrection[LBL_NAME_LTH];
	} RvrMosProperty_typ;

/***  Function Prototypes  ***/
int	RvrCameraModelProperty( int, int, LblCntrl_typ *, RvrCameraProperty_typ * );
int	RvrCommandProperty( int, int, LblCntrl_typ *, RvrCmndProperty_typ * );
int	RvrCompressionProperty( int, int, LblCntrl_typ *, RvrCompProperty_typ * );
int	RvrObservationProperty( int, int, LblCntrl_typ *, RvrObsProperty_typ * );
int	RvrPdsProperty( int, int, LblCntrl_typ *, RvrPdsProperty_typ * );
int	RvrMosaicProperty( int, int, LblCntrl_typ *, RvrMosProperty_typ * );

static char	*RvrCamera[4] = { "LEFT", "RIGHT", "REAR", "UNKNOWN"};
static char	*RvrInstName[4] = { "Left", "Right", "Rear", "Unknown"};
static char	*RvrApidMap = "********STL*************AN******************";
#endif
