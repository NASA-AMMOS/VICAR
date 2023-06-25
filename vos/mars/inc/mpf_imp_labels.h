#ifndef MIPS_MPF_IMP_LBLS_INCLUDED
#define MIPS_MPF_IMP_LBLS_INCLUDED 1

/**  Copyright (c) 1995, California Institute of Technology		**/
/**  U. S. Government sponsorship under NASA contract is acknowledged	**/

#include "mpf_gen_labels.h"

#define  LBL_JPEG_TBL_SIZE	64
/***  Constant Label Values  ***/
#define  LBL_IMP_DATA_SET_ID	"MPFL-M-IMP-2-EDR-V1.0"	/* prefix */
#define  LBL_IMP_DATA_SET_NAME	"MPF LANDER MARS IMAGER FOR MARS PATHFINDER 2 EDR V1.0"
#define  LBL_IMP_COMP_LOSSY	"LOSSY"
#define  LBL_IMP_COMP_LOSSLESS	"LOSSLESS"
#define  LBL_IMP_COORD_SYSTEM	LBL_COORD_SYSTEM
#define  LBL_IMP_DETECTOR_SIZE	23.0 /* microns */
#define  LBL_IMP_ENCODING_RICE	"Rice Adaptive Variable-length Coding (RICE)"
#define  LBL_IMP_ENCODING_JPEG	"JPEG Discrete Cosine Transform (DCT); "
#define  LBL_IMP_ENCODING_NONE	"NONE"
#define  LBL_IMP_INSTRUMENT_ID	"IMP"
#define  LBL_IMP_INSTRUMENT	"Imager for Mars Pathfinder"
#define  LBL_IMP_MISSION	LBL_MISSION_NAME
#define  LBL_IMP_ORIENTATION	"zero azimuth is up"
#define  LBL_IMP_PRODUCER	LBL_PRODUCER
#define  LBL_IMP_PRODUCER_ID	LBL_PRODUCER_ID
#define  LBL_IMP_PRODUCER_INST	LBL_PRODUCER_INST
#define  LBL_IMP_QUANTIZER	"TABULAR"
#define  LBL_IMP_RECORD_TYPE	"FIXED_LENGTH"
#define  LBL_IMP_SAMPLE_TYPE	"MSB_UNSIGNED_INTEGER"
#define  LBL_IMP_SPACECRAFT	"MARS PATHFINDER LANDER"
#define  LBL_IMP_TLM_METHOD	"TELEMETRY"
#define  LBL_IMP_NAV_METHOD	"MPFNAV-MIPL"
#define  LBL_IMP_SURF_METHOD	"L_FRAME-QUATERNION"

typedef	struct
	{
	float		Azimuth;
	float		AzimuthFOV;
	char		AzimuthMethod[LBL_MTHD_LTH];
	int		AzimuthMotorClicks;
	double		CameraOrientationQuaternion[LBL_QUATERNION_ARRAY];
	float		Elevation;
	float		ElevationFOV;
	char		ElevationMethod[LBL_MTHD_LTH];
	int		ElevationMotorClicks;
	float		FocalCenterC[LBL_VECTOR_ARRAY];
	float		HorizontalImagePlaneH[LBL_VECTOR_ARRAY];
	double		LanderSurfaceQuaternion[LBL_QUATERNION_ARRAY];
	char		MllMfxOffsetMethod[LBL_MTHD_LTH];
	float		MllMfxOffsetVector[LBL_VECTOR_ARRAY];
	float		PointingDirectionA[LBL_VECTOR_ARRAY];
	float		SurfaceBasedAzimuth;
	float		SurfaceBasedElevation;
	char		SurfaceBasedMethod[LBL_MTHD_LTH];
	float		VerticalImagePlaneV[LBL_VECTOR_ARRAY];
	} ImpCameraProperty_typ;

typedef	struct
	{
	int		AutoExposureDataCut;
	float		AutoExposurePixelFraction;
	char		BadPixelReplacement[LBL_LOGICAL_LTH];
	char		CommandName[LBL_NAME_LTH];
	char		DarkCurrentCorrection[LBL_LOGICAL_LTH];
	char		Download[LBL_NAME_LTH];
	int		ExposureCount;
	char		FlatFieldCorrection[LBL_LOGICAL_LTH];
	char		ShutterEffectCorrection[LBL_LOGICAL_LTH];
	char		SqrtCompression[LBL_LOGICAL_LTH];
	} ImpCmndProperty_typ;

typedef	struct
	{
	int		InstCmprsBlkSize[2];
	int		InstCmprsBlocks;
	int		InstCmprsMode;
	char		InstCmprsName[LBL_NAME_LTH];
	int		InstCmprsParam;
	char		InstCmprsQuantzTblId[LBL_NAME_LTH];
	int		InstCmprsQuality;
	float		InstCmprsRate;
	float		InstCmprsRatio;
	int		InstCmprsSyncBlks;
	int		PixelAveragingHeight;
	int		PixelAveragingWidth;
	int		RiceOptionValue;
	int		RiceStartOption;
	int		SqrtMaxPixel;
	int		SqrtMinPixel;
	} ImpCompProperty_typ;


typedef	struct
	{
	float		AzimuthOfSampleOne;
	char		MapProjectionType[LBL_TYPE_LTH];
	float		MosaicReferenceAzimuth;
	float		MosaicReferenceElevation;
	float		MosaicReferenceLine;
	float		MosaicReferenceSample;
	float		MosaicResolution;
	float		NadirLine;
	float		NadirSample;
	char		Orientation[LBL_DESC_LTH];
	float		RadialMosaicResolution;
	int		ZeroElevationImageLine;
	float		MosaicAzimuthResolution;
	float		StartAzimuth;
	float		StopAzimuth;
	float		MinimumElevation;
	float		MaximumElevation;
	float		RadianceScalingFactor;
	float		MinimumX;
	float		MaximumX;
	float		MinimumY;
	float		MaximumY;
	} ImpMosaicProperty_typ;

typedef	struct
	{
	int		APID;
	int		ErrorPixels;
	float		ExposureDuration;
	char		ExposureType[LBL_TYPE_LTH];
	char		FilterName[LBL_NAME_LTH];
	int		FilterNumber;
	int		FirstLine;
	int		FirstLineSample;
	char		FrameId[LBL_NAME_LTH];
	int		ImageId;
	char		ImageObservationType[LBL_TYPE_LTH];
	char		ImageTime[LBL_TIME_LTH];
	char		InstDeploymentState[LBL_NAME_LTH];
	float		InstTemperature[2];
	int		InstTemperatureCount[2];
	char		LocalTime[LBL_TIME_LTH];
	int		Maximum;
	float		Mean;
	int		Median;
	int		Minimum;
	char		ObservationName[LBL_NAME_LTH];
	int		PlanetDayNumber;
	int		SCLK_StartCount;
	float		StandardDeviation;
	char		TargetName[LBL_NAME_LTH];
	} ImpObsProperty_typ;

typedef struct
	{
	char		ApplicationPacketName[LBL_DESC_LTH];
	int		Bands;
	unsigned int	Checksum;
	char		CommandDesc[LBL_DESC_LTH];
	char		DarkCurrentDownloadFlag[LBL_LOGICAL_LTH];
	char		DataSetId[LBL_NAME_LTH];
	char		DataSetName[LBL_LONG_NAME_LTH];
	float		DetectorPixelHeight;
	float		DetectorPixelWidth;
	char		InstCmprsDesc[LBL_DESC_LTH];
	char		InstCmprsQuantzType[LBL_TYPE_LTH];
	char		InterchangeFormat[LBL_DESC_LTH];
	int		Lines;
	int		LineSamples;
	char		PdsVersionId[LBL_TYPE_LTH];
	char		ProcessingHistoryText[LBL_DESC_LTH];
	char		ProducerFullName[LBL_LONG_NAME_LTH];
	char		ProducerInstitutionName[LBL_DESC_LTH];
	int		SampleBits;
	char		SampleBitMask[LBL_DESC_LTH];
	char		SampleType[LBL_NAME_LTH];
/***  probably will have  ***/
	float		BodyCenterDistance;
	char		CoordinateSystemName[LBL_NAME_LTH];
	float		EmissionAngle;
	float		IncidenceAngle;
	float		PhaseAngle;
	float		SolarAzimuth;
	float		SolarElevation;
	float		SubSpacecraftLatitude;
	float		SubSpacecraftLongitude;
	float		SurfaceNormalAzimuth;
	float		SurfaceNormalElevation;
/***  Cool to have  ***/
	float		InertialAerocentricMatrix[LBL_TRANSFORM_ARRAY];
	int		InstCmprsQuantzTbl[LBL_JPEG_TBL_SIZE];
	float		MfxMbfMatrix[LBL_TRANSFORM_ARRAY];
	} ImpPdsProperty_typ;

/***  Function Prototypes  ***/
int	ImpCameraModelProperty( int, int, LblCntrl_typ *, ImpCameraProperty_typ *);
int	ImpCommandProperty( int, int, LblCntrl_typ *, ImpCmndProperty_typ *);
int	ImpCompressionProperty( int, int, LblCntrl_typ *, ImpCompProperty_typ *);
int	ImpMosaicProperty( int, int, LblCntrl_typ *, ImpMosaicProperty_typ *);
int	ImpObservationProperty( int, int, LblCntrl_typ *, ImpObsProperty_typ *);
int	ImpPdsProperty( int, int, LblCntrl_typ *, ImpPdsProperty_typ * );

static char	*ImpFilterName[12] =
		{"L440_R440", "L450_R670", "L885_R947",  "L925_R935",
		 "L935_R990", "L670_R670", "L800_R750",  "L860_R-DIOPTER",
		 "L900_R600", "L930_R530", "L1000_R480", "L965_R965"};
static char	*ImpObsType[16] =
		{"REGULAR",	"DARK_CURRENT",	"FLAT_FIELD",
		 "HISTOGRAM",	"SUMMATION",	"DARK_STRIP",
		 "NULL_STRIP",	"", "", "", "", "", "", "", "", ""};
static	char	*JpegType[10] =
		{"raw",			"Huffman/Quality",
		 "Huffman/Ratio",	"Arithmetic/Quality",
		 "Arithmetic/Ratio",	"Huffman/Quality/LCT",
		 "Huffman/Ratio/LCT",	"Arithmetic/Quality/LCT",
		 "Arithmetic/Ratio/LCT","rice"};

#endif
