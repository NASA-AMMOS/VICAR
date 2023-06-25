#ifndef MIPS_MPF_APX_LBLS_INCLUDED
#define MIPS_MPF_APX_LBLS_INCLUDED 1

/**  Copyright (c) 1995, California Institute of Technology		**/
/**  U. S. Government sponsorship under NASA contract is acknowledged	**/

#include "mpf_gen_labels.h"

/***  Constant Label Values  ***/
/* Array sizes */
#define  LBL_RVR_POSITION	 2
#define  LBL_RVR_TEMPERATURES	13
#define  LBL_APX_TEMPERATURES	20
#define  LBL_RVR_ACCELEROMETER	 2
/* Label Values */
#define  LBL_APX_DATA_SET_ID	"MPFR-M-APXS-2-EDR-V1.0"
#define  LBL_APX_DATA_SET_NAME	"MPF ROVER MARS ALPHA PROTON X-RAY SPECTROMETER 2 EDR V1.0"
#define  LBL_APX_INSTRUMENT	"Alpha Proton X-Ray Spectrometer"
#define  LBL_APX_INSTRUMENT_ID	"APXS"
#define  LBL_APX_INST_HOST	"MICROROVER FLIGHT EXPERIMENT"
#define  LBL_APX_INST_HOST_ALIAS	"SOJOURNER"
#define  LBL_APX_INST_HOST_ID	"MPFR"
#define  LBL_APX_PRODUCER	LBL_PRODUCER
#define  LBL_APX_PRODUCER_ID	LBL_PRODUCER_ID
#define  LBL_APX_PRODUCER_INST	LBL_PRODUCER_INST
#define  LBL_APX_RECORD_TYPE	"FIXED_LENGTH"
#define  LBL_APX_SAMPLE_TYPE	"LSB_UNSIGNED_INTEGER"
#define  LBL_APX_SPACECRAFT	"PATHFINDER ROVER"

/***  APX COMMANDS  ***/
typedef struct
	{
	int		CommandSequenceNumber;
	} ApxCmndProperty_typ;

/***  OBSERVATION  ***/
typedef struct
	{
	int		AccumulationCount;
	char		AlphaSamplingDuration[LBL_TIME_LTH];
	float		AmbientTemperature[LBL_APX_TEMPERATURES];
	int		APID;
	int		ApxsCommunicationErrorCount;
	float		ApxsMechanismAngle;
	char		BackgroundSamplingDuration[LBL_TIME_LTH];
	int		CommandSequenceNumber;
	int		ContactSensorState;
	int		ConverterCurrent;
	int		ConverterVoltage;
	int		InstHostTemperature[LBL_RVR_TEMPERATURES];
	float		InstrumentTemperature[LBL_APX_TEMPERATURES];
	float		LinearAccelerometer[LBL_RVR_ACCELEROMETER];
	int		Packet_SCLK;
	int		PlanetDayNumber;
	char		ProtonSamplingDuration[LBL_TIME_LTH];
	int		RoverHeading;
	float		RoverPosition[LBL_RVR_POSITION];
	int		SCLK_StartCount;
	int		SCLK_StopCount;
	int		StartErrorState;
	char		StartTime[LBL_TIME_LTH];
	int		StopErrorState;
	char		StopTime[LBL_TIME_LTH];
	char		TargetName[LBL_NAME_LTH];
	char		XraySamplingDuration[LBL_TIME_LTH];
	} ApxObsProperty_typ;

/***  PDS (other ?)  ***/
typedef struct
	{
	char		ApplicationPacketName[LBL_NAME_LTH];
	char		CommandDescription[LBL_DESC_LTH];
	char		DataSetId[LBL_NAME_LTH];
	char		DataSetName[LBL_LONG_NAME_LTH];
	char		InterchangeFormat[LBL_TYPE_LTH];
	char		InstrumentHostAlias[LBL_NAME_LTH];
	char		InstrumentHostId[LBL_TYPE_LTH];
	char		InstrumentHostName[LBL_NAME_LTH];
	char		PdsVersionId[LBL_TYPE_LTH];
	char		ProcessingHistoryText[LBL_DESC_LTH];
	char		ProducerFullName[LBL_LONG_NAME_LTH];
	char		ProducerInstitutionName[LBL_DESC_LTH];
	int		SampleBits;
	char		SampleBitMask[LBL_DESC_LTH];
	char		SampleType[LBL_NAME_LTH];
	} ApxPdsProperty_typ;

/***  Function Prototypes  ***/
int	ApxCommandProperty( int, int, LblCntrl_typ *, ApxCmndProperty_typ * );
int	ApxObservationProperty( int, int, LblCntrl_typ *, ApxObsProperty_typ * );
int	ApxPdsProperty( int, int, LblCntrl_typ *, ApxPdsProperty_typ * );

#endif
