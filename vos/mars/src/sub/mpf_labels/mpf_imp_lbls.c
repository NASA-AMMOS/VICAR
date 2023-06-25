/**  Copyright (c) 1995, California Institute of Technology		**/
/**  U. S. Government sponsorship under NASA contract is acknowledged	**/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "mpf_imp_labels.h"

#ifndef TRUE
#define TRUE 1
#endif

#define  COMMAND_LBL_NAME	"IMP_COMMANDS"

static	char	LogMsgBuf[256];
static	char	*LBL_ImpExposureType[] = {"AUTO","INCREMENTAL","MANUAL",
					"PRETIMED","NONE",0};
static	char	*LBL_ImpFrameID[] =	{"LEFT","RIGHT","BOTH","LEFT_HALF",0};
static	char	*LBL_ImpMapProjection[] = {"SIMPLE","CYLINDRICAL",
					"POLAR_SIMPLE_CYLINDRICAL",0};
static	ImpCameraProperty_typ	CM;
static	ImpCmndProperty_typ	CMD;
static	ImpCompProperty_typ	CMP;
static	ImpObsProperty_typ	OBS;
static	ImpMosaicProperty_typ	MOS;
static	ImpPdsProperty_typ	PDS;

/*** Camera Model Table  ***/
static MpfLabelTable_typ	CameraModel[] = {
	{"AZIMUTH_FOV",				"REAL",		1,
		&CM.AzimuthFOV},
	{"AZIMUTH_MOTOR_CLICKS",		"INT",		1,
		&CM.AzimuthMotorClicks},
	{"CAMERA_ORIENTATION_QUATERNION",	"DOUB",	LBL_QUATERNION_ARRAY,
		CM.CameraOrientationQuaternion},
	{"ELEVATION_FOV",			"REAL",		1,
		&CM.ElevationFOV},
	{"ELEVATION_MOTOR_CLICKS",		"INT",		1,
		&CM.ElevationMotorClicks},
	{"FOCAL_CENTER_VECTOR",			"REAL",	LBL_VECTOR_ARRAY,
		CM.FocalCenterC},
	{"HORIZONTAL_IMAGE_PLANE_VECTOR",	"REAL",	LBL_VECTOR_ARRAY,
		CM.HorizontalImagePlaneH},
	{"INSTRUMENT_AZIMUTH",			"REAL",		1,
		&CM.Azimuth},
	{"INSTRUMENT_AZIMUTH_METHOD",		"STRING",	1,
		CM.AzimuthMethod},
	{"INSTRUMENT_ELEVATION",		"REAL",		1,
		&CM.Elevation},
	{"INSTRUMENT_ELEVATION_METHOD",		"STRING",	1,
		CM.ElevationMethod},
	{"LANDER_SURFACE_QUATERNION",		"DOUB",	LBL_QUATERNION_ARRAY,
		CM.LanderSurfaceQuaternion},
	{"MLL_MFX_OFFSET_METHOD",		"STRING",	1,
		CM.MllMfxOffsetMethod},
	{"MLL_MFX_OFFSET_VECTOR",		"REAL",	LBL_VECTOR_ARRAY,
		CM.MllMfxOffsetVector},
	{"POINTING_DIRECTION_VECTOR",		"REAL",	LBL_VECTOR_ARRAY,
		CM.PointingDirectionA},
	{"SURFACE_BASED_INST_AZIMUTH",		"REAL",		1,
		&CM.SurfaceBasedAzimuth},
	{"SURFACE_BASED_INST_ELEVATION",	"REAL",		1,
		&CM.SurfaceBasedElevation},
	{"SURFACE_BASED_INST_METHOD",		"STRING",	1,
		CM.SurfaceBasedMethod},
	{"VERTICAL_IMAGE_PLANE_VECTOR",		"REAL",	LBL_VECTOR_ARRAY,
		CM.VerticalImagePlaneV},
	{ 0, 0, 0, 0}};

static MpfLabelTable_typ	OldCameraModel[] = {
	{"AZIMUTH",				"REAL",		1,
		&CM.Azimuth},
	{"AZIMUTH_METHOD",			"STRING",	1,
		CM.AzimuthMethod},
	{"ELEVATION",				"REAL",		1,
		&CM.Elevation},
	{"ELEVATION_METHOD",			"STRING",	1,
		CM.ElevationMethod},
	{"SURFACE_BASED_CAMERA_AZIMUTH",	"REAL",		1,
		&CM.SurfaceBasedAzimuth},
	{"SURFACE_BASED_CAMERA_ELEVATION",	"REAL",		1,
		&CM.SurfaceBasedElevation},
	{"SURFACE_BASED_CAMERA_METHOD",		"STRING",	1,
		CM.SurfaceBasedMethod},
	{ 0, 0, 0, 0}};

/*** IMP Commands Table  ***/
static MpfLabelTable_typ	Command[] = {
	{"AUTO_EXPOSURE_DATA_CUT",		"INT",		1,
		&CMD.AutoExposureDataCut},
	{"AUTO_EXPOSURE_PIXEL_FRACTION",	"REAL",		1,
		&CMD.AutoExposurePixelFraction},
	{"BAD_PIXEL_REPLACEMENT_FLAG",		"STRING",	1,
		CMD.BadPixelReplacement},
	{"COMMAND_NAME",			"STRING",	1,
		CMD.CommandName},
	{"DARK_CURRENT_CORRECTION_FLAG",	"STRING",	1,
		CMD.DarkCurrentCorrection},
	{"DOWNLOAD_TYPE",			"STRING",	1,
		CMD.Download},
	{"EXPOSURE_COUNT",			"INT",		1,
		&CMD.ExposureCount},
	{"FLAT_FIELD_CORRECTION_FLAG",		"STRING",	1,
		CMD.FlatFieldCorrection},
	{"SHUTTER_EFFECT_CORRECTION_FLAG",	"STRING",	1,
		CMD.ShutterEffectCorrection},
	{"SQRT_COMPRESSION_FLAG",		"STRING",	1,
		CMD.SqrtCompression},
	{0, 0, 0, 0}};

/***  Compression Table  ***/
static MpfLabelTable_typ	Compression[] = {
	{"INST_CMPRS_BLK_SIZE",			"INT",		2,
		CMP.InstCmprsBlkSize},
	{"INST_CMPRS_BLOCKS",			"INT",		1,
		&CMP.InstCmprsBlocks},
	{"INST_CMPRS_MODE",			"INT",		1,
		&CMP.InstCmprsMode},
	{"INST_CMPRS_NAME",			"STRING",	1,
		CMP.InstCmprsName},
	{"INST_CMPRS_PARAM",			"INT",		1,
		&CMP.InstCmprsParam},
	{"INST_CMPRS_QUALITY",			"INT",		1,
		&CMP.InstCmprsQuality},
	{"INST_CMPRS_QUANTZ_TBL_ID",		"STRING",	1,
		CMP.InstCmprsQuantzTblId},
	{"INST_CMPRS_RATE",			"REAL",		1,
		&CMP.InstCmprsRate},
	{"INST_CMPRS_RATIO",			"REAL",		1,
		&CMP.InstCmprsRatio},
	{"INST_CMPRS_SYNC_BLKS",		"INT",		1,
		&CMP.InstCmprsSyncBlks},
	{"PIXEL_AVERAGING_HEIGHT",		"INT",		1,
		&CMP.PixelAveragingHeight},
	{"PIXEL_AVERAGING_WIDTH",		"INT",		1,
		&CMP.PixelAveragingWidth},
	{"RICE_OPTION_VALUE",			"INT",		1,
		&CMP.RiceOptionValue},
	{"RICE_START_OPTION",			"INT",		1,
		&CMP.RiceStartOption},
	{"SQRT_MAXIMUM_PIXEL",			"INT",		1,
		&CMP.SqrtMaxPixel},
	{"SQRT_MINIMUM_PIXEL",			"INT",		1,
		&CMP.SqrtMinPixel},
	{0, 0, 0, 0}};

/***  Observation Property Table  ***/
static MpfLabelTable_typ	Observation[] = {
	{"APPLICATION_PACKET_ID",		"INT",		1,
		&OBS.APID},
	{"ERROR_PIXELS",			"INT",		1,
		&OBS.ErrorPixels},
	{"EXPOSURE_DURATION",			"REAL",		1,
		&OBS.ExposureDuration},
	{"EXPOSURE_TYPE",			"STRING",	1,
		OBS.ExposureType},
	{"FILTER_NAME",				"STRING",	1,
		OBS.FilterName},
	{"FILTER_NUMBER",			"INT",		1,
		&OBS.FilterNumber},
	{"FIRST_LINE",				"INT",		1,
		&OBS.FirstLine},
	{"FIRST_LINE_SAMPLE",			"INT",		1,
		&OBS.FirstLineSample},
	{"FRAME_ID",				"STRING",	1,
		OBS.FrameId},
	{"IMAGE_ID",				"INT",		1,
		&OBS.ImageId},
	{"IMAGE_OBSERVATION_TYPE",		"STRING",	1,
		OBS.ImageObservationType},
	{"IMAGE_TIME",				"STRING",	1,
		OBS.ImageTime},
	{"INSTRUMENT_DEPLOYMENT_STATE",		"STRING",	1,
		OBS.InstDeploymentState},
	{"INSTRUMENT_TEMPERATURE",		"REAL",		2,
		OBS.InstTemperature},
	{"INSTRUMENT_TEMPERATURE_COUNT",	"INT",		2,
		OBS.InstTemperatureCount},
	{"MAXIMUM",				"INT",		1,
		&OBS.Maximum},
	{"MEAN",				"REAL",		1,
		&OBS.Mean},
	{"MEDIAN",				"INT",		1,
		&OBS.Median},
	{"MINIMUM",				"INT",		1,
		&OBS.Minimum},
	{"MPF_LOCAL_TIME",			"STRING",	1,
		OBS.LocalTime},
	{"OBSERVATION_NAME",			"STRING",	1,
		OBS.ObservationName},
	{"PLANET_DAY_NUMBER",			"INT",		1,
		&OBS.PlanetDayNumber},
	{"SPACECRAFT_CLOCK_START_COUNT",	"INT",		1,
		&OBS.SCLK_StartCount},
	{"STANDARD_DEVIATION",			"REAL",		1,
		&OBS.StandardDeviation},
	{"TARGET_NAME",				"STRING",	1,
		OBS.TargetName},
	{ 0, 0, 0, 0}};

/***  PDS Property Table  ***/
static MpfLabelTable_typ	Pds[] = {
	{"APPLICATION_PACKET_NAME",		"STRING",	1,
		PDS.ApplicationPacketName},
	{"BANDS",				"INT",		1,
		&PDS.Bands},
	{"CHECKSUM",				"INT",		1,
		&PDS.Checksum},
	{"COMMAND_DESC",			"STRING",	1,
		PDS.CommandDesc},
	{"DARK_CURRENT_DOWNLOAD_FLAG",		"STRING",	1,
		PDS.DarkCurrentDownloadFlag},
	{"DATA_SET_ID",				"STRING",	1,
		PDS.DataSetId},
	{"DATA_SET_NAME",			"STRING",	1,
		PDS.DataSetName},
	{"DETECTOR_PIXEL_HEIGHT",		"REAL",		1,
		&PDS.DetectorPixelHeight},
	{"DETECTOR_PIXEL_WIDTH",		"REAL",		1,
		&PDS.DetectorPixelWidth},
	{"INST_CMPRS_DESC",			"STRING",	1,
		PDS.InstCmprsDesc},
	{"INST_CMPRS_QUANTZ_TYPE",		"STRING",	1,
		PDS.InstCmprsQuantzType},
	{"INTERCHANGE_FORMAT",			"STRING",	1,
		PDS.InterchangeFormat},
	{"LINES",				"INT",		1,
		&PDS.Lines},
	{"LINE_SAMPLES",			"INT",		1,
		&PDS.LineSamples},
	{"PDS_VERSION_ID",			"STRING",	1,
		PDS.PdsVersionId},
	{"PROCESSING_HISTORY_TEXT"	,	"STRING",	1,
		PDS.ProcessingHistoryText},
	{"PRODUCER_FULL_NAME",			"STRING",	1,
		PDS.ProducerFullName},
	{"PRODUCER_INSTITUTION_NAME",		"STRING",	1,
		PDS.ProducerInstitutionName},
	{"SAMPLE_BITS",				"INT",		1,
		&PDS.SampleBits},
	{"SAMPLE_BIT_MASK",			"STRING",	1,
		PDS.SampleBitMask},
	{"SAMPLE_TYPE",				"STRING",	1,
		PDS.SampleType},
	{"SOLAR_AZIMUTH",			"REAL",		1,
		&PDS.SolarAzimuth},
	{"SOLAR_ELEVATION",			"REAL",		1,
		&PDS.SolarElevation},
/***  Cool to have, but ...  ****
	{"BODY_CENTER_DISTANCE",		"REAL",		1,
		&PDS.BodyCenterDistance},
	{"COORDINATE_SYSTEM_NAME",		"STRING",	1,
		PDS.CoordinateSystemName},
	{"EMISSION_ANGLE",			"REAL",		1,
		&PDS.EmissionAngle},
	{"INCIDENCE_ANGLE",			"REAL",		1,
		&PDS.IncidenceAngle},
	{"PHASE_ANGLE",				"REAL",		1,
		&PDS.PhaseAngle},	
	{"SUB_SPACECRAFT_LATITUDE",		"REAL",		1,
		&PDS.SubSpacecraftLatitude},
	{"SUB_SPACECRAFT_LONGITUDE",		"REAL",		1,
		&PDS.SubSpacecraftLongitude},
	{"SURFACE_NORMAL_AZIMUTH",		"REAL",		1,
		&PDS.SurfaceNormalAzimuth},
	{"SURFACE_NORMAL_ELEVATION",		"REAL",		1,
		&PDS.SurfaceNormalElevation},
	{"INERTIAL_AREOCENTRIC_MATRIX",	"REAL",	LBL_TRANSFORM_ARRAY,
		PDS.InertialAerocentricMatrix},
	{"INST_CMPRS_QUANTZ_TBL",		"INT",	LBL_JPEG_TBL_SIZE,
		PDS.InstCmprsQuantzTbl},
	{"MFX_MBF_TMATRIX",			"REAL",	LBL_TRANSFORM_ARRAY,
		PDS.MfxMbfMatrix},
/**/
	{ 0, 0, 0, 0}};

/***  Mosaic Property Table  ***/
static MpfLabelTable_typ	Mosaic[] = {
	{"AZIMUTH_OF_SAMPLE_ONE",	"REAL",		1,
		&MOS.AzimuthOfSampleOne},
	{"MAP_PROJECTION_TYPE",		"STRING",	1,
		MOS.MapProjectionType},
	{"MAXIMUM_ELEVATION",			"REAL",		1,
		&MOS.MaximumElevation},
	{"MAXIMUM_X",				"REAL",		1,
		&MOS.MaximumX},
	{"MAXIMUM_Y",				"REAL",		1,
		&MOS.MaximumY},
	{"MINIMUM_ELEVATION",			"REAL",		1,
		&MOS.MinimumElevation},
	{"MINIMUM_X",				"REAL",		1,
		&MOS.MinimumX},
	{"MINIMUM_Y",				"REAL",		1,
		&MOS.MinimumY},
	{"MOSAIC_AZIMUTH_RESOLUTION",		"REAL",		1,
		&MOS.MosaicAzimuthResolution},
	{"MOSAIC_REFERENCE_AZIMUTH",	"REAL",		1,
		&MOS.MosaicReferenceAzimuth},
	{"MOSAIC_REFERENCE_ELEVATION",	"REAL",		1,
		&MOS.MosaicReferenceElevation},
	{"MOSAIC_REFERENCE_LINE",	"REAL",		1,
		&MOS.MosaicReferenceLine},
	{"MOSAIC_REFERENCE_SAMPLE",	"REAL",		1,
		&MOS.MosaicReferenceSample},
	{"MOSAIC_RESOLUTION",		"REAL",		1,
		&MOS.MosaicResolution},
	{"NADIR_LINE",			"REAL",		1,
		&MOS.NadirLine},
	{"NADIR_SAMPLE",		"REAL",		1,
		&MOS.NadirSample},
	{"ORIENTATION",			"STRING",	1,
		MOS.Orientation},
	{"RADIAL_MOSAIC_RESOLUTION",	"REAL",		1,
		&MOS.RadialMosaicResolution},
	{"RADIANCE_SCALING_FACTOR",		"REAL",		1,
		&MOS.RadianceScalingFactor},
	{"START_AZIMUTH",			"REAL",		1,
		&MOS.StartAzimuth},
	{"STOP_AZIMUTH",			"REAL",		1,
		&MOS.StopAzimuth},
	{"ZERO_ELEVATION_IMAGE_LINE",	"INT",		1,
		&MOS.ZeroElevationImageLine},
	{ 0, 0, 0, 0}};

/******************************************************************************
/*				IMP_CAMERA_MODEL_PROPERTY
/*
/*****************************************************************************/
int	ImpCameraModelProperty(
  int	Unit,
  int	Obtain,
  LblCntrl_typ	*Cntrl,
  ImpCameraProperty_typ	*Label)
{ int	status;
  LblCntrl_typ	OldCntrl;
  MpfLabelProcess_typ	MpfLabel = {CameraModel, "PROPERTY", "PROPERTY",
                                    CAMERA_LBL_NAME},
			OldMpfLabel = {OldCameraModel, "PROPERTY", "PROPERTY",
                                    CAMERA_LBL_NAME};

  memmove(&CM,Label,sizeof(ImpCameraProperty_typ));

  /***  For READs, try reading old label items first  ***/
  if (Obtain == LBL_READ)
  { memset(&OldCntrl,0,sizeof(LblCntrl_typ));
    OldCntrl.ProceedOnError = TRUE;
    OldCntrl.Instrument = LBL_IMP;

    status = MpfLblProcessor(Unit, Obtain, &OldCntrl, &OldMpfLabel);
  }

  status = MpfLblProcessor(Unit, Obtain, Cntrl, &MpfLabel);

  memmove(Label,&CM,sizeof(ImpCameraProperty_typ));

  return (status);
}

/******************************************************************************
/*				IMP_COMMAND_PROPERTY
/*
/*****************************************************************************/
int	ImpCommandProperty(
  int	Unit,
  int	Obtain,
  LblCntrl_typ	*Cntrl,
  ImpCmndProperty_typ	*Label)
{ int	status;
  MpfLabelProcess_typ	MpfLabel = {Command, "PROPERTY", "PROPERTY",
                                    COMMAND_LBL_NAME};

  memmove(&CMD,Label,sizeof(ImpCmndProperty_typ));

  status = MpfLblProcessor(Unit, Obtain, Cntrl, &MpfLabel);

  memmove(Label,&CMD,sizeof(ImpCmndProperty_typ));

  return (status);
}

/******************************************************************************
/*				IMP_COMPRESSION_PROPERTY
/*
/*****************************************************************************/
int	ImpCompressionProperty(
  int	Unit,
  int	Obtain,
  LblCntrl_typ	*Cntrl,
  ImpCompProperty_typ	*Label)
{ int	status;
  MpfLabelProcess_typ	MpfLabel = {Compression, "PROPERTY", "PROPERTY",
                                    DECOMP_LBL_NAME};

  memmove(&CMP,Label,sizeof(ImpCompProperty_typ));

  status = MpfLblProcessor(Unit, Obtain, Cntrl, &MpfLabel);

  memmove(Label,&CMP,sizeof(ImpCompProperty_typ));

  return (status);
}

/******************************************************************************
/*				IMP_PDS_PROPERTY
/*
/*****************************************************************************/
int	ImpPdsProperty(
  int	Unit,
  int	Obtain,
  LblCntrl_typ	*Cntrl,
  ImpPdsProperty_typ	*Label)
{ int	status;
  MpfLabelProcess_typ	MpfLabel = {Pds, "PROPERTY", "PROPERTY",
                                    PDS_LBL_NAME};

  memmove(&PDS,Label,sizeof(ImpPdsProperty_typ));

  status = MpfLblProcessor(Unit, Obtain, Cntrl, &MpfLabel);

  memmove(Label,&PDS,sizeof(ImpPdsProperty_typ));

  return (status);
}

/******************************************************************************
/*				IMP_MOSAIC_PROPERTY
/*
/*****************************************************************************/
int	ImpMosaicProperty(
  int	Unit,
  int	Obtain,
  LblCntrl_typ	*Cntrl,
  ImpMosaicProperty_typ	*Label)
{ int	status;
  MpfLabelProcess_typ	MpfLabel = {Mosaic, "PROPERTY", "PROPERTY",
                                    MOS_LBL_NAME};

  memmove(&MOS,Label,sizeof(ImpMosaicProperty_typ));

  status = MpfLblProcessor(Unit, Obtain, Cntrl, &MpfLabel);

  memmove(Label,&MOS,sizeof(ImpMosaicProperty_typ));

  return (status);
}

/******************************************************************************
/*				IMP_OBSERVATION_PROPERTY
/*
/*****************************************************************************/
int	ImpObservationProperty(
  int	Unit,
  int	Obtain,
  LblCntrl_typ	*Cntrl,
  ImpObsProperty_typ	*Label)
{ int	status;
  MpfLabelProcess_typ	MpfLabel = {Observation, "PROPERTY", "PROPERTY",
                                    OBS_LBL_NAME};

  memmove(&OBS,Label,sizeof(ImpObsProperty_typ));

  status = MpfLblProcessor(Unit, Obtain, Cntrl, &MpfLabel);

  memmove(Label,&OBS,sizeof(ImpObsProperty_typ));

  return (status);
}
