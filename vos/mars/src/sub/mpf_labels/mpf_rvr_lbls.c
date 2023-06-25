/**  Copyright (c) 1995, California Institute of Technology		**/
/**  U. S. Government sponsorship under NASA contract is acknowledged	**/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "mpf_rvr_labels.h"

#define  COMMAND_LBL_NAME	"ROVER_COMMANDS"

static char	LogMsgBuf[256];

static RvrCameraProperty_typ	CM;
static RvrCmndProperty_typ	CMD;
static RvrCompProperty_typ	CMP;
static RvrObsProperty_typ	OBS;
static RvrPdsProperty_typ	PDS;
static RvrMosProperty_typ	MOS;

/***  Camera Model Table  ***/
static MpfLabelTable_typ	CameraModel[] = {
	{"AZIMUTH_FOV",				"REAL",	1,
		&CM.AzimuthFOV},
	{"ELEVATION_FOV",			"REAL",	1,
		&CM.ElevationFOV},
	{"FOCAL_CENTER_VECTOR",			"REAL",	LBL_VECTOR_ARRAY,
		CM.FocalCenterC},
	{"HORIZONTAL_IMAGE_PLANE_VECTOR",	"REAL",	LBL_VECTOR_ARRAY,
		CM.HorizontalImagePlaneH},
	{"POINTING_DIRECTION_VECTOR",		"REAL",	LBL_VECTOR_ARRAY,
		CM.PointingDirectionA},
	{"VERTICAL_IMAGE_PLANE_VECTOR",		"REAL",	LBL_VECTOR_ARRAY,
		CM.VerticalImagePlaneV},
	{ 0, 0, 0, 0}};

/***  Rover Commands Table  ***/
static MpfLabelTable_typ	Command[] = {
	{"COMMAND_NAME",			"STRING",	1,
		CMD.CommandName},
/***
	{"COMMAND_SEQUENCE_NUMBER",		"INT",		1,
		&CMD.CommandSequenceNumber},
/**/
	{0, 0, 0, 0}};

/***  Compression Table  ***/
static MpfLabelTable_typ	Compression[] = {
	{"INST_CMPRS_BLK_SIZE",		"INT",		2,
		CMP.InstCmprsBlkSize},
	{"INST_CMPRS_BLOCKS",		"INT",		1,
		&CMP.InstCmprsBlocks},
	{"INST_CMPRS_NAME",		"STRING",	1,
		CMP.InstCmprsName},
	{"INST_CMPRS_RATE",		"REAL",		1,
		&CMP.InstCmprsRate},
	{"INST_CMPRS_RATIO",		"REAL",		1,
		&CMP.InstCmprsRatio},
	{0, 0, 0, 0}};

/***  Observation Property Table  ***/
static MpfLabelTable_typ	Observation[] = {
	{"APPLICATION_PACKET_ID",	"INT",		1,
		&OBS.APID},
	{"COMMAND_SEQUENCE_NUMBER",	"INT",		1,
		&OBS.CommandSequenceNumber},
	{"EXPOSURE_DURATION",		"REAL",		1,
		&OBS.ExposureDuration},
	{"EXPOSURE_TYPE",		"STRING",	1,
		OBS.ExposureType},
	{"FIRST_LINE",			"INT",		1,
		&OBS.FirstLine},
	{"FIRST_LINE_SAMPLE",		"INT",		1,
		&OBS.FirstLineSample},
	{"FRAME_ID",			"STRING",	1,
		OBS.FrameId},
	{"IMAGE_ID",			"STRING",	1,
		OBS.ImageId},
	{"IMAGE_TIME",			"STRING",	1,
		OBS.ImageTime},
	{"INSTRUMENT_TEMPERATURE",	"REAL",		1,
		&OBS.InstrumentTemperature},
	{"LINEAR_ACCELEROMETER",	"REAL",		LBL_RVR_ACCELEROMETER,
		OBS.LinearAccelerometer},
	{"MAXIMUM",			"INT",		1,
		&OBS.Maximum},
	{"MEAN",			"REAL",		1,
		&OBS.Mean},
	{"MEDIAN",			"INT",		1,
		&OBS.Median},
	{"MINIMUM",			"INT",		1,
		&OBS.Minimum},
	{"MPF_LOCAL_TIME",		"STRING",	1,
		OBS.LocalTime},
	{"OBSERVATION_NAME",		"STRING",	1,
		OBS.ObservationName},
	{"PLANET_DAY_NUMBER",		"INT",		1,
		&OBS.PlanetDayNumber},
	{"ROVER_HEADING",		"INT",		1,
		&OBS.RoverHeading},
	{"ROVER_POSITION",		"REAL",		LBL_RVR_POSITION,
		OBS.RoverPosition},
	{"SPACECRAFT_CLOCK_START_COUNT","INT",		1,
		&OBS.SCLK_StartCount},
	{"STANDARD_DEVIATION",		"REAL",		1,
		&OBS.StandardDeviation},
	{"TARGET_NAME",			"STRING",	1,
		OBS.TargetName},
	{ 0, 0, 0, 0}};

/***  PDS Label Table  ***/
static MpfLabelTable_typ	PdsLabel[] = {
	{"APPLICATION_PACKET_NAME",	"STRING",	1,
		PDS.ApplicationPacketName},
	{"BAND_SEQUENCE",		"STRING",	1,
		PDS.BandSequence},
	{"BAND_STORAGE_TYPE",		"STRING",	1,
		PDS.BandStorage},
	{"BANDS",			"INT",		1,
		&PDS.Bands},
	{"CHECKSUM",			"INT",		1,
		&PDS.Checksum},
/***
	{"COMMAND_DESC",		"STRING",	1,
		PDS.CommandDesc},
/**/
	{"DATA_SET_ID",			"STRING",	1,
		PDS.DataSetId},
	{"DATA_SET_NAME",		"STRING",	1,
		PDS.DataSetName},
	{"DETECTOR_PIXEL_HEIGHT",	"REAL",		1,
		&PDS.DetectorPixelHeight},
	{"DETECTOR_PIXEL_WIDTH",	"REAL",		1,
		&PDS.DetectorPixelWidth},
	{"INST_CMPRS_DESC",		"STRING",	1,
		PDS.InstCmprsDesc},
	{"INSTRUMENT_HOST_ID",		"STRING",	1,
		PDS.InstrumentHostId},
	{"INSTRUMENT_HOST_NAME",	"STRING",	1,
		PDS.InstrumentHostName},
	{"INTERCHANGE_FORMAT",		"STRING",	1,
		PDS.InterchangeFormat},
	{"LINES",			"INT",		1,
		&PDS.Lines},
	{"LINE_SAMPLES",		"INT",		1,
		&PDS.LineSamples},
	{"PDS_VERSION_ID",		"STRING",	1,
		PDS.PdsVersionId},
	{"PROCESSING_HISTORY_TEXT",	"STRING",	1,
		PDS.ProcessingHistoryText},
	{"PRODUCER_FULL_NAME",		"STRING",	1,
		PDS.ProducerFullName},
	{"PRODUCER_INSTITUTION_NAME",	"STRING",	1,
		PDS.ProducerInstitutionName},
	{"SAMPLE_BITS",			"INT",		1,
		&PDS.SampleBits},
	{"SAMPLE_BIT_MASK",		"STRING",	1,
		PDS.SampleBitMask},
	{"SAMPLE_TYPE",			"STRING",	1,
		PDS.SampleType},
	{"SOLAR_AZIMUTH",		"REAL",		1,
		&PDS.SolarAzimuth},
	{"SOLAR_ELEVATION",		"REAL",		1,
		&PDS.SolarElevation},
/***  Gonna hafta fill these  ***
	{"BODY_CENTER_DISTANCE",	"REAL",		1,
		&PDS.BodyCenterDistance},
	{"COORDINATE_SYSTEM_NAME",	"STRING",	1,
		PDS.CoordinateSystemName},
	{"INCIDENCE_ANGLE",		"REAL",		1,
		&PDS.IncidenceAngle},
	{"SUB_SPACECRAFT_LATITUDE",	"REAL",		1,
		&PDS.SubSpacecraftLatitude},
	{"SUB_SPACECRAFT_LONGITUDE",	"REAL",		1,
		&PDS.SubSpacecraftLongitude},
	{"SURFACE_NORMAL_AZIMUTH",	"REAL",		1,
		&PDS.SurfaceNormalAzimuth},
	{"SURFACE_NORMAL_ELEVATION",	"REAL",		1,
		&PDS.SurfaceNormalElevation},

/***  Cool to have  ***
	{"INERTIAL_AREOCENTRIC_MATRIX","REAL",		LBL_TRANSFORM_ARRAY,
		&PDS.InertialAreocentricMatrix},
/**/
	{0, 0, 0, 0}};

/***  MOSAIC Label Table  ***/
static MpfLabelTable_typ	MosLabel[] = {
	{"SOURCE_COMMAND_SEQUENCE_NUMBER",	"INT",	LBL_RVR_SOURCE_NUM,
		MOS.SourceCommandSeqNum},
	{"SOURCE_SCLK_START",		"INT",		1,
		&MOS.SourceSclkStart},
	{"SOURCE_SCLK_STOP",		"INT",		1,
		&MOS.SourceSclkStop},
	{"OUTPUT_EXPOSURE",		"REAL",		1,
		&MOS.OutputExposure},
	{"OUTPUT_FIRST_LINE",		"INT",		1,
		&MOS.OutputFirstLine},
	{"OUTPUT_FIRST_LINE_SAMPLE",	"INT",		1,
		&MOS.OutputFirstLineSample},
	{"AVERAGE_DARK_LEVEL",		"REAL",		LBL_VECTOR_ARRAY,
		MOS.AvgDarkLevel},
	{"DARK_CURRENT_CORRECTION",	"STRING",	1,
		MOS.DarkCurrentCorrection},
	{ 0, 0, 0, 0}};

/******************************************************************************
/*				RVR_CAMERA_MODEL_PROPERTY
/*
/*****************************************************************************/
int	RvrCameraModelProperty(
  int	Unit,
  int	Obtain,
  LblCntrl_typ	*Cntrl,
  RvrCameraProperty_typ	*Label)
{ int	status;
  MpfLabelProcess_typ	MpfLabel = {CameraModel, "PROPERTY", "PROPERTY",
                                    CAMERA_LBL_NAME};

  memmove(&CM,Label,sizeof(RvrCameraProperty_typ));

  status = MpfLblProcessor(Unit, Obtain, Cntrl, &MpfLabel);

  memmove(Label,&CM,sizeof(RvrCameraProperty_typ));

  return (status);
}

/******************************************************************************
/*				RVR_COMMAND_PROPERTY
/*
/*****************************************************************************/
int	RvrCommandProperty(
  int	Unit,
  int	Obtain,
  LblCntrl_typ	*Cntrl,
  RvrCmndProperty_typ	*Label)
{ int	status;
  MpfLabelProcess_typ	MpfLabel = {Command, "PROPERTY", "PROPERTY",
                                    COMMAND_LBL_NAME};

  memmove(&CMD,Label,sizeof(RvrCmndProperty_typ));

  status = MpfLblProcessor(Unit, Obtain, Cntrl, &MpfLabel);

  memmove(Label,&CMD,sizeof(RvrCmndProperty_typ));

  return (status);
}

/******************************************************************************
/*				RVR_COMPRESSION_PROPERTY
/*
/*****************************************************************************/
int	RvrCompressionProperty(
  int	Unit,
  int	Obtain,
  LblCntrl_typ	*Cntrl,
  RvrCompProperty_typ	*Label)
{ int	status;
  MpfLabelProcess_typ	MpfLabel = {Compression, "PROPERTY", "PROPERTY",
                                    DECOMP_LBL_NAME};

  memmove(&CMP,Label,sizeof(RvrCompProperty_typ));

  status = MpfLblProcessor(Unit, Obtain, Cntrl, &MpfLabel);

  memmove(Label,&CMP,sizeof(RvrCompProperty_typ));

  return (status);
}

/******************************************************************************
/*				RVR_OBSERVATION_PROPERTY
/*
/*****************************************************************************/
int	RvrObservationProperty(
  int	Unit,
  int	Obtain,
  LblCntrl_typ	*Cntrl,
  RvrObsProperty_typ	*Label)
{ int	status;
  MpfLabelProcess_typ	MpfLabel = {Observation, "PROPERTY", "PROPERTY",
                                    OBS_LBL_NAME};

  memmove(&OBS,Label,sizeof(RvrObsProperty_typ));

  status = MpfLblProcessor(Unit, Obtain, Cntrl, &MpfLabel);

  memmove(Label,&OBS,sizeof(RvrObsProperty_typ));

  return (status);
}

/******************************************************************************
/*				RVR_PDS_LABEL
/*
/*****************************************************************************/
int	RvrPdsProperty(
  int	Unit,
  int	Obtain,
  LblCntrl_typ	*Cntrl,
  RvrPdsProperty_typ	*Label)
{ int	status;
  MpfLabelProcess_typ	MpfLabel = {PdsLabel, "PROPERTY", "PROPERTY",
                                    PDS_LBL_NAME};

  memmove(&PDS,Label,sizeof(RvrPdsProperty_typ));

  status = MpfLblProcessor(Unit, Obtain, Cntrl, &MpfLabel);

  memmove(Label,&PDS,sizeof(RvrPdsProperty_typ));

  return (status);
}

/******************************************************************************
/*				RVR_MOSAIC_LABEL
/*
/*****************************************************************************/
int	RvrMosaicProperty(
  int	Unit,
  int	Obtain,
  LblCntrl_typ	*Cntrl,
  RvrMosProperty_typ	*Label)
{ int	status;
  MpfLabelProcess_typ	MpfLabel = {MosLabel, "PROPERTY", "PROPERTY",
                                    MOS_LBL_NAME};

  memmove(&MOS,Label,sizeof(RvrMosProperty_typ));

  status = MpfLblProcessor(Unit, Obtain, Cntrl, &MpfLabel);

  memmove(Label,&MOS,sizeof(RvrMosProperty_typ));

  return (status);
}
