/**  Copyright (c) 1995, California Institute of Technology		**/
/**  U. S. Government sponsorship under NASA contract is acknowledged	**/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "mpf_apx_labels.h"

#define  COMMAND_LBL_NAME	"APX_COMMANDS"

static char	LogMsgBuf[256];
static ApxCmndProperty_typ	CMD;
static ApxObsProperty_typ	OBS;
static ApxPdsProperty_typ	PDS;

/***  APXS Command Property Table  ***/
static MpfLabelTable_typ	Command[] = {
	{"COMMAND_SEQUENCE_NUMBER",		"INT",		1,
		&CMD.CommandSequenceNumber},
	{0, 0, 0, 0}};

/***  Observation Property Table  ***/
static MpfLabelTable_typ	Observation[] = {
	{"ACCUMULATION_COUNT",			"INT",		1,
		&OBS.AccumulationCount},
	{"ALPHA_SAMPLING_DURATION",		"STRING",	1,
		OBS.AlphaSamplingDuration},
	{"AMBIENT_TEMPERATURE",			"REAL",		LBL_APX_TEMPERATURES,
		OBS.AmbientTemperature},
	{"APPLICATION_PACKET_ID",		"INT",		1,
		&OBS.APID},
	{"APXS_COMMUNICATION_ERROR_COUNT",	"INT",		1,
		&OBS.ApxsCommunicationErrorCount},
	{"APXS_MECHANISM_ANGLE",		"REAL",		1,
		&OBS.ApxsMechanismAngle},
	{"BACKGROUND_SAMPLING_DURATION",	"STRING",	1,
		OBS.BackgroundSamplingDuration},
	{"COMMAND_SEQUENCE_NUMBER",		"INT",		1,
		&OBS.CommandSequenceNumber},
	{"CONTACT_SENSOR_STATE",		"INT",		1,
		&OBS.ContactSensorState},
	{"CONVERTER_CURRENT",			"INT",		1,
		&OBS.ConverterCurrent},
	{"CONVERTER_VOLTAGE",			"INT",		1,
		&OBS.ConverterVoltage},
	{"INSTRUMENT_HOST_TEMPERATURE",		"INT",		LBL_RVR_TEMPERATURES,
		OBS.InstHostTemperature},
	{"INSTRUMENT_TEMPERATURE",		"REAL",		LBL_APX_TEMPERATURES,
		OBS.InstrumentTemperature},
	{"LINEAR_ACCELEROMETER",		"REAL",		LBL_RVR_ACCELEROMETER,
		OBS.LinearAccelerometer},
	{"PACKET_CREATION_SCLK",		"INT",		1,
		&OBS.Packet_SCLK},
	{"PLANET_DAY_NUMBER",			"INT",		1,
		&OBS.PlanetDayNumber},
	{"PROTON_SAMPLING_DURATION",		"STRING",	1,
		OBS.ProtonSamplingDuration},
	{"ROVER_HEADING",			"INT",		1,
		&OBS.RoverHeading},
	{"ROVER_POSITION",			"REAL",		LBL_RVR_POSITION,
		OBS.RoverPosition},
	{"SPACECRAFT_CLOCK_START_COUNT",	"INT",		1,
		&OBS.SCLK_StartCount},
	{"SPACECRAFT_CLOCK_STOP_COUNT",		"INT",		1,
		&OBS.SCLK_StopCount},
	{"START_ERROR_STATE",			"INT",		1,
		&OBS.StartErrorState},
	{"START_TIME",				"STRING",	1,
		OBS.StartTime},
	{"STOP_ERROR_STATE",			"INT",		1,
		&OBS.StopErrorState},
	{"STOP_TIME",				"STRING",	1,
		OBS.StopTime},
	{"TARGET_NAME",				"STRING",	1,
		OBS.TargetName},
	{"XRAY_SAMPLING_DURATION",		"STRING",	1,
		OBS.XraySamplingDuration},
	{0, 0, 0, 0}};

/***  PDS Label Table  ***/
static MpfLabelTable_typ	PdsLabel[] = {
	{"APPLICATION_PACKET_NAME",	"STRING",	1,
		PDS.ApplicationPacketName},
	{"COMMAND_DESC",		"STRING",	1,
		PDS.CommandDescription},
	{"DATA_SET_ID",			"STRING",	1,
		PDS.DataSetId},
	{"DATA_SET_NAME",		"STRING",	1,
		PDS.DataSetName},
	{"INTERCHANGE_FORMAT",		"STRING",	1,
		PDS.InterchangeFormat},
	{"INSTRUMENT_HOST_NAME",	"STRING",	1,
		PDS.InstrumentHostName},
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
	{0, 0, 0, 0}};

/******************************************************************************
/*				APX_COMMAND_PROPERTY
/*
/*****************************************************************************/
int	ApxCommandProperty(
  int	Unit,
  int	Obtain,
  LblCntrl_typ	*Cntrl,
  ApxCmndProperty_typ	*Label)
{ int	status;
  MpfLabelProcess_typ	MpfLabel = {Command, "PROPERTY", "PROPERTY",
                                    COMMAND_LBL_NAME};

  memmove(&CMD,Label,sizeof(ApxCmndProperty_typ));

  status = MpfLblProcessor(Unit, Obtain, Cntrl, &MpfLabel);

  memmove(Label,&CMD,sizeof(ApxCmndProperty_typ));

  return (status);
}

/******************************************************************************
/*				APX_OBSERVATION_PROPERTY
/*
/*****************************************************************************/
int	ApxObservationProperty(
  int	Unit,
  int	Obtain,
  LblCntrl_typ	*Cntrl,
  ApxObsProperty_typ	*Label)
{ int	status;
  MpfLabelProcess_typ	MpfLabel = {Observation, "PROPERTY", "PROPERTY",
                                    OBS_LBL_NAME};

  memmove(&OBS,Label,sizeof(ApxObsProperty_typ));

  status = MpfLblProcessor(Unit, Obtain, Cntrl, &MpfLabel);

  memmove(Label,&OBS,sizeof(ApxObsProperty_typ));

  return (status);
}

/******************************************************************************
/*				APX_PDS_LABEL
/*
/*****************************************************************************/
int	ApxPdsProperty(
  int	Unit,
  int	Obtain,
  LblCntrl_typ	*Cntrl,
  ApxPdsProperty_typ	*Label)
{ int	status;
  MpfLabelProcess_typ	MpfLabel = {PdsLabel, "PROPERTY", "PROPERTY",
                                    PDS_LBL_NAME};

  memmove(&PDS,Label,sizeof(ApxPdsProperty_typ));

  status = MpfLblProcessor(Unit, Obtain, Cntrl, &MpfLabel);

  memmove(Label,&PDS,sizeof(ApxPdsProperty_typ));

  return (status);
}
