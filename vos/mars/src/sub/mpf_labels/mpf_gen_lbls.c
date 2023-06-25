/**  Copyright (c) 1995, California Institute of Technology		**/
/**  U. S. Government sponsorship under NASA contract is acknowledged	**/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include "zvproto.h"

#include "mpf_gen_labels.h"

#define  TLM_COMP_DISCREP	0
#define  TLM_CMND_DISCREP	1

#ifndef TRUE
#define TRUE 1
#endif

static char	LogMsgBuf[256];
static char	LblMsgBuf[256] = {""};
static char	FlagBuf[LBL_LOGICAL_LTH];
static MpfTelemProperty_typ		TLM;

static MpfLabelTable_typ	Telemetry[] = {
	{"EARTH_RECEIVED_START_TIME",		"STRING",	1,
		TLM.EarthReceivedStartTime},
	{"EARTH_RECEIVED_STOP_TIME",		"STRING",	1,
		TLM.EarthReceivedStopTime},
	{"EXPECTED_PACKETS",			"INT",		1,
		&TLM.ExpectedPackets},
	{"INSTRUMENT_ID",			"STRING",	1,
		TLM.InstrumentId},
	{"INSTRUMENT_NAME",			"STRING",	1,
		TLM.InstrumentName},
	{"MISSION_NAME",			"STRING",	1,
		TLM.MissionName},
	{"PRODUCER_ID",				"STRING",	1,
		TLM.ProducerId},
	{"PRODUCT_CREATION_TIME",		"STRING",	1,
		TLM.ProductCreationTime},
	{"PRODUCT_ID",				"STRING",	1,
		TLM.ProductId},
	{"RECEIVED_PACKETS",			"INT",		1,
		&TLM.ReceivedPackets},
	{"SOFTWARE_NAME",			"STRING",	1,
		TLM.SoftwareName},
	{"SOFTWARE_VERSION_ID",			"STRING",	1,
		TLM.SoftwareVersionId},
	{"SOURCE_PRODUCT_ID",			"STRING",	1,
		TLM.SourceProductId},
	{"SPACECRAFT_NAME",			"STRING",	1,
		TLM.SpacecraftName},
	{"TLM_CMD_DISCREPANCY_FLAG",		"STRING",	1,
		FlagBuf},
		/** TLM.TlmCmdDiscrepancy **/
	{0, 0, 0, 0}};
 
/******************************************************************************
/*				MPF_LBL_ERROR_MSG
/*
/*****************************************************************************/
char	*MpfLblErrorMsg()
{
  return (LblMsgBuf);
}

/******************************************************************************
/*				MPF_LBL_PROCESSOR
/*
/*****************************************************************************/
int	MpfLblProcessor(
  int	Unit,
  int	Obtain,
  LblCntrl_typ	*Cntrl,
  MpfLabelProcess_typ	*Label)
{ int	lc,
	status;
  MpfLabelTable_typ	*Fields = Label->Fields;

  Cntrl->ErrorCount = 0;
  Cntrl->ErrorStatus = 0;

  for (lc=0; Fields[lc].Key; lc++)
  { if (Obtain) status = zlget(Unit, Label->Type,
				Fields[lc].Key,	Fields[lc].Value,
				"FORMAT",	Fields[lc].Format,
				"NELEMENT",	Fields[lc].Elements,
				Label->Name,	Label->PropertyName, 0);
    else status = zladd(Unit, Label->Type,
			Fields[lc].Key,	Fields[lc].Value,
			"FORMAT",	Fields[lc].Format,
			"NELEMENT",	Fields[lc].Elements,
			"ELEMENT",	1,
			"MODE",		"REPLACE",
			Label->Name,	Label->PropertyName, 0);
    if (status != 1)
    { Cntrl->ErrorCount++;
      Cntrl->ErrorStatus = status;
      Cntrl->ErrorLocation = lc;
      sprintf(LblMsgBuf,"Error (%d) processing label (%d): %s",
              status,lc,Fields[lc].Key);
      if (!Cntrl->ProceedOnError) return (TRUE);
    }
  }

  return (Cntrl->ErrorCount);
}

/******************************************************************************
/*				MPF_TELEM_PROPERTY
/*
/*****************************************************************************/
int	MpfTelemProperty(
  int	Unit,
  int	Obtain,
  LblCntrl_typ	*Cntrl,
  MpfTelemProperty_typ	*Label)
{ int	status;
  time_t	tp;
  struct tm	*Created;
  MpfLabelProcess_typ	MpfLabel = {Telemetry, "PROPERTY", "PROPERTY",
                                    TELEM_LBL_NAME};

  memmove(&TLM,Label,sizeof(MpfTelemProperty_typ));

  /*** Convert TLM flag to label string  ***/
  strcpy(FlagBuf,(TLM.TlmCmdDiscrepancy ? "TRUE" : "FALSE"));

  if (Obtain)
  { time(&tp);
    Created = gmtime(&tp);
    if (Created)
       strftime(TLM.ProductCreationTime,LBL_TIME_LTH,
                "%Y-%m-%dT%H:%M:%S",Created);
  }

  status = MpfLblProcessor(Unit, Obtain, Cntrl, &MpfLabel);

  /*** Convert TLM label string to flag  ***/
  if (!status)
     TLM.TlmCmdDiscrepancy = (strcmp(FlagBuf,"TRUE") == 0);

  memmove(Label,&TLM,sizeof(MpfTelemProperty_typ));

  return (status);
}
