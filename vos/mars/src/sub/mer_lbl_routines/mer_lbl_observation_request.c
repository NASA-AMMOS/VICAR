/**  Copyright (c) 2002, California Institute of Technology             **/
/**  U. S. Government sponsorship under NASA contract is acknowledged   **/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "mer_lbl_observation_request.h"

/******************************************************************************
 *				MER_LBL_OBSERVATION_REQUEST
 *
 *	This module contains routines to help create, read/write and print
 *  a ObservationRequest property label.  It is part of the MIPL label API
 *  package, using a lower-level label processor to do the real work.  This
 *  package basically defines a table that the lower-level routines use.
 *  The table is the bridge between how the application access the label
 *  elements, and how the label processor specifies the label components
 *  to the VICAR label Run Time Library (RTL).
 *
 *	The label processor interface structures and routines are defined in
 *  the file "lbl_gen_api.h" (Check the label processor documentation for
 *  how to create APIs like this one).  The application program interface
 *  structures are defined in the file "lbl_command.h".  The
 *  implementation supporting the interface is this module.
 *
 *	The primary routine used by a typical application program is
 *  MerLblObservationRequest.  This routine requires exactly 4 parameters.
 *  All label API routines must (should) have the same first three parameters:
 *		INT	VICAR RTL unit number of an opened image file.
 *			This is the file where the label will be read or
 *			written.  It must be open with the appropriate
 *			I/O mode
 *		INT	Read/Write flag.  If the value of this parameter is
 *			non-zero, the label will be read from the file.  If
 *			the value of the parameter is zero, a new label will
 *			be written to the file.
 *		VOID*	The structure that an application program will use
 *			to set or retreive the label element values.  Okay
 *			this really isn't a VOID*, but it is a pointer to
 *			the label specific structure.
 *		INT	The instance of this label type.  They typical value
 *			of this parameter should be '1'.
 *
 *	The other two routines contined in this module were included for
 *  development and testing purposes and like the label processing code, use
 *  generic lower-level routines.
 *
 *	All routines use the return_status.h macros to identify the
 *  success or failure of the routine.  Basically, a value of zero represents
 *  a successful completion of the label processing, a non-zero value
 *  indicates a failure.
 *****************************************************************************/

#define  LBL_SIZE(x)	sizeof(((MerLblObsRequest_typ *)0)->x)

static LblApiElement_typ	LabelTbl[] = {
	{"SOURCE_ID",				"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(MerLblObsRequest_typ, SourceId.Value),
		LBL_OFFSET(MerLblObsRequest_typ, SourceId.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SourceId.Value)},

	{"COMMAND_INSTRUMENT_ID",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(MerLblObsRequest_typ, CommandInstrumentId.Value),
		LBL_OFFSET(MerLblObsRequest_typ, CommandInstrumentId.Valid),
		LBL_NO_RETURN,	LBL_SIZE(CommandInstrumentId.Value)},

	{"FILTER_NAME",				"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(MerLblObsRequest_typ, FilterName.Value),
		LBL_OFFSET(MerLblObsRequest_typ, FilterName.Valid),
		LBL_NO_RETURN,	LBL_SIZE(FilterName.Value)},

	{"FILTER_NUMBER",			"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(MerLblObsRequest_typ, FilterNumber.Value),
                LBL_OFFSET(MerLblObsRequest_typ, FilterNumber.Valid),
                LBL_NO_RETURN,  LBL_SIZE(FilterNumber.Value)},

	{"AUTO_EXPOSURE_DATA_CUT",		"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(MerLblObsRequest_typ, AutoExposureDataCut.Value),
                LBL_OFFSET(MerLblObsRequest_typ, AutoExposureDataCut.Valid),
                LBL_NO_RETURN,  LBL_SIZE(AutoExposureDataCut.Value)},

	{"AUTO_EXPOSURE_PERCENT",		"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(MerLblObsRequest_typ, AutoExposurePercent.Value),
                LBL_OFFSET(MerLblObsRequest_typ, AutoExposurePercent.Valid),
                LBL_NO_RETURN,  LBL_SIZE(AutoExposurePercent.Value)},

	{"AUTO_EXPOSURE_PIXEL_FRACTION",	"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(MerLblObsRequest_typ, AutoExposurePixelFraction.Value),
                LBL_OFFSET(MerLblObsRequest_typ, AutoExposurePixelFraction.Valid),
                LBL_NO_RETURN,  LBL_SIZE(AutoExposurePixelFraction.Value)},

	{"BAD_PIXEL_REPLACEMENT_FLAG",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(MerLblObsRequest_typ, BadPixelReplacementFlag.Value),
		LBL_OFFSET(MerLblObsRequest_typ, BadPixelReplacementFlag.Valid),
		LBL_NO_RETURN,	LBL_SIZE(BadPixelReplacementFlag.Value)},

	{"DETECTOR_ERASE_COUNT",		"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(MerLblObsRequest_typ, DetectorEraseCount.Value),
                LBL_OFFSET(MerLblObsRequest_typ, DetectorEraseCount.Valid),
                LBL_NO_RETURN,  LBL_SIZE(DetectorEraseCount.Value)},

	{"EARLY_PIXEL_SCALE_FLAG",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(MerLblObsRequest_typ, EarlyImageReturnFlag.Value),
		LBL_OFFSET(MerLblObsRequest_typ, EarlyImageReturnFlag.Valid),
		LBL_NO_RETURN,	LBL_SIZE(EarlyImageReturnFlag.Value)},

	{"EARLY_IMAGE_RETURN_FLAG",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(MerLblObsRequest_typ, EarlyPixelScaleFlag.Value),
		LBL_OFFSET(MerLblObsRequest_typ, EarlyPixelScaleFlag.Valid),
		LBL_NO_RETURN,	LBL_SIZE(EarlyPixelScaleFlag.Value)},

	{"EXPOSURE_TYPE",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(MerLblObsRequest_typ, ExposureType.Value),
		LBL_OFFSET(MerLblObsRequest_typ, ExposureType.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ExposureType.Value)},

	{"EXPOSURE_SCALE_FACTOR",		"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(MerLblObsRequest_typ, ExposureScaleFactor.Value),
                LBL_OFFSET(MerLblObsRequest_typ, ExposureScaleFactor.Valid),
                LBL_NO_RETURN,  LBL_SIZE(ExposureScaleFactor.Value)},

	{"EXPOSURE_DURATION_COUNT",		"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(MerLblObsRequest_typ, ExposureDurationCount.Value),
                LBL_OFFSET(MerLblObsRequest_typ, ExposureDurationCount.Valid),
                LBL_NO_RETURN,  LBL_SIZE(ExposureDurationCount.Value)},

	{"EXPOSURE_TABLE_ID",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(MerLblObsRequest_typ, ExposureTableId.Value),
		LBL_OFFSET(MerLblObsRequest_typ, ExposureTableId.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ExposureTableId.Value)},

	{"EXPOSURE_TBL_UPDATE_FLAG",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(MerLblObsRequest_typ, ExposureTblUpdateFlag.Value),
		LBL_OFFSET(MerLblObsRequest_typ, ExposureTblUpdateFlag.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ExposureTblUpdateFlag.Value)},

	{"FLAT_FIELD_CORRECTION_FLAG",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(MerLblObsRequest_typ, FlatFieldCorrectionFlag.Value),
		LBL_OFFSET(MerLblObsRequest_typ, FlatFieldCorrectionFlag.Valid),
		LBL_NO_RETURN,	LBL_SIZE(FlatFieldCorrectionFlag.Value)},

	{"INSTRUMENT_COORDINATE",		"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(MerLblObsRequest_typ, InstrumentCoordinate[0].Value),
                LBL_OFFSET(MerLblObsRequest_typ, InstrumentCoordinate[0].Valid),
                LBL_NO_RETURN,  LBL_SIZE(InstrumentCoordinate[0].Value)},

	{"INSTRUMENT_COORDINATE",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(MerLblObsRequest_typ, InstrumentCoordinate[1].Value),
                LBL_OFFSET(MerLblObsRequest_typ, InstrumentCoordinate[1].Valid),
                LBL_NO_RETURN,  LBL_SIZE(InstrumentCoordinate[1].Value)},

	{"INSTRUMENT_COORDINATE",		"REAL",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(MerLblObsRequest_typ, InstrumentCoordinate[2].Value),
                LBL_OFFSET(MerLblObsRequest_typ, InstrumentCoordinate[2].Valid),
                LBL_NO_RETURN,  LBL_SIZE(InstrumentCoordinate[2].Value)},

	{"INSTRUMENT_COORDINATE__UNIT",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(MerLblObsRequest_typ, InstrumentCoordinateUnit[0].Value),
                LBL_OFFSET(MerLblObsRequest_typ, InstrumentCoordinateUnit[0].Valid),
                LBL_NO_RETURN,  LBL_SIZE(InstrumentCoordinateUnit[0].Value)},

	{"INSTRUMENT_COORDINATE__UNIT",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(MerLblObsRequest_typ, InstrumentCoordinateUnit[1].Value),
                LBL_OFFSET(MerLblObsRequest_typ, InstrumentCoordinateUnit[1].Valid),
                LBL_NO_RETURN,  LBL_SIZE(InstrumentCoordinateUnit[1].Value)},

	{"INSTRUMENT_COORDINATE__UNIT",		"STRING",	LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(MerLblObsRequest_typ, InstrumentCoordinateUnit[2].Value),
                LBL_OFFSET(MerLblObsRequest_typ, InstrumentCoordinateUnit[2].Valid),
                LBL_NO_RETURN,  LBL_SIZE(InstrumentCoordinateUnit[2].Value)},

	{"INSTRUMENT_COORDINATE_ID",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(MerLblObsRequest_typ, InstrumentCoordinateId.Value),
		LBL_OFFSET(MerLblObsRequest_typ, InstrumentCoordinateId.Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentCoordinateId.Value)},

	{"INSTRUMENT_BORESIGHT_ID",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(MerLblObsRequest_typ, InstrumentBoresiteId.Value),
		LBL_OFFSET(MerLblObsRequest_typ, InstrumentBoresiteId.Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstrumentBoresiteId.Value)},

	{"INSTRUMENT_IDLE_TIMEOUT",		"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(MerLblObsRequest_typ, InstrumentIdleTimeout.Value),
                LBL_OFFSET(MerLblObsRequest_typ, InstrumentIdleTimeout.Valid),
                LBL_NO_RETURN,  LBL_SIZE(InstrumentIdleTimeout.Value)},

	{"INSTRUMENT_IDLE_TIMEOUT__UNIT",	"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(MerLblObsRequest_typ, InstrumentIdleTimeoutUnit.Value),
                LBL_OFFSET(MerLblObsRequest_typ, InstrumentIdleTimeoutUnit.Valid),
                LBL_NO_RETURN,  LBL_SIZE(InstrumentIdleTimeoutUnit.Value)},

	{"MAX_AUTO_EXPOS_ITERATION_COUNT",	"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(MerLblObsRequest_typ, MaxAutoExposIterationCount.Value),
                LBL_OFFSET(MerLblObsRequest_typ, MaxAutoExposIterationCount.Valid),
                LBL_NO_RETURN,  LBL_SIZE(MaxAutoExposIterationCount.Value)},

	{"SHUTTER_CORRECTION_MODE_ID",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(MerLblObsRequest_typ, ShutterCorrectionModeId.Value),
		LBL_OFFSET(MerLblObsRequest_typ, ShutterCorrectionModeId.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ShutterCorrectionModeId.Value)},

	{"SHUTTER_CORRECT_THRESH_COUNT",	"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(MerLblObsRequest_typ, ShutterCorrectThreshCount.Value),
                LBL_OFFSET(MerLblObsRequest_typ, ShutterCorrectThreshCount.Valid),
                LBL_NO_RETURN,  LBL_SIZE(ShutterCorrectThreshCount.Value)},

	{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}};

static LblApiProcess_typ	Label = {
	LabelTbl,       "PROPERTY",     "PROPERTY",
	"OBSERVATION_REQUEST_PARMS",	LBL_NULL };

/******************************************************************************
 *				MER_LBL_OBSERVATION_REQUEST
 *
 *****************************************************************************/
int     MerLblObservationRequest(
  int   Unit,
  int   Obtain,
  MerLblObsRequest_typ      *LabelItems,
  int	Instance)
{ int   RtnStatus;
  LblApiCntrl_typ	Cntrl;

  Label.Buffer = (void *)LabelItems;
  Label.BufferSize = sizeof(MerLblObsRequest_typ);

  memset(&Cntrl,0,sizeof(LblApiCntrl_typ));
  Cntrl.Instance = Instance;
  Cntrl.FileUnit = Unit;
  Cntrl.Obtain = Obtain;
  Cntrl.ProceedOnError = LBL_TRUE;

  RtnStatus = LblProcessor(&Cntrl, &Label);

  return (RtnStatus);
}

/******************************************************************************
 *				LBL_PRINT_OBSERVATION_REQUEST
 *
 *****************************************************************************/
void	MerLblPrintObservationRequest(
  MerLblObsRequest_typ	*LabelItems)
{
  Label.Buffer = (void *)LabelItems;

  PrintLabelElements( &Label );

  return;
}

/******************************************************************************
 *				LBL_TEST_OBSERVATION_REQUEST
 *
 *****************************************************************************/
void	MerLblTestObservationRequest(
  MerLblObsRequest_typ	*LabelItems)
{
  Label.Buffer = (void *)LabelItems;

  TestLoadLabelElements( &Label );

  return;
}
