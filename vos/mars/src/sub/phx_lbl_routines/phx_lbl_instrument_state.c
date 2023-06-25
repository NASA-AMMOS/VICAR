/**  Copyright (c) 1995, California Institute of Technology             **/
/**  U. S. Government sponsorship under NASA contract is acknowledged   **/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#pragma set woff 1032

#include "phx_lbl_instrument_state.h"

/******************************************************************************
 *				PHX_LBL_INSTRUMENT_STATE
 *
 *	This module contains routines to help create, read/write and print an
 *  Instrument State property label.  It is part of the MIPL label API package,
 *  using a lower-level label processor to do the real work.  This package
 *  basically defines a table that the lower-level routines use.  The table
 *  is the bridge between how the application access the label elements, and
 *  how the label processor specifies the label components to the VICAR label
 *  Run Time Library (RTL).
 *
 *	The label processor interface structures and routines are defined in
 *  the file "lbl_gen_api.h" (Check the label processor documentation for
 *  how to create APIs like this one).  The application program interface
 *  structures are defined in the file "lbl_instrument_state.h".  The
 *  iphxementation supporting the interface is this module.
 *
 *	The primary routine used by a typical application program is
 *  PhxLblInstrumentState.  This routine requires exactly 4 parameters.
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
 *  a successful cophxetion of the label processing, a non-zero value
 *  indicates a failure.
 *****************************************************************************/

#define  LBL_SIZE(x)	sizeof(((PhxLblInstrumentState_typ*)0)->x)

static LblApiElement_typ	LabelTbl[] = {
    {"EXPOSURE_DURATION",      "REAL",      LBL_REQUIRED,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(PhxLblInstrumentState_typ, ExposureDuration.Value),
        LBL_OFFSET(PhxLblInstrumentState_typ, ExposureDuration.Valid),
        LBL_NO_RETURN,  LBL_SIZE(ExposureDuration.Value)},

    {"EXPOSURE_DURATION_UNITS",      "STRING",      LBL_REQUIRED,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(PhxLblInstrumentState_typ, ExposureDurationUnits.Value),
        LBL_OFFSET(PhxLblInstrumentState_typ, ExposureDurationUnits.Valid),
        LBL_NO_RETURN,  LBL_SIZE(ExposureDurationUnits.Value)},

    {"SOURCE_TEMP_START_C",      "INT",      LBL_REQUIRED,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(PhxLblInstrumentState_typ, SouceTempStartC.Value),
        LBL_OFFSET(PhxLblInstrumentState_typ, SouceTempStartC.Valid),
        LBL_NO_RETURN,  LBL_SIZE(SouceTempStartC.Value)},

    {"SOURCE_TEMP_END_C",      "INT",      LBL_REQUIRED,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(PhxLblInstrumentState_typ, SourceTempEndC.Value),
        LBL_OFFSET(PhxLblInstrumentState_typ, SourceTempEndC.Valid),
        LBL_NO_RETURN,  LBL_SIZE(SourceTempEndC.Value)},

    {"SOURCE_TEMP_START_RAW",      "INT",      LBL_REQUIRED,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(PhxLblInstrumentState_typ, SourceTempStartRaw.Value),
        LBL_OFFSET(PhxLblInstrumentState_typ, SourceTempStartRaw.Valid),
        LBL_NO_RETURN,  LBL_SIZE(SourceTempStartRaw.Value)},

    {"SOURCE_TEMP_END_RAW",      "INT",      LBL_REQUIRED,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(PhxLblInstrumentState_typ, SourceTempEndRaw.Value),
        LBL_OFFSET(PhxLblInstrumentState_typ, SourceTempEndRaw.Valid),
        LBL_NO_RETURN,  LBL_SIZE(SourceTempEndRaw.Value)},

    {"ELECTRONICS_TEMP_C",      "INT",      LBL_REQUIRED,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(PhxLblInstrumentState_typ, ElectronicsTempC.Value),
        LBL_OFFSET(PhxLblInstrumentState_typ, ElectronicsTempC.Valid),
        LBL_NO_RETURN,  LBL_SIZE(ElectronicsTempC.Value)},

    {"ELECTRONICS_TEMP_RAW",      "INT",      LBL_REQUIRED,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(PhxLblInstrumentState_typ, ElectronicsTempRaw.Value),
        LBL_OFFSET(PhxLblInstrumentState_typ, ElectronicsTempRaw.Valid),
        LBL_NO_RETURN,  LBL_SIZE(ElectronicsTempRaw.Value)},

    {"OPTICAL_BENCH_TEMP_C",      "INT",      LBL_REQUIRED,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(PhxLblInstrumentState_typ, OpticalBenchTempC.Value),
        LBL_OFFSET(PhxLblInstrumentState_typ, OpticalBenchTempC.Valid),
        LBL_NO_RETURN,  LBL_SIZE(OpticalBenchTempC.Value)},

    {"OPTICAL_BENCH_TEMP_RAW",      "INT",      LBL_REQUIRED,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(PhxLblInstrumentState_typ, OpticalBenchTempRaw.Value),
        LBL_OFFSET(PhxLblInstrumentState_typ, OpticalBenchTempRaw.Valid),
        LBL_NO_RETURN,  LBL_SIZE(OpticalBenchTempRaw.Value)},

    {"COVER_STATUS",      "INT",      LBL_REQUIRED,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(PhxLblInstrumentState_typ, CoverStatus.Value),
        LBL_OFFSET(PhxLblInstrumentState_typ, CoverStatus.Valid),
        LBL_NO_RETURN,  LBL_SIZE(CoverStatus.Value)},

    {"LAMP_STATE",      "INT",      LBL_REQUIRED,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(PhxLblInstrumentState_typ, LampState.Value),
        LBL_OFFSET(PhxLblInstrumentState_typ, LampState.Valid),
        LBL_NO_RETURN,  LBL_SIZE(LampState.Value)},

    {"SHUTTER_BIAS",      "INT",      LBL_REQUIRED,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(PhxLblInstrumentState_typ, ShutterBias.Value),
        LBL_OFFSET(PhxLblInstrumentState_typ, ShutterBias.Valid),
        LBL_NO_RETURN,  LBL_SIZE(ShutterBias.Value)},

    {"AUTO_EXPOSURE",      "INT",      LBL_REQUIRED,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(PhxLblInstrumentState_typ, AutoExposure.Value),
        LBL_OFFSET(PhxLblInstrumentState_typ, AutoExposure.Valid),
        LBL_NO_RETURN,  LBL_SIZE(AutoExposure.Value)},

    {"AUTO_EXPOSURE_COUNT",      "INT",      LBL_REQUIRED,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(PhxLblInstrumentState_typ, AutoExposureCount.Value),
        LBL_OFFSET(PhxLblInstrumentState_typ, AutoExposureCount.Valid),
        LBL_NO_RETURN,  LBL_SIZE(AutoExposureCount.Value)},

    {"SUN_FIND",      "INT",      LBL_REQUIRED,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(PhxLblInstrumentState_typ, SunFind.Value),
        LBL_OFFSET(PhxLblInstrumentState_typ, SunFind.Valid),
        LBL_NO_RETURN,  LBL_SIZE(SunFind.Value)},

    {"SUN_FIND_RESULT",      "INT",      LBL_REQUIRED,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(PhxLblInstrumentState_typ, SunFindResult.Value),
        LBL_OFFSET(PhxLblInstrumentState_typ, SunFindResult.Valid),
        LBL_NO_RETURN,  LBL_SIZE(SunFindResult.Value)},

    {"IMAGE_DEC_MODE_X",      "INT",      LBL_REQUIRED,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(PhxLblInstrumentState_typ, ImageDecModeX.Value),
        LBL_OFFSET(PhxLblInstrumentState_typ, ImageDecModeX.Valid),
        LBL_NO_RETURN,  LBL_SIZE(ImageDecModeX.Value)},

    {"IMAGE_DEC_MODE_Y",      "INT",      LBL_REQUIRED,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(PhxLblInstrumentState_typ, ImageDecModeY.Value),
        LBL_OFFSET(PhxLblInstrumentState_typ, ImageDecModeY.Valid),
        LBL_NO_RETURN,  LBL_SIZE(ImageDecModeY.Value)},

    {"CCD_SUB_FRAME_START",      "INT",      LBL_REQUIRED,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(PhxLblInstrumentState_typ, CCDSubFrameStart.Value),
        LBL_OFFSET(PhxLblInstrumentState_typ, CCDSubFrameStart.Valid),
        LBL_NO_RETURN,  LBL_SIZE(CCDSubFrameStart.Value)},

    {"CCD_SUB_FRAME_END",      "INT",      LBL_REQUIRED,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(PhxLblInstrumentState_typ, CCDSubFrameEnd.Value),
        LBL_OFFSET(PhxLblInstrumentState_typ, CCDSubFrameEnd.Valid),
        LBL_NO_RETURN,  LBL_SIZE(CCDSubFrameEnd.Value)},

    {"DOWNSAMPLE_METHOD",      "INT",      LBL_REQUIRED,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(PhxLblInstrumentState_typ, DownsampleMethod.Value),
        LBL_OFFSET(PhxLblInstrumentState_typ, DownsampleMethod.Valid),
        LBL_NO_RETURN,  LBL_SIZE(DownsampleMethod.Value)},

    {"CCD_CONV_OFFSET",      "INT",      LBL_REQUIRED,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(PhxLblInstrumentState_typ, CCDConvOffset.Value),
        LBL_OFFSET(PhxLblInstrumentState_typ, CCDConvOffset.Valid),
        LBL_NO_RETURN,  LBL_SIZE(CCDConvOffset.Value)},

    {"IMAGE_BIAS",      "INT",      LBL_REQUIRED,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(PhxLblInstrumentState_typ, ImageBias.Value),
        LBL_OFFSET(PhxLblInstrumentState_typ, ImageBias.Valid),
        LBL_NO_RETURN,  LBL_SIZE(ImageBias.Value)},

	{0, 0, 0, 0, 0, 0, 0, 0, 0, 0}};

static LblApiProcess_typ	Label = {
	LabelTbl,       "PROPERTY",     "PROPERTY",     "INSTRUMENT_STATE_PARAMS",
	LBL_NULL };

/******************************************************************************
 *				PHX_LBL_INSTRUMENT_STATE
 *
 *****************************************************************************/
int     PhxLblInstrumentState(
  int   Unit,
  int   Obtain,
  PhxLblInstrumentState_typ      *LabelItems,
  int	Instance)
{ int   RtnStatus;
  LblApiCntrl_typ	Cntrl;

  Label.Buffer = (void *)LabelItems;
  Label.BufferSize = sizeof(PhxLblInstrumentState_typ);

  memset(&Cntrl,0,sizeof(LblApiCntrl_typ));
  Cntrl.Instance = Instance;
  Cntrl.FileUnit = Unit;
  Cntrl.Obtain = Obtain;
  Cntrl.ProceedOnError = LBL_TRUE;

  RtnStatus = LblProcessor(&Cntrl, &Label);

  return (RtnStatus);
}

/******************************************************************************
 *				LBL_PRINT_INSTRUMENT_STATE
 *
 *****************************************************************************/
void	PhxLblPrintInstrumentState(
  PhxLblInstrumentState_typ	*LabelItems)
{
  Label.Buffer = (void *)LabelItems;

  PrintLabelElements( &Label );

  return;
}

/******************************************************************************
 *				LBL_TEST_INSTRUMENT_STATE
 *
 *****************************************************************************/
void	PhxLblTestInstrumentState(
  PhxLblInstrumentState_typ	*LabelItems)
{
  Label.Buffer = (void *)LabelItems;

  TestLoadLabelElements( &Label );

  return;
}
