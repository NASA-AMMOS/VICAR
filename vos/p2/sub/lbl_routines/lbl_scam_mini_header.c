/**  Copyright (c) 2002, California Institute of Technology             **/
/**  U. S. Government sponsorship under NASA contract is acknowledged   **/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "lbl_scam_mini_header.h"

/******************************************************************************
 *				_LBL_SCAM_MINI_HDR
 *
 *	This module contains routines to help create, read/write and print
 *  a ScamMiniHdr property label.  It is part of the MIPL label API
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
 *  LblScamMiniHdr.  This routine requires exactly 4 parameters.
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

#define  LBL_SIZE(x)	sizeof(((LblScamMiniHdr_typ *)0)->x)

static LblApiElement_typ	LabelTbl[] = {
    {"RMI_HEADER_SIZE",		"INT",	LBL_OPTIONAL,
	LBL_NO_CONT,	1,	1,	LBL_NULL,
	LBL_OFFSET(LblScamMiniHdr_typ, RmiHdrSize.Value),
        LBL_OFFSET(LblScamMiniHdr_typ, RmiHdrSize.Valid),
        LBL_NO_RETURN,  LBL_SIZE(RmiHdrSize.Value)},

    {"EXPOSURE_DURATION",		"INT",	LBL_OPTIONAL,
	LBL_NO_CONT,	1,	1,	LBL_NULL,
	LBL_OFFSET(LblScamMiniHdr_typ, ExposureDuration.Value),
        LBL_OFFSET(LblScamMiniHdr_typ, ExposureDuration.Valid),
        LBL_NO_RETURN,  LBL_SIZE(ExposureDuration.Value)},

    {"EXPOSURE_DURATION__UNIT",      "STRING",       LBL_OPTIONAL,
        LBL_NO_CONT,    1,      1,      LBL_NULL,
        LBL_OFFSET(LblScamMiniHdr_typ, ExposureDurationUnit.Value),
        LBL_OFFSET(LblScamMiniHdr_typ, ExposureDurationUnit.Valid),
        LBL_NO_RETURN,  LBL_SIZE(ExposureDurationUnit.Value)},

    {"HDR_ACQUISITION_MODE",		"STRING",	LBL_OPTIONAL,
	LBL_NO_CONT,	1,	1,	LBL_NULL,
	LBL_OFFSET(LblScamMiniHdr_typ, HdrAcquisitionMode.Value),
        LBL_OFFSET(LblScamMiniHdr_typ, HdrAcquisitionMode.Valid),
        LBL_NO_RETURN,  LBL_SIZE(HdrAcquisitionMode.Value)},

    {"LINES",		"INT",	LBL_OPTIONAL,
	LBL_NO_CONT,	1,	1,	LBL_NULL,
	LBL_OFFSET(LblScamMiniHdr_typ, ROISize.Value),
        LBL_OFFSET(LblScamMiniHdr_typ, ROISize.Valid),
        LBL_NO_RETURN,  LBL_SIZE(ROISize.Value)},

    {"FIRST_LINE",		"INT",	LBL_OPTIONAL,
	LBL_NO_CONT,	1,	1,	LBL_NULL,
	LBL_OFFSET(LblScamMiniHdr_typ, ROIYPosition.Value),
        LBL_OFFSET(LblScamMiniHdr_typ, ROIYPosition.Valid),
        LBL_NO_RETURN,  LBL_SIZE(ROIYPosition.Value)},

    {"MEMORY_BANK",		"INT",	LBL_OPTIONAL,
	LBL_NO_CONT,	1,	1,	LBL_NULL,
	LBL_OFFSET(LblScamMiniHdr_typ, MemoryBank.Value),
        LBL_OFFSET(LblScamMiniHdr_typ, MemoryBank.Valid),
        LBL_NO_RETURN,  LBL_SIZE(MemoryBank.Value)},

    {"AUTO_EXPOSURE_LOWER_THRESHOLD",		"INT",	LBL_OPTIONAL,
	LBL_NO_CONT,	1,	1,	LBL_NULL,
	LBL_OFFSET(LblScamMiniHdr_typ, AutoExposureLowerThreshold.Value),
        LBL_OFFSET(LblScamMiniHdr_typ, AutoExposureLowerThreshold.Valid),
        LBL_NO_RETURN,  LBL_SIZE(AutoExposureLowerThreshold.Value)},

    {"AUTO_EXPOSURE_UPPER_THRESHOLD",		"INT",	LBL_OPTIONAL,
	LBL_NO_CONT,	1,	1,	LBL_NULL,
	LBL_OFFSET(LblScamMiniHdr_typ, AutoExposureUpperThreshold.Value),
        LBL_OFFSET(LblScamMiniHdr_typ, AutoExposureUpperThreshold.Valid),
        LBL_NO_RETURN,  LBL_SIZE(AutoExposureUpperThreshold.Value)},

    {"AUTO_EXPOSURE_LOWER_LIMIT",		"INT",	LBL_OPTIONAL,
	LBL_NO_CONT,	1,	1,	LBL_NULL,
	LBL_OFFSET(LblScamMiniHdr_typ, AutoExposureLowerLimit.Value),
        LBL_OFFSET(LblScamMiniHdr_typ, AutoExposureLowerLimit.Valid),
        LBL_NO_RETURN,  LBL_SIZE(AutoExposureLowerLimit.Value)},

    {"AUTO_EXPOSURE_UPPER_LIMIT",		"INT",	LBL_OPTIONAL,
	LBL_NO_CONT,	1,	1,	LBL_NULL,
	LBL_OFFSET(LblScamMiniHdr_typ, AutoExposureUpperLimit.Value),
        LBL_OFFSET(LblScamMiniHdr_typ, AutoExposureUpperLimit.Valid),
        LBL_NO_RETURN,  LBL_SIZE(AutoExposureUpperLimit.Value)},

    {"AUTO_EXPOSURE_ROI_LINES",		"INT",	LBL_OPTIONAL,
	LBL_NO_CONT,	1,	1,	LBL_NULL,
	LBL_OFFSET(LblScamMiniHdr_typ, AutoExposureROISize.Value),
        LBL_OFFSET(LblScamMiniHdr_typ, AutoExposureROISize.Valid),
        LBL_NO_RETURN,  LBL_SIZE(AutoExposureROISize.Value)},

    {"AUTO_EXPOSURE_ROI_FIRST_LINE",		"INT",	LBL_OPTIONAL,
	LBL_NO_CONT,	1,	1,	LBL_NULL,
	LBL_OFFSET(LblScamMiniHdr_typ, AutoExposureROIYPosition.Value),
        LBL_OFFSET(LblScamMiniHdr_typ, AutoExposureROIYPosition.Valid),
        LBL_NO_RETURN,  LBL_SIZE(AutoExposureROIYPosition.Value)},

    {"HDR_CLIP_THRESHOLD",		"INT",	LBL_OPTIONAL,
	LBL_NO_CONT,	1,	1,	LBL_NULL,
	LBL_OFFSET(LblScamMiniHdr_typ, ClippingThreshold.Value),
        LBL_OFFSET(LblScamMiniHdr_typ, ClippingThreshold.Valid),
        LBL_NO_RETURN,  LBL_SIZE(ClippingThreshold.Value)},

    {"HDR_FRAME_COUNT",		"INT",	LBL_OPTIONAL,
	LBL_NO_CONT,	1,	1,	LBL_NULL,
	LBL_OFFSET(LblScamMiniHdr_typ, FrameNumber.Value),
        LBL_OFFSET(LblScamMiniHdr_typ, FrameNumber.Valid),
        LBL_NO_RETURN,  LBL_SIZE(FrameNumber.Value)},

    {"HDR_EXPOSURE_TIME_DELTA",		"INT",	LBL_OPTIONAL,
	LBL_NO_CONT,	1,	1,	LBL_NULL,
	LBL_OFFSET(LblScamMiniHdr_typ, ExposureStep[0].Value),
        LBL_OFFSET(LblScamMiniHdr_typ, ExposureStep[0].Valid),
        LBL_NO_RETURN,  LBL_SIZE(ExposureStep[0].Value)},

    {"HDR_EXPOSURE_TIME_DELTA",		"INT",	LBL_OPTIONAL,
	LBL_NO_CONT,	1,	2,	LBL_NULL,
	LBL_OFFSET(LblScamMiniHdr_typ, ExposureStep[1].Value),
        LBL_OFFSET(LblScamMiniHdr_typ, ExposureStep[1].Valid),
        LBL_NO_RETURN,  LBL_SIZE(ExposureStep[1].Value)},

    {"HDR_EXPOSURE_TIME_DELTA",		"INT",	LBL_OPTIONAL,
	LBL_NO_CONT,	1,	3,	LBL_NULL,
	LBL_OFFSET(LblScamMiniHdr_typ, ExposureStep[2].Value),
        LBL_OFFSET(LblScamMiniHdr_typ, ExposureStep[2].Valid),
        LBL_NO_RETURN,  LBL_SIZE(ExposureStep[2].Value)},

    {"HDR_EXPOSURE_TIME_DELTA",		"INT",	LBL_OPTIONAL,
	LBL_NO_CONT,	1,	4,	LBL_NULL,
	LBL_OFFSET(LblScamMiniHdr_typ, ExposureStep[3].Value),
        LBL_OFFSET(LblScamMiniHdr_typ, ExposureStep[3].Valid),
        LBL_NO_RETURN,  LBL_SIZE(ExposureStep[3].Value)},

    {"HDR_EXPOSURE_TIME_DELTA",		"INT",	LBL_OPTIONAL,
	LBL_NO_CONT,	1,	5,	LBL_NULL,
	LBL_OFFSET(LblScamMiniHdr_typ, ExposureStep[4].Value),
        LBL_OFFSET(LblScamMiniHdr_typ, ExposureStep[4].Valid),
        LBL_NO_RETURN,  LBL_SIZE(ExposureStep[4].Value)},

    {"HDR_EXPOSURE_TIME_DELTA",		"INT",	LBL_OPTIONAL,
	LBL_NO_CONT,	1,	6,	LBL_NULL,
	LBL_OFFSET(LblScamMiniHdr_typ, ExposureStep[5].Value),
        LBL_OFFSET(LblScamMiniHdr_typ, ExposureStep[5].Valid),
        LBL_NO_RETURN,  LBL_SIZE(ExposureStep[5].Value)},

    {"HDR_EXPOSURE_TIME_DELTA",         "INT",  LBL_OPTIONAL,
        LBL_NO_CONT,    1,      7,      LBL_NULL,
        LBL_OFFSET(LblScamMiniHdr_typ, ExposureStep[6].Value),
        LBL_OFFSET(LblScamMiniHdr_typ, ExposureStep[6].Valid),
        LBL_NO_RETURN,  LBL_SIZE(ExposureStep[6].Value)},

    {"HDR_EXPOSURE_TIME_DELTA__UNIT",         "STRING",  LBL_OPTIONAL,
        LBL_NO_CONT,    1,      1,      LBL_NULL,
        LBL_OFFSET(LblScamMiniHdr_typ, ExposureStepUnit[0].Value),
        LBL_OFFSET(LblScamMiniHdr_typ, ExposureStepUnit[0].Valid),
        LBL_NO_RETURN,  LBL_SIZE(ExposureStepUnit[0].Value)},

    {"HDR_EXPOSURE_TIME_DELTA__UNIT",         "STRING",  LBL_OPTIONAL,
        LBL_NO_CONT,    1,      2,      LBL_NULL,
        LBL_OFFSET(LblScamMiniHdr_typ, ExposureStepUnit[1].Value),
        LBL_OFFSET(LblScamMiniHdr_typ, ExposureStepUnit[1].Valid),
        LBL_NO_RETURN,  LBL_SIZE(ExposureStepUnit[1].Value)},

    {"HDR_EXPOSURE_TIME_DELTA__UNIT",         "STRING",  LBL_OPTIONAL,
        LBL_NO_CONT,    1,      3,      LBL_NULL,
        LBL_OFFSET(LblScamMiniHdr_typ, ExposureStepUnit[2].Value),
        LBL_OFFSET(LblScamMiniHdr_typ, ExposureStepUnit[2].Valid),
        LBL_NO_RETURN,  LBL_SIZE(ExposureStepUnit[2].Value)},

    {"HDR_EXPOSURE_TIME_DELTA__UNIT",         "STRING",  LBL_OPTIONAL,
        LBL_NO_CONT,    1,      4,      LBL_NULL,
        LBL_OFFSET(LblScamMiniHdr_typ, ExposureStepUnit[3].Value),
        LBL_OFFSET(LblScamMiniHdr_typ, ExposureStepUnit[3].Valid),
        LBL_NO_RETURN,  LBL_SIZE(ExposureStepUnit[3].Value)},

    {"HDR_EXPOSURE_TIME_DELTA__UNIT",         "STRING",  LBL_OPTIONAL,
        LBL_NO_CONT,    1,      5,      LBL_NULL,
        LBL_OFFSET(LblScamMiniHdr_typ, ExposureStepUnit[4].Value),
        LBL_OFFSET(LblScamMiniHdr_typ, ExposureStepUnit[4].Valid),
        LBL_NO_RETURN,  LBL_SIZE(ExposureStepUnit[4].Value)},

    {"HDR_EXPOSURE_TIME_DELTA__UNIT",         "STRING",  LBL_OPTIONAL,
        LBL_NO_CONT,    1,      6,      LBL_NULL,
        LBL_OFFSET(LblScamMiniHdr_typ, ExposureStepUnit[5].Value),
        LBL_OFFSET(LblScamMiniHdr_typ, ExposureStepUnit[5].Valid),
        LBL_NO_RETURN,  LBL_SIZE(ExposureStepUnit[5].Value)},

    {"HDR_EXPOSURE_TIME_DELTA__UNIT",         "STRING",  LBL_OPTIONAL,
        LBL_NO_CONT,    1,      7,      LBL_NULL,
        LBL_OFFSET(LblScamMiniHdr_typ, ExposureStepUnit[6].Value),
        LBL_OFFSET(LblScamMiniHdr_typ, ExposureStepUnit[6].Valid),
        LBL_NO_RETURN,  LBL_SIZE(ExposureStepUnit[6].Value)},

    {"RMI_CMOS_REGISTERS", "STRING",   LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(LblScamMiniHdr_typ, RmiCMOSRegisters.Value),
        LBL_OFFSET(LblScamMiniHdr_typ, RmiCMOSRegisters.Valid),
        LBL_NO_RETURN,  LBL_SIZE(RmiCMOSRegisters.Value)},

    {"RMI_FPGA_REGISTERS", "STRING",   LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(LblScamMiniHdr_typ, MUFPGARegisters.Value),
        LBL_OFFSET(LblScamMiniHdr_typ, MUFPGARegisters.Valid),
        LBL_NO_RETURN,  LBL_SIZE(MUFPGARegisters.Value)},

    {"MARKER_PAD",		"INT",	LBL_OPTIONAL,
	LBL_NO_CONT,	1,	1,	LBL_NULL,
	LBL_OFFSET(LblScamMiniHdr_typ, MarkerPad.Value),
        LBL_OFFSET(LblScamMiniHdr_typ, MarkerPad.Valid),
        LBL_NO_RETURN,  LBL_SIZE(MarkerPad.Value)},

    {"RCE_TIME_SYNC",		"INT",	LBL_OPTIONAL,
	LBL_NO_CONT,	1,	1,	LBL_NULL,
	LBL_OFFSET(LblScamMiniHdr_typ, RCETimeSync.Value),
        LBL_OFFSET(LblScamMiniHdr_typ, RCETimeSync.Valid),
        LBL_NO_RETURN,  LBL_SIZE(RCETimeSync.Value)},

    {"MILLISECONDS_COUNT",		"INT",	LBL_OPTIONAL,
	LBL_NO_CONT,	1,	1,	LBL_NULL,
	LBL_OFFSET(LblScamMiniHdr_typ, MillisecondCount.Value),
        LBL_OFFSET(LblScamMiniHdr_typ, MillisecondCount.Valid),
        LBL_NO_RETURN,  LBL_SIZE(MillisecondCount.Value)},

    {"NUM_IMAGES_TRANSMITTED",		"INT",	LBL_OPTIONAL,
	LBL_NO_CONT,	1,	1,	LBL_NULL,
	LBL_OFFSET(LblScamMiniHdr_typ, NVImageCounterID.Value),
        LBL_OFFSET(LblScamMiniHdr_typ, NVImageCounterID.Valid),
        LBL_NO_RETURN,  LBL_SIZE(NVImageCounterID.Value)},

    {"RESERVED_1",		"INT",	LBL_OPTIONAL,
	LBL_NO_CONT,	1,	1,	LBL_NULL,
	LBL_OFFSET(LblScamMiniHdr_typ, Reserved1.Value),
        LBL_OFFSET(LblScamMiniHdr_typ, Reserved1.Valid),
        LBL_NO_RETURN,  LBL_SIZE(Reserved1.Value)},

    {"RESERVED_2",		"INT",	LBL_OPTIONAL,
	LBL_NO_CONT,	1,	1,	LBL_NULL,
	LBL_OFFSET(LblScamMiniHdr_typ, Reserved2.Value),
        LBL_OFFSET(LblScamMiniHdr_typ, Reserved2.Valid),
        LBL_NO_RETURN,  LBL_SIZE(Reserved2.Value)},

    {"DATA_LENGTH",		"INT",	LBL_OPTIONAL,
	LBL_NO_CONT,	1,	1,	LBL_NULL,
	LBL_OFFSET(LblScamMiniHdr_typ, DataLength.Value),
        LBL_OFFSET(LblScamMiniHdr_typ, DataLength.Valid),
        LBL_NO_RETURN,  LBL_SIZE(DataLength.Value)},

	{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}};

static LblApiProcess_typ	Label = {
	LabelTbl,       "PROPERTY",     "PROPERTY",
	"SCAM_MINI_HEADER",	LBL_NULL };

/******************************************************************************
 *				_LBL_SCAM_MINI_HDR
 *
 *****************************************************************************/
int     LblScamMiniHdr(
  int   Unit,
  int   Obtain,
  LblScamMiniHdr_typ      *LabelItems,
  int	Instance)
{ int   RtnStatus;
  LblApiCntrl_typ	Cntrl;

  Label.Buffer = (void *)LabelItems;
  Label.BufferSize = sizeof(LblScamMiniHdr_typ);

  memset(&Cntrl,0,sizeof(LblApiCntrl_typ));
  Cntrl.Instance = Instance;
  Cntrl.FileUnit = Unit;
  Cntrl.Obtain = Obtain;
  Cntrl.ProceedOnError = LBL_TRUE;

  RtnStatus = LblProcessor(&Cntrl, &Label);

  return (RtnStatus);
}

/******************************************************************************
 *				LBL_PRINT_SCAM_MINI_HDR
 *
 *****************************************************************************/
void	LblPrintScamMiniHdr(
  LblScamMiniHdr_typ	*LabelItems)
{
  Label.Buffer = (void *)LabelItems;

  PrintLabelElements( &Label );

  return;
}

/******************************************************************************
 *				LBL_TEST_SCAM_MINI_HDR
 *
 *****************************************************************************/
void	LblTestScamMiniHdr(
  LblScamMiniHdr_typ	*LabelItems)
{
  Label.Buffer = (void *)LabelItems;

  TestLoadLabelElements( &Label );

  return;
}
