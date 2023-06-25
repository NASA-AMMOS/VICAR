/**  Copyright (c) 1995, California Institute of Technology             **/
/**  U. S. Government sponsorship under NASA contract is acknowledged   **/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "lbl_video_parms.h"

/******************************************************************************
 *				LBL_VIDEO_PARMS
 *
 *	This module contains routines to help create, read/write and print
 *  an Video Parameter property label.  It is part of the MIPL label API package,
 *  using a lower-level label processor to do the real work.  This package
 *  basically defines a table that the lower-level routines use.  The table
 *  is the bridge between how the application access the label elements, and
 *  how the label processor specifies the label components to the VICAR label
 *  Run Time Library (RTL).
 *
 *	The label processor interface structures and routines are defined in
 *  the file "lbl_gen_api.h" (Check the label processor documentation for
 *  how to create APIs like this one).  The application program interface
 *  structures are defined in the file "lbl_image_data.h".  The
 *  implementation supporting the interface is this module.
 *
 *	The primary routine used by a typical application program is
 *  LblVideoParms.  This routine requires exactly 4 parameters.
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
 *****************************************************************************
 * History
 *========
 * Date         Who             Description
 * ============ =============== =============================================
 * 
 *****************************************************************************/

#define  LBL_SIZE(x)	sizeof(((LblVideoParms_typ *)0)->x)

static LblApiElement_typ	LabelTbl[] = {
    {"GROUP_APPLICABILITY_FLAG",    "STRING", LBL_OPTIONAL,
        LBL_NO_CONT,  1,  1,  LBL_NULL,
        LBL_OFFSET(LblVideoParms_typ, GroupApplicabilityFlag.Value),
        LBL_OFFSET(LblVideoParms_typ, GroupApplicabilityFlag.Valid),
        LBL_NO_RETURN,  LBL_SIZE(GroupApplicabilityFlag.Value)},

	  {"FRAME_RATE",                     "REAL",     LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(LblVideoParms_typ, FrameRate.Value),
        LBL_OFFSET(LblVideoParms_typ, FrameRate.Valid),
        LBL_NO_RETURN,  LBL_SIZE(FrameRate.Value)},

    {"FRAME_RATE__UNIT",              "STRING",     LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(LblVideoParms_typ, FrameRateUnit.Value),
        LBL_OFFSET(LblVideoParms_typ, FrameRateUnit.Valid),
        LBL_NO_RETURN,  LBL_SIZE(FrameRateUnit.Value)},

    {"FRAME_TIME",                     "REAL",     LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(LblVideoParms_typ, FrameTime.Value),
        LBL_OFFSET(LblVideoParms_typ, FrameTime.Valid),
        LBL_NO_RETURN,  LBL_SIZE(FrameRate.Value)},

    {"FRAME_TIME__UNIT",              "STRING",     LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(LblVideoParms_typ, FrameTimeUnit.Value),
        LBL_OFFSET(LblVideoParms_typ, FrameTimeUnit.Valid),
        LBL_NO_RETURN,  LBL_SIZE(FrameTimeUnit.Value)},

    {"FRAME_INDEX",                    "INT",     LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(LblVideoParms_typ, FrameIndex.Value),
        LBL_OFFSET(LblVideoParms_typ, FrameIndex.Valid),
        LBL_NO_RETURN,  LBL_SIZE(FrameIndex.Value)},

    {"GOP_FRAME_INDEX",                "INT",     LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(LblVideoParms_typ, GOPFrameIndex.Value),
        LBL_OFFSET(LblVideoParms_typ, GOPFrameIndex.Valid),
        LBL_NO_RETURN,  LBL_SIZE(GOPFrameIndex.Value)},

    {"GOP_TOTAL_FRAMES",               "INT",     LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(LblVideoParms_typ, GOPTotalFrames.Value),
        LBL_OFFSET(LblVideoParms_typ, GOPTotalFrames.Valid),
        LBL_NO_RETURN,  LBL_SIZE(GOPTotalFrames.Value)},

    {"GOP_OFFSET",                     "INT",     LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(LblVideoParms_typ, GOPOffset.Value),
        LBL_OFFSET(LblVideoParms_typ, GOPOffset.Valid),
        LBL_NO_RETURN,  LBL_SIZE(GOPOffset.Value)},

    {"GOP_LENGTH",                     "INT",     LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(LblVideoParms_typ, GOPLength.Value),
        LBL_OFFSET(LblVideoParms_typ, GOPLength.Valid),
        LBL_NO_RETURN,  LBL_SIZE(GOPLength.Value)},

	{0, 0, 0, 0, 0, 0, 0, 0, 0, 0}};

static LblApiProcess_typ	Label = {
	LabelTbl,       "PROPERTY",     "PROPERTY",     "VIDEO_PARMS",
	LBL_NULL };

/******************************************************************************
 *				LBL_VIDEO_PARMS
 *
 *****************************************************************************/
int     LblVideoParms(
  int   Unit,
  int   Obtain,
  LblVideoParms_typ      *LabelItems,
  int	Instance)
{ int   RtnStatus;
  LblApiCntrl_typ	Cntrl;

  Label.Buffer = (void *)LabelItems;
  Label.BufferSize = sizeof(LblVideoParms_typ);

  memset(&Cntrl,0,sizeof(LblApiCntrl_typ));
  Cntrl.Instance = Instance;
  Cntrl.FileUnit = Unit;
  Cntrl.Obtain = Obtain;
  Cntrl.ProceedOnError = LBL_TRUE;

  RtnStatus = LblProcessor(&Cntrl, &Label);

  return (RtnStatus);
}

/******************************************************************************
 *				LBL_PRINT_VIDEO_PARMS
 *
 *****************************************************************************/
void	LblPrintVideoParms(
  LblVideoParms_typ	*LabelItems)
{
  Label.Buffer = (void *)LabelItems;

  PrintLabelElements( &Label );

  return;
}

/******************************************************************************
 *				LBL_TEST_VIDEO_PARMS
 *
 *****************************************************************************/
void	LblTestVideoParms(
  LblVideoParms_typ	*LabelItems)
{
  Label.Buffer = (void *)LabelItems;

  TestLoadLabelElements( &Label );

  return;
}
