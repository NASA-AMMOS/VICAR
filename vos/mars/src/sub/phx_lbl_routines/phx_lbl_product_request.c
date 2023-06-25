/**  Copyright (c) 2002, California Institute of Technology             **/
/**  U. S. Government sponsorship under NASA contract is acknowledged   **/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "phx_lbl_product_request.h"

/******************************************************************************
 *				PHX_LBL_<PRODUCT>_REQUEST
 *
 *	This module contains routines to help create, read/write and print
 *  a Request property label.  It is part of the MIPL label API package,
 *  using a lower-level label processor to do the real work.  This
 *  package basically defines a table that the lower-level routines use.
 *  The table is the bridge between how the application access the label
 *  elements, and how the label processor specifies the label components
 *  to the VICAR label Run Time Library (RTL).
 *
 *	The different available <product> Request types are:
 *		Image
 *		RefPixel	(Reference Pixel)
 *		Thumbnail
 *		Subframe
 *		RowSum
 *		ColSum		(Column Sum)
 *		SunFind
 *		Histogram
 *
 *	The primary routine used by a typical application program is
 *  PhxLbl<product>Request.  This routine requires exactly 4 parameters.
 *  All label API routines  have the same four parameters:
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
 *============================================================================
 *
 * History of modifications:
 *
 * Date         who             Description
 * -----------  --------------- ----------------------------------------------
 * 11-Feb-2003  Payam Zamani    Changed InstCmprsMode from STRING to INT
 * ?            Allan Runkle    Original development and release
 *****************************************************************************/

#define  LBL_SIZE(x)	sizeof(((PhxLblProdRequest_typ *)0)->x)


/**************************/
/***  SUBFRAME REQUEST  ***/
/**************************/
static LblApiElement_typ	SB_LabelTbl[] = {
	{"SOURCE_ID",				"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(PhxLblProdRequest_typ, SourceId.Value),
		LBL_OFFSET(PhxLblProdRequest_typ, SourceId.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SourceId.Value)},

	{"GROUP_APPLICABILITY_FLAG",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(PhxLblProdRequest_typ, GroupApplicabilityFlag.Value),
		LBL_OFFSET(PhxLblProdRequest_typ, GroupApplicabilityFlag.Valid),
		LBL_NO_RETURN,	LBL_SIZE(GroupApplicabilityFlag.Value)},

	{"SUBFRAME_TYPE"	,		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(PhxLblProdRequest_typ, SubframeType.Value),
		LBL_OFFSET(PhxLblProdRequest_typ, SubframeType.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SubframeType.Value)},

	{"FIRST_LINE",				"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(PhxLblProdRequest_typ, FirstLine.Value),
                LBL_OFFSET(PhxLblProdRequest_typ, FirstLine.Valid),
                LBL_NO_RETURN,  LBL_SIZE(FirstLine.Value)},

	{"FIRST_LINE_SAMPLE",			"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(PhxLblProdRequest_typ, FirstLineSample.Value),
                LBL_OFFSET(PhxLblProdRequest_typ, FirstLineSample.Valid),
                LBL_NO_RETURN,  LBL_SIZE(FirstLineSample.Value)},

	{"LINES",				"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(PhxLblProdRequest_typ, Lines.Value),
                LBL_OFFSET(PhxLblProdRequest_typ, Lines.Valid),
                LBL_NO_RETURN,  LBL_SIZE(Lines.Value)},

	{"LINE_SAMPLES",			"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(PhxLblProdRequest_typ, LineSamples.Value),
                LBL_OFFSET(PhxLblProdRequest_typ, LineSamples.Valid),
                LBL_NO_RETURN,  LBL_SIZE(LineSamples.Value)},

	{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}};


/***  Table Definitions  ***/

static LblApiProcess_typ	SB_Label = {
	SB_LabelTbl,	"PROPERTY",	"PROPERTY",
	"SUBFRAME_PARMS",	LBL_NULL };

/*
 * SUBFRAME REQUEST
 */
/******************************************************************************
 *				MER_LBL_SUBFRAME_REQUEST
 *
 *****************************************************************************/
int     PhxLblSubframeRequest(
  int   Unit,
  int   Obtain,
  PhxLblProdRequest_typ      *LabelItems,
  int	Instance)
{ int   RtnStatus;
  LblApiCntrl_typ	Cntrl;

  SB_Label.Buffer = (void *)LabelItems;
  SB_Label.BufferSize = sizeof(PhxLblProdRequest_typ);

  memset(&Cntrl,0,sizeof(LblApiCntrl_typ));
  Cntrl.Instance = Instance;
  Cntrl.FileUnit = Unit;
  Cntrl.Obtain = Obtain;
  Cntrl.ProceedOnError = LBL_TRUE;

  RtnStatus = LblProcessor(&Cntrl, &SB_Label);

  return (RtnStatus);
}

/******************************************************************************
 *				LBL_PRINT_SUBFRAME_REQUEST
 *
 *****************************************************************************/
void	PhxLblPrintSubframeRequest(
  PhxLblProdRequest_typ	*LabelItems)
{
  SB_Label.Buffer = (void *)LabelItems;

  PrintLabelElements( &SB_Label );

  return;
}

/******************************************************************************
 *				LBL_TEST_SUBFRAME_REQUEST
 *
 *****************************************************************************/
void	PhxLblTestSubframeRequest(
  PhxLblProdRequest_typ	*LabelItems)
{
  SB_Label.Buffer = (void *)LabelItems;

  TestLoadLabelElements( &SB_Label );

  return;
}

