/**  Copyright (c) 1995, California Institute of Technology             **/
/**  U. S. Government sponsorship under NASA contract is acknowledged   **/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#pragma set woff 1032

#include "phx_lbl_image.h"

/******************************************************************************
 *				PHX_LBL_IDENTIFICATION
 *
 *	This module contains routines to help create, read/write and print
 *  an Image property label.  It is part of the MIPL label API package,
 *  using a lower-level label processor to do the real work.  This package
 *  basically defines a table that the lower-level routines use.  The table
 *  is the bridge between how the application access the label elements, and
 *  how the label processor specifies the label components to the VICAR label
 *  Run Time Library (RTL).
 *
 *	The label processor interface structures and routines are defined in
 *  the file "lbl_gen_api.h" (Check the label processor documentation for
 *  how to create APIs like this one).  The application program interface
 *  structures are defined in the file "lbl_identification.h".  The
 *  iphxementation supporting the interface is this module.
 *
 *	The primary routine used by a typical application program is
 *  PhxLblImage.  This routine requires exactly 4 parameters.
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

#define  LBL_SIZE(x)	sizeof(((PhxLblImage_typ *)0)->x)

static LblApiElement_typ	LabelTbl[] = {
    {"FIRST_LINE_SAMPLE",      "INT",      LBL_REQUIRED,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(PhxLblImage_typ, FirstLineSample.Value),
        LBL_OFFSET(PhxLblImage_typ, FirstLineSample.Valid),
        LBL_NO_RETURN,  LBL_SIZE(FirstLineSample.Value)},

    {"FIRST_LINE",      "INT",      LBL_REQUIRED,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(PhxLblImage_typ, FirstLine.Value),
        LBL_OFFSET(PhxLblImage_typ, FirstLine.Valid),
        LBL_NO_RETURN,  LBL_SIZE(FirstLine.Value)},

    {"LINE_SAMPLES",      "INT",      LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(PhxLblImage_typ, LineSamples.Value),
        LBL_OFFSET(PhxLblImage_typ, LineSamples.Valid),
        LBL_NO_RETURN,  LBL_SIZE(LineSamples.Value)},

    {"LINES",      "INT",      LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(PhxLblImage_typ, Lines.Value),
        LBL_OFFSET(PhxLblImage_typ, Lines.Valid),
        LBL_NO_RETURN,  LBL_SIZE(Lines.Value)},

    {"SAMPLE_BIT_MASK",         "STRING",   LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(PhxLblImage_typ, SampleBitMask.Value),
        LBL_OFFSET(PhxLblImage_typ, SampleBitMask.Valid),
        LBL_NO_RETURN,  LBL_SIZE(SampleBitMask.Value)},

    {"INVALID_CONSTANT",            "REAL",     LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(PhxLblImage_typ, InvalidConstant[0].Value),
        LBL_OFFSET(PhxLblImage_typ, InvalidConstant[0].Valid),
        LBL_NO_RETURN,  LBL_SIZE(InvalidConstant[0].Value)},
        
    {"INVALID_CONSTANT",            "REAL", LBL_OPTIONAL,
        LBL_CONTINUE,   1,  2,  LBL_NULL,
        LBL_OFFSET(PhxLblImage_typ, InvalidConstant[1].Value),
        LBL_OFFSET(PhxLblImage_typ, InvalidConstant[1].Valid),
        LBL_NO_RETURN,  LBL_SIZE(InvalidConstant[1].Value)},
        
    {"INVALID_CONSTANT",            "REAL", LBL_OPTIONAL,
        LBL_CONTINUE,   1,  3,  LBL_NULL,
        LBL_OFFSET(PhxLblImage_typ, InvalidConstant[2].Value),
        LBL_OFFSET(PhxLblImage_typ, InvalidConstant[2].Valid),
        LBL_NO_RETURN,  LBL_SIZE(InvalidConstant[2].Value)},
        
    {"MISSING_CONSTANT",            "REAL",     LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(PhxLblImage_typ, MissingConstant[0].Value),
        LBL_OFFSET(PhxLblImage_typ, MissingConstant[0].Valid),
        LBL_NO_RETURN,  LBL_SIZE(MissingConstant[0].Value)},

    {"MISSING_CONSTANT",                    "REAL",         LBL_OPTIONAL,
        LBL_CONTINUE,   1,      2,      LBL_NULL,
        LBL_OFFSET(PhxLblImage_typ, MissingConstant[1].Value),
        LBL_OFFSET(PhxLblImage_typ, MissingConstant[1].Valid),
        LBL_NO_RETURN,  LBL_SIZE(MissingConstant[1].Value)},

    {"MISSING_CONSTANT",                    "REAL",         LBL_OPTIONAL,
        LBL_CONTINUE,   1,      3,      LBL_NULL,
        LBL_OFFSET(PhxLblImage_typ, MissingConstant[2].Value),
        LBL_OFFSET(PhxLblImage_typ, MissingConstant[2].Valid),
        LBL_NO_RETURN,  LBL_SIZE(MissingConstant[2].Value)},

	{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}};
	
static LblApiProcess_typ	Label = {
	LabelTbl,	"PROPERTY",	"PROPERTY",	"IMAGE_DATA",
	LBL_NULL };

/******************************************************************************
 *				PHX_LBL_IDENTIFIER
 *
 *****************************************************************************/
int     PhxLblImage(
  int   Unit,
  int   Obtain,
  PhxLblImage_typ      *LabelItems,
  int	Instance)
{ int   RtnStatus;
  LblApiCntrl_typ	Cntrl;

  Label.Buffer = (void *)LabelItems;
  Label.BufferSize = sizeof(PhxLblImage_typ);

  memset(&Cntrl,0,sizeof(LblApiCntrl_typ));
  Cntrl.Instance = Instance;
  Cntrl.FileUnit = Unit;
  Cntrl.Obtain = Obtain;
  Cntrl.ProceedOnError = LBL_TRUE;

  RtnStatus = LblProcessor(&Cntrl, &Label);

  return (RtnStatus);
}

/******************************************************************************
 *				LBL_PRINT_IDENTIFIER
 *
 *****************************************************************************/
void     PhxLblPrintImage(
  PhxLblImage_typ	*LabelItems)
{
  Label.Buffer = (void *)LabelItems;

  PrintLabelElements( &Label );

  return;
}

/******************************************************************************
 *				LBL_TEST_IDENTIFIER
 *
 *****************************************************************************/
void     PhxLblTestImage(
  PhxLblImage_typ	*LabelItems)
{
  Label.Buffer = (void *)LabelItems;

  TestLoadLabelElements( &Label );

  return;
}
