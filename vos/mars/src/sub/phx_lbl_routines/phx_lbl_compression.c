/**  Copyright (c) 1995, California Institute of Technology             **/
/**  U. S. Government sponsorship under NASA contract is acknowledged   **/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#pragma set woff 1032

#include "phx_lbl_compression.h"
#include "lbl_compression.h"

/******************************************************************************
 *				PHX_LBL_IDENTIFICATION
 *
 *	This module contains routines to help create, read/write and print
 *  an CompressionParams property label.  It is part of the MIPL label API package,
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
 *  PhxLblCompressionParams.  This routine requires exactly 4 parameters.
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

#define  LBL_SIZE(x)	sizeof(((PhxLblCompressionParams_typ *)0)->x)

static LblApiElement_typ	LabelTbl[] = {
	{"ERROR_PIXELS",			"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, ErrorPixels.Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, ErrorPixels.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ErrorPixels.Value)},

	{"CMPRS_AC_INDEX",			"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsACIdx.Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsACIdx.Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsACIdx.Value)},

	{"CMPRS_DC_INDEX",			"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsDCIdx.Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsDCIdx.Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsDCIdx.Value)},

	{"CMPRS_Q_INDEX",			"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsQIdx.Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsQIdx.Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsQIdx.Value)},

	{"INST_CMPRS_MODE",			"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsMode.Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsMode.Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsMode.Value)},

	{"INST_CMPRS_NAME",			"STRING",	LBL_REQUIRED,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsName.Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsName.Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsName.Value)},

	{"INST_CMPRS_QUALITY",			"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsQuality.Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsQuality.Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsQuality.Value)},

	{"INST_CMPRS_RATE",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsRate.Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsRate.Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsRate.Value)},

	{"INST_CMPRS_RATIO",			"REAL",		LBL_REQUIRED,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsRatio.Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsRatio.Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsRatio.Value)},

	{"INST_CMPRS_SEGMENTS",			"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegments.Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegments.Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegments.Value)},

//-----

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[0].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[0].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[1].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[1].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[2].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[2].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	4,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[3].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[3].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[3].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	5,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[4].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[4].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[4].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	6,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[5].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[5].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[5].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	7,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[6].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[6].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[6].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	8,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[7].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[7].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[7].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	9,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[8].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[8].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[8].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	10,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[9].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[9].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[9].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	11,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[10].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[10].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[10].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	12,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[11].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[11].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[11].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	13,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[12].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[12].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[12].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	14,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[13].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[13].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[13].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	15,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[14].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[14].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[14].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	16,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[15].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[15].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[15].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	17,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[16].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[16].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[16].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	18,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[17].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[17].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[17].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	19,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[18].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[18].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[18].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	20,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[19].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[19].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[19].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	21,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[20].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[20].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[20].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	22,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[21].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[21].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[21].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	23,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[22].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[22].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[22].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	24,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[23].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[23].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[23].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	25,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[24].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[24].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[24].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	26,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[25].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[25].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[25].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	27,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[26].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[26].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[26].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	28,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[27].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[27].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[27].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	29,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[28].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[28].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[28].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	30,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[29].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[29].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[29].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	31,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[30].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[30].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[30].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	32,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[31].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[31].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[31].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	33,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[32].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[32].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[32].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	34,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[33].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[33].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[33].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	35,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[34].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[34].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[34].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	36,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[35].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[35].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[35].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	37,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[36].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[36].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[36].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	38,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[37].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[37].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[37].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	39,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[38].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[38].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[38].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	40,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[39].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[39].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[39].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	41,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[40].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[40].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[40].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	42,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[41].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[41].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[41].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	43,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[42].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[42].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[42].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	44,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[43].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[43].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[43].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	45,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[44].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[44].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[44].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	46,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[45].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[45].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[45].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	47,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[46].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[46].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[46].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	48,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[47].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[47].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[47].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	49,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[48].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[48].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[48].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	50,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[49].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[49].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[49].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	51,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[50].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[50].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[50].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	52,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[51].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[51].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[51].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	53,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[52].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[52].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[52].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	54,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[53].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[53].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[53].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	55,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[54].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[54].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[54].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	56,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[55].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[55].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[55].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	57,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[56].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[56].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[56].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	58,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[57].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[57].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[57].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	59,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[58].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[58].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[58].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	60,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[59].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[59].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[59].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	61,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[60].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[60].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[60].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	62,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[61].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[61].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[61].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	63,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[62].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[62].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[62].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	64,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[63].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[63].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[63].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	65,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[64].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[64].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[64].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	66,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[65].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[65].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[65].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	67,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[66].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[66].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[66].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	68,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[67].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[67].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[67].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	69,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[68].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[68].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[68].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	70,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[69].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[69].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[69].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	71,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[70].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[70].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[70].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	72,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[71].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[71].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[71].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	73,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[72].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[72].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[72].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	74,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[73].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[73].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[73].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	75,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[74].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[74].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[74].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	76,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[75].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[75].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[75].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	77,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[76].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[76].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[76].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	78,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[77].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[77].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[77].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	79,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[78].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[78].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[78].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	80,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[79].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[79].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[79].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	81,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[80].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[80].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[80].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	82,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[81].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[81].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[81].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	83,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[82].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[82].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[82].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	84,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[83].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[83].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[83].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	85,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[84].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[84].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[84].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	86,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[85].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[85].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[85].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	87,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[86].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[86].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[86].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	88,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[87].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[87].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[87].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	89,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[88].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[88].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[88].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	90,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[89].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[89].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[89].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	91,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[90].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[90].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[90].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	92,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[91].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[91].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[91].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	93,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[92].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[92].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[92].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	94,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[93].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[93].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[93].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	95,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[94].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[94].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[94].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	96,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[95].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[95].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[95].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	97,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[96].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[96].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[96].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	98,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[97].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[97].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[97].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	99,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[98].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[98].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[98].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	100,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[99].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[99].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[99].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	101,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[100].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[100].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[100].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	102,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[101].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[101].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[101].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	103,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[102].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[102].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[102].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	104,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[103].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[103].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[103].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	105,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[104].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[104].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[104].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	106,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[105].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[105].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[105].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	107,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[106].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[106].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[106].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	108,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[107].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[107].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[107].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	109,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[108].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[108].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[108].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	110,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[109].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[109].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[109].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	111,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[110].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[110].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[110].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	112,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[111].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[111].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[111].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	113,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[112].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[112].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[112].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	114,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[113].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[113].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[113].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	115,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[114].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[114].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[114].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	116,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[115].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[115].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[115].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	117,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[116].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[116].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[116].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	118,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[117].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[117].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[117].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	119,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[118].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[118].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[118].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	120,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[119].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[119].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[119].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	121,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[120].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[120].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[120].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	122,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[121].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[121].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[121].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	123,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[122].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[122].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[122].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	124,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[123].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[123].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[123].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	125,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[124].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[124].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[124].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	126,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[125].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[125].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[125].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	127,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[126].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[126].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[126].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	128,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[127].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[127].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[127].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	129,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[128].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[128].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[128].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	130,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[129].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[129].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[129].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	131,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[130].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[130].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[130].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	132,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[131].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[131].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[131].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	133,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[132].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[132].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[132].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	134,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[133].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[133].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[133].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	135,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[134].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[134].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[134].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	136,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[135].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[135].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[135].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	137,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[136].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[136].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[136].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	138,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[137].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[137].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[137].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	139,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[138].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[138].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[138].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	140,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[139].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[139].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[139].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	141,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[140].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[140].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[140].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	142,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[141].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[141].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[141].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	143,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[142].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[142].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[142].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	144,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[143].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[143].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[143].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	145,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[144].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[144].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[144].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	146,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[145].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[145].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[145].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	147,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[146].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[146].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[146].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	148,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[147].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[147].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[147].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	149,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[148].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[148].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[148].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	150,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[149].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[149].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[149].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	151,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[150].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[150].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[150].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	152,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[151].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[151].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[151].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	153,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[152].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[152].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[152].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	154,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[153].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[153].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[153].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	155,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[154].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[154].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[154].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	156,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[155].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[155].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[155].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	157,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[156].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[156].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[156].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	158,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[157].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[157].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[157].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	159,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[158].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[158].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[158].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	160,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[159].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[159].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[159].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	161,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[160].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[160].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[160].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	162,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[161].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[161].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[161].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	163,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[162].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[162].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[162].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	164,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[163].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[163].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[163].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	165,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[164].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[164].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[164].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	166,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[165].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[165].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[165].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	167,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[166].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[166].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[166].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	168,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[167].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[167].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[167].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	169,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[168].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[168].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[168].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	170,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[169].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[169].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[169].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	171,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[170].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[170].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[170].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	172,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[171].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[171].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[171].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	173,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[172].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[172].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[172].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	174,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[173].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[173].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[173].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	175,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[174].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[174].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[174].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	176,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[175].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[175].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[175].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	177,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[176].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[176].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[176].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	178,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[177].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[177].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[177].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	179,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[178].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[178].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[178].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	180,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[179].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[179].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[179].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	181,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[180].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[180].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[180].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	182,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[181].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[181].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[181].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	183,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[182].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[182].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[182].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	184,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[183].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[183].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[183].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	185,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[184].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[184].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[184].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	186,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[185].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[185].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[185].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	187,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[186].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[186].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[186].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	188,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[187].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[187].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[187].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	189,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[188].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[188].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[188].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	190,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[189].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[189].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[189].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	191,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[190].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[190].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[190].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	192,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[191].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[191].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[191].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	193,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[192].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[192].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[192].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	194,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[193].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[193].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[193].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	195,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[194].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[194].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[194].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	196,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[195].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[195].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[195].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	197,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[196].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[196].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[196].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	198,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[197].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[197].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[197].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	199,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[198].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[198].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[198].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	200,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[199].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[199].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[199].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	201,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[200].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[200].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[200].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	202,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[201].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[201].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[201].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	203,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[202].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[202].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[202].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	204,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[203].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[203].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[203].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	205,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[204].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[204].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[204].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	206,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[205].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[205].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[205].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	207,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[206].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[206].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[206].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	208,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[207].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[207].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[207].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	209,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[208].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[208].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[208].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	210,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[209].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[209].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[209].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	211,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[210].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[210].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[210].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	212,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[211].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[211].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[211].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	213,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[212].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[212].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[212].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	214,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[213].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[213].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[213].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	215,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[214].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[214].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[214].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	216,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[215].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[215].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[215].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	217,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[216].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[216].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[216].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	218,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[217].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[217].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[217].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	219,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[218].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[218].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[218].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	220,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[219].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[219].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[219].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	221,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[220].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[220].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[220].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	222,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[221].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[221].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[221].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	223,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[222].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[222].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[222].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	224,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[223].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[223].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[223].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	225,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[224].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[224].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[224].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	226,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[225].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[225].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[225].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	227,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[226].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[226].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[226].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	228,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[227].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[227].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[227].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	229,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[228].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[228].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[228].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	230,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[229].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[229].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[229].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	231,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[230].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[230].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[230].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	232,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[231].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[231].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[231].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	233,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[232].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[232].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[232].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	234,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[233].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[233].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[233].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	235,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[234].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[234].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[234].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	236,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[235].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[235].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[235].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	237,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[236].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[236].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[236].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	238,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[237].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[237].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[237].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	239,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[238].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[238].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[238].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	240,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[239].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[239].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[239].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	241,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[240].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[240].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[240].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	242,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[241].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[241].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[241].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	243,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[242].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[242].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[242].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	244,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[243].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[243].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[243].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	245,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[244].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[244].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[244].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	246,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[245].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[245].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[245].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	247,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[246].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[246].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[246].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	248,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[247].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[247].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[247].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	249,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[248].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[248].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[248].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	250,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[249].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[249].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[249].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	251,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[250].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[250].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[250].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	252,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[251].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[251].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[251].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	253,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[252].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[252].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[252].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	254,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[253].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[253].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[253].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	255,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[254].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[254].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[254].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	256,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[255].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLine[255].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLine[255].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[0].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[0].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[1].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[1].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[2].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[2].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	4,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[3].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[3].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[3].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	5,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[4].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[4].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[4].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	6,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[5].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[5].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[5].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	7,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[6].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[6].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[6].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	8,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[7].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[7].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[7].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	9,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[8].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[8].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[8].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	10,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[9].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[9].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[9].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	11,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[10].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[10].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[10].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	12,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[11].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[11].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[11].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	13,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[12].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[12].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[12].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	14,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[13].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[13].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[13].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	15,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[14].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[14].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[14].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	16,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[15].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[15].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[15].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	17,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[16].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[16].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[16].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	18,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[17].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[17].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[17].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	19,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[18].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[18].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[18].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	20,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[19].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[19].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[19].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	21,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[20].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[20].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[20].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	22,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[21].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[21].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[21].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	23,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[22].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[22].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[22].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	24,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[23].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[23].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[23].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	25,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[24].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[24].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[24].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	26,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[25].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[25].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[25].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	27,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[26].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[26].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[26].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	28,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[27].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[27].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[27].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	29,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[28].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[28].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[28].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	30,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[29].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[29].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[29].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	31,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[30].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[30].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[30].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	32,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[31].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[31].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[31].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	33,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[32].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[32].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[32].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	34,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[33].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[33].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[33].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	35,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[34].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[34].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[34].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	36,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[35].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[35].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[35].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	37,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[36].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[36].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[36].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	38,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[37].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[37].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[37].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	39,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[38].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[38].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[38].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	40,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[39].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[39].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[39].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	41,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[40].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[40].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[40].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	42,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[41].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[41].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[41].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	43,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[42].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[42].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[42].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	44,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[43].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[43].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[43].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	45,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[44].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[44].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[44].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	46,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[45].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[45].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[45].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	47,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[46].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[46].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[46].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	48,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[47].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[47].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[47].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	49,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[48].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[48].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[48].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	50,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[49].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[49].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[49].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	51,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[50].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[50].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[50].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	52,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[51].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[51].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[51].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	53,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[52].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[52].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[52].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	54,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[53].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[53].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[53].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	55,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[54].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[54].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[54].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	56,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[55].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[55].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[55].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	57,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[56].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[56].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[56].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	58,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[57].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[57].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[57].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	59,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[58].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[58].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[58].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	60,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[59].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[59].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[59].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	61,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[60].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[60].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[60].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	62,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[61].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[61].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[61].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	63,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[62].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[62].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[62].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	64,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[63].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[63].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[63].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	65,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[64].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[64].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[64].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	66,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[65].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[65].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[65].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	67,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[66].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[66].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[66].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	68,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[67].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[67].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[67].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	69,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[68].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[68].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[68].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	70,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[69].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[69].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[69].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	71,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[70].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[70].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[70].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	72,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[71].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[71].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[71].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	73,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[72].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[72].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[72].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	74,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[73].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[73].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[73].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	75,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[74].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[74].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[74].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	76,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[75].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[75].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[75].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	77,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[76].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[76].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[76].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	78,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[77].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[77].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[77].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	79,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[78].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[78].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[78].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	80,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[79].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[79].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[79].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	81,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[80].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[80].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[80].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	82,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[81].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[81].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[81].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	83,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[82].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[82].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[82].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	84,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[83].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[83].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[83].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	85,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[84].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[84].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[84].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	86,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[85].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[85].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[85].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	87,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[86].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[86].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[86].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	88,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[87].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[87].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[87].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	89,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[88].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[88].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[88].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	90,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[89].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[89].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[89].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	91,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[90].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[90].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[90].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	92,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[91].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[91].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[91].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	93,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[92].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[92].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[92].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	94,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[93].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[93].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[93].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	95,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[94].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[94].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[94].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	96,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[95].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[95].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[95].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	97,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[96].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[96].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[96].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	98,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[97].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[97].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[97].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	99,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[98].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[98].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[98].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	100,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[99].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[99].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[99].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	101,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[100].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[100].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[100].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	102,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[101].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[101].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[101].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	103,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[102].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[102].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[102].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	104,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[103].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[103].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[103].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	105,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[104].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[104].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[104].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	106,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[105].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[105].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[105].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	107,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[106].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[106].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[106].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	108,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[107].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[107].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[107].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	109,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[108].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[108].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[108].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	110,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[109].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[109].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[109].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	111,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[110].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[110].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[110].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	112,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[111].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[111].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[111].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	113,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[112].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[112].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[112].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	114,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[113].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[113].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[113].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	115,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[114].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[114].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[114].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	116,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[115].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[115].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[115].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	117,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[116].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[116].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[116].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	118,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[117].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[117].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[117].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	119,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[118].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[118].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[118].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	120,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[119].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[119].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[119].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	121,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[120].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[120].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[120].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	122,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[121].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[121].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[121].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	123,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[122].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[122].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[122].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	124,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[123].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[123].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[123].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	125,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[124].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[124].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[124].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	126,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[125].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[125].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[125].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	127,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[126].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[126].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[126].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	128,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[127].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[127].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[127].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	129,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[128].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[128].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[128].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	130,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[129].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[129].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[129].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	131,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[130].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[130].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[130].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	132,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[131].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[131].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[131].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	133,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[132].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[132].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[132].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	134,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[133].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[133].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[133].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	135,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[134].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[134].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[134].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	136,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[135].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[135].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[135].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	137,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[136].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[136].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[136].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	138,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[137].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[137].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[137].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	139,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[138].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[138].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[138].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	140,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[139].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[139].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[139].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	141,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[140].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[140].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[140].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	142,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[141].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[141].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[141].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	143,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[142].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[142].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[142].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	144,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[143].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[143].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[143].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	145,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[144].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[144].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[144].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	146,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[145].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[145].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[145].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	147,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[146].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[146].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[146].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	148,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[147].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[147].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[147].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	149,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[148].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[148].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[148].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	150,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[149].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[149].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[149].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	151,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[150].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[150].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[150].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	152,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[151].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[151].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[151].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	153,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[152].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[152].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[152].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	154,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[153].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[153].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[153].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	155,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[154].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[154].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[154].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	156,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[155].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[155].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[155].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	157,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[156].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[156].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[156].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	158,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[157].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[157].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[157].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	159,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[158].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[158].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[158].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	160,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[159].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[159].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[159].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	161,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[160].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[160].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[160].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	162,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[161].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[161].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[161].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	163,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[162].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[162].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[162].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	164,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[163].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[163].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[163].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	165,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[164].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[164].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[164].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	166,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[165].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[165].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[165].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	167,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[166].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[166].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[166].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	168,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[167].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[167].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[167].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	169,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[168].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[168].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[168].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	170,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[169].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[169].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[169].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	171,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[170].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[170].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[170].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	172,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[171].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[171].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[171].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	173,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[172].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[172].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[172].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	174,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[173].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[173].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[173].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	175,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[174].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[174].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[174].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	176,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[175].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[175].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[175].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	177,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[176].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[176].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[176].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	178,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[177].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[177].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[177].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	179,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[178].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[178].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[178].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	180,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[179].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[179].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[179].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	181,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[180].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[180].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[180].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	182,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[181].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[181].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[181].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	183,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[182].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[182].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[182].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	184,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[183].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[183].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[183].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	185,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[184].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[184].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[184].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	186,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[185].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[185].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[185].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	187,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[186].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[186].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[186].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	188,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[187].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[187].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[187].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	189,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[188].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[188].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[188].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	190,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[189].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[189].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[189].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	191,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[190].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[190].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[190].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	192,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[191].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[191].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[191].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	193,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[192].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[192].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[192].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	194,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[193].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[193].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[193].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	195,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[194].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[194].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[194].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	196,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[195].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[195].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[195].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	197,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[196].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[196].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[196].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	198,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[197].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[197].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[197].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	199,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[198].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[198].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[198].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	200,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[199].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[199].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[199].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	201,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[200].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[200].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[200].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	202,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[201].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[201].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[201].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	203,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[202].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[202].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[202].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	204,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[203].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[203].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[203].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	205,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[204].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[204].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[204].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	206,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[205].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[205].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[205].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	207,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[206].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[206].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[206].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	208,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[207].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[207].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[207].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	209,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[208].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[208].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[208].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	210,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[209].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[209].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[209].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	211,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[210].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[210].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[210].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	212,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[211].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[211].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[211].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	213,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[212].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[212].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[212].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	214,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[213].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[213].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[213].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	215,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[214].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[214].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[214].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	216,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[215].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[215].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[215].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	217,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[216].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[216].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[216].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	218,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[217].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[217].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[217].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	219,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[218].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[218].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[218].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	220,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[219].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[219].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[219].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	221,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[220].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[220].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[220].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	222,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[221].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[221].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[221].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	223,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[222].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[222].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[222].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	224,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[223].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[223].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[223].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	225,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[224].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[224].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[224].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	226,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[225].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[225].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[225].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	227,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[226].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[226].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[226].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	228,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[227].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[227].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[227].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	229,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[228].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[228].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[228].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	230,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[229].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[229].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[229].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	231,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[230].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[230].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[230].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	232,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[231].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[231].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[231].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	233,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[232].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[232].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[232].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	234,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[233].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[233].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[233].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	235,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[234].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[234].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[234].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	236,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[235].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[235].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[235].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	237,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[236].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[236].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[236].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	238,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[237].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[237].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[237].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	239,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[238].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[238].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[238].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	240,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[239].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[239].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[239].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	241,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[240].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[240].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[240].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	242,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[241].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[241].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[241].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	243,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[242].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[242].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[242].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	244,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[243].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[243].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[243].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	245,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[244].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[244].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[244].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	246,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[245].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[245].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[245].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	247,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[246].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[246].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[246].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	248,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[247].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[247].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[247].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	249,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[248].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[248].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[248].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	250,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[249].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[249].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[249].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	251,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[250].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[250].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[250].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	252,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[251].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[251].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[251].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	253,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[252].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[252].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[252].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	254,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[253].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[253].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[253].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	255,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[254].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[254].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[254].Value)},

	{"INST_CMPRS_SEG_FIRST_LINE_SAMP",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	256,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[255].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegFirstLineSamp[255].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegFirstLineSamp[255].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[0].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[0].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[1].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[1].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[2].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[2].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	4,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[3].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[3].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[3].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	5,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[4].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[4].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[4].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	6,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[5].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[5].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[5].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	7,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[6].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[6].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[6].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	8,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[7].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[7].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[7].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	9,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[8].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[8].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[8].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	10,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[9].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[9].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[9].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	11,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[10].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[10].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[10].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	12,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[11].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[11].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[11].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	13,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[12].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[12].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[12].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	14,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[13].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[13].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[13].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	15,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[14].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[14].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[14].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	16,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[15].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[15].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[15].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	17,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[16].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[16].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[16].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	18,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[17].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[17].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[17].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	19,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[18].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[18].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[18].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	20,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[19].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[19].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[19].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	21,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[20].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[20].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[20].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	22,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[21].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[21].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[21].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	23,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[22].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[22].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[22].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	24,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[23].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[23].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[23].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	25,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[24].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[24].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[24].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	26,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[25].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[25].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[25].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	27,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[26].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[26].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[26].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	28,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[27].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[27].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[27].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	29,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[28].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[28].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[28].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	30,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[29].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[29].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[29].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	31,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[30].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[30].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[30].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	32,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[31].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[31].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[31].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	33,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[32].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[32].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[32].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	34,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[33].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[33].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[33].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	35,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[34].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[34].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[34].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	36,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[35].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[35].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[35].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	37,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[36].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[36].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[36].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	38,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[37].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[37].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[37].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	39,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[38].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[38].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[38].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	40,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[39].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[39].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[39].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	41,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[40].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[40].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[40].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	42,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[41].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[41].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[41].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	43,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[42].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[42].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[42].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	44,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[43].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[43].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[43].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	45,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[44].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[44].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[44].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	46,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[45].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[45].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[45].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	47,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[46].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[46].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[46].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	48,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[47].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[47].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[47].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	49,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[48].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[48].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[48].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	50,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[49].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[49].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[49].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	51,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[50].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[50].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[50].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	52,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[51].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[51].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[51].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	53,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[52].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[52].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[52].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	54,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[53].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[53].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[53].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	55,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[54].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[54].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[54].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	56,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[55].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[55].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[55].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	57,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[56].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[56].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[56].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	58,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[57].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[57].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[57].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	59,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[58].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[58].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[58].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	60,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[59].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[59].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[59].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	61,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[60].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[60].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[60].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	62,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[61].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[61].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[61].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	63,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[62].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[62].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[62].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	64,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[63].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[63].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[63].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	65,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[64].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[64].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[64].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	66,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[65].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[65].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[65].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	67,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[66].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[66].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[66].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	68,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[67].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[67].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[67].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	69,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[68].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[68].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[68].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	70,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[69].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[69].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[69].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	71,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[70].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[70].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[70].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	72,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[71].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[71].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[71].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	73,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[72].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[72].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[72].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	74,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[73].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[73].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[73].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	75,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[74].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[74].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[74].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	76,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[75].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[75].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[75].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	77,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[76].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[76].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[76].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	78,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[77].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[77].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[77].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	79,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[78].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[78].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[78].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	80,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[79].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[79].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[79].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	81,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[80].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[80].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[80].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	82,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[81].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[81].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[81].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	83,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[82].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[82].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[82].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	84,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[83].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[83].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[83].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	85,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[84].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[84].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[84].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	86,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[85].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[85].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[85].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	87,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[86].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[86].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[86].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	88,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[87].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[87].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[87].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	89,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[88].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[88].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[88].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	90,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[89].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[89].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[89].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	91,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[90].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[90].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[90].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	92,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[91].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[91].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[91].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	93,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[92].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[92].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[92].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	94,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[93].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[93].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[93].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	95,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[94].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[94].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[94].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	96,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[95].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[95].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[95].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	97,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[96].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[96].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[96].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	98,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[97].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[97].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[97].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	99,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[98].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[98].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[98].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	100,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[99].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[99].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[99].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	101,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[100].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[100].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[100].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	102,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[101].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[101].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[101].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	103,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[102].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[102].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[102].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	104,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[103].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[103].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[103].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	105,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[104].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[104].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[104].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	106,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[105].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[105].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[105].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	107,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[106].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[106].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[106].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	108,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[107].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[107].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[107].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	109,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[108].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[108].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[108].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	110,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[109].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[109].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[109].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	111,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[110].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[110].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[110].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	112,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[111].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[111].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[111].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	113,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[112].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[112].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[112].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	114,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[113].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[113].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[113].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	115,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[114].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[114].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[114].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	116,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[115].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[115].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[115].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	117,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[116].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[116].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[116].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	118,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[117].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[117].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[117].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	119,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[118].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[118].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[118].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	120,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[119].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[119].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[119].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	121,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[120].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[120].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[120].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	122,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[121].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[121].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[121].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	123,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[122].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[122].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[122].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	124,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[123].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[123].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[123].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	125,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[124].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[124].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[124].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	126,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[125].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[125].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[125].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	127,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[126].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[126].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[126].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	128,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[127].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[127].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[127].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	129,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[128].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[128].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[128].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	130,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[129].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[129].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[129].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	131,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[130].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[130].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[130].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	132,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[131].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[131].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[131].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	133,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[132].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[132].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[132].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	134,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[133].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[133].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[133].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	135,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[134].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[134].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[134].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	136,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[135].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[135].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[135].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	137,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[136].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[136].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[136].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	138,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[137].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[137].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[137].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	139,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[138].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[138].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[138].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	140,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[139].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[139].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[139].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	141,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[140].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[140].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[140].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	142,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[141].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[141].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[141].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	143,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[142].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[142].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[142].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	144,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[143].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[143].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[143].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	145,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[144].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[144].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[144].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	146,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[145].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[145].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[145].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	147,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[146].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[146].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[146].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	148,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[147].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[147].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[147].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	149,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[148].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[148].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[148].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	150,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[149].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[149].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[149].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	151,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[150].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[150].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[150].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	152,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[151].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[151].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[151].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	153,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[152].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[152].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[152].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	154,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[153].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[153].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[153].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	155,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[154].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[154].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[154].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	156,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[155].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[155].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[155].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	157,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[156].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[156].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[156].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	158,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[157].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[157].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[157].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	159,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[158].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[158].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[158].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	160,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[159].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[159].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[159].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	161,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[160].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[160].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[160].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	162,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[161].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[161].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[161].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	163,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[162].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[162].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[162].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	164,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[163].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[163].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[163].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	165,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[164].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[164].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[164].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	166,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[165].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[165].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[165].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	167,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[166].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[166].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[166].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	168,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[167].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[167].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[167].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	169,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[168].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[168].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[168].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	170,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[169].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[169].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[169].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	171,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[170].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[170].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[170].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	172,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[171].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[171].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[171].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	173,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[172].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[172].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[172].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	174,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[173].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[173].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[173].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	175,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[174].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[174].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[174].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	176,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[175].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[175].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[175].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	177,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[176].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[176].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[176].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	178,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[177].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[177].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[177].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	179,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[178].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[178].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[178].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	180,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[179].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[179].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[179].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	181,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[180].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[180].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[180].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	182,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[181].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[181].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[181].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	183,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[182].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[182].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[182].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	184,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[183].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[183].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[183].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	185,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[184].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[184].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[184].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	186,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[185].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[185].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[185].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	187,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[186].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[186].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[186].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	188,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[187].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[187].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[187].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	189,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[188].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[188].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[188].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	190,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[189].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[189].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[189].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	191,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[190].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[190].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[190].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	192,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[191].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[191].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[191].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	193,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[192].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[192].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[192].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	194,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[193].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[193].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[193].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	195,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[194].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[194].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[194].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	196,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[195].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[195].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[195].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	197,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[196].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[196].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[196].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	198,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[197].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[197].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[197].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	199,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[198].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[198].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[198].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	200,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[199].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[199].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[199].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	201,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[200].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[200].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[200].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	202,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[201].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[201].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[201].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	203,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[202].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[202].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[202].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	204,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[203].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[203].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[203].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	205,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[204].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[204].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[204].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	206,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[205].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[205].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[205].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	207,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[206].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[206].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[206].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	208,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[207].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[207].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[207].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	209,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[208].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[208].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[208].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	210,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[209].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[209].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[209].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	211,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[210].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[210].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[210].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	212,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[211].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[211].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[211].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	213,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[212].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[212].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[212].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	214,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[213].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[213].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[213].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	215,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[214].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[214].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[214].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	216,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[215].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[215].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[215].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	217,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[216].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[216].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[216].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	218,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[217].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[217].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[217].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	219,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[218].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[218].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[218].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	220,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[219].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[219].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[219].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	221,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[220].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[220].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[220].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	222,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[221].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[221].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[221].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	223,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[222].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[222].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[222].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	224,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[223].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[223].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[223].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	225,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[224].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[224].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[224].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	226,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[225].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[225].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[225].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	227,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[226].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[226].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[226].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	228,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[227].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[227].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[227].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	229,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[228].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[228].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[228].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	230,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[229].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[229].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[229].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	231,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[230].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[230].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[230].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	232,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[231].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[231].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[231].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	233,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[232].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[232].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[232].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	234,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[233].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[233].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[233].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	235,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[234].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[234].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[234].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	236,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[235].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[235].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[235].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	237,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[236].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[236].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[236].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	238,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[237].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[237].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[237].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	239,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[238].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[238].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[238].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	240,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[239].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[239].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[239].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	241,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[240].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[240].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[240].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	242,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[241].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[241].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[241].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	243,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[242].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[242].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[242].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	244,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[243].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[243].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[243].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	245,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[244].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[244].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[244].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	246,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[245].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[245].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[245].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	247,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[246].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[246].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[246].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	248,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[247].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[247].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[247].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	249,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[248].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[248].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[248].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	250,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[249].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[249].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[249].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	251,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[250].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[250].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[250].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	252,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[251].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[251].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[251].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	253,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[252].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[252].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[252].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	254,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[253].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[253].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[253].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	255,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[254].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[254].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[254].Value)},

	{"INST_CMPRS_SEG_LINES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	256,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[255].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegLines[255].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegLines[255].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[0].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[0].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[1].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[1].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[2].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[2].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	4,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[3].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[3].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[3].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	5,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[4].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[4].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[4].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	6,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[5].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[5].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[5].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	7,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[6].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[6].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[6].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	8,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[7].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[7].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[7].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	9,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[8].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[8].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[8].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	10,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[9].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[9].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[9].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	11,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[10].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[10].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[10].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	12,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[11].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[11].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[11].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	13,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[12].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[12].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[12].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	14,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[13].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[13].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[13].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	15,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[14].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[14].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[14].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	16,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[15].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[15].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[15].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	17,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[16].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[16].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[16].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	18,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[17].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[17].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[17].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	19,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[18].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[18].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[18].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	20,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[19].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[19].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[19].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	21,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[20].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[20].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[20].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	22,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[21].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[21].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[21].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	23,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[22].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[22].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[22].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	24,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[23].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[23].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[23].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	25,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[24].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[24].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[24].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	26,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[25].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[25].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[25].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	27,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[26].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[26].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[26].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	28,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[27].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[27].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[27].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	29,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[28].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[28].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[28].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	30,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[29].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[29].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[29].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	31,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[30].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[30].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[30].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	32,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[31].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[31].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[31].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	33,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[32].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[32].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[32].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	34,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[33].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[33].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[33].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	35,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[34].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[34].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[34].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	36,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[35].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[35].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[35].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	37,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[36].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[36].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[36].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	38,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[37].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[37].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[37].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	39,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[38].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[38].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[38].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	40,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[39].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[39].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[39].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	41,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[40].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[40].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[40].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	42,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[41].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[41].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[41].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	43,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[42].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[42].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[42].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	44,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[43].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[43].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[43].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	45,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[44].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[44].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[44].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	46,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[45].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[45].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[45].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	47,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[46].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[46].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[46].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	48,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[47].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[47].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[47].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	49,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[48].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[48].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[48].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	50,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[49].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[49].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[49].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	51,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[50].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[50].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[50].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	52,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[51].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[51].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[51].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	53,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[52].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[52].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[52].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	54,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[53].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[53].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[53].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	55,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[54].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[54].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[54].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	56,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[55].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[55].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[55].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	57,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[56].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[56].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[56].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	58,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[57].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[57].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[57].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	59,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[58].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[58].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[58].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	60,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[59].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[59].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[59].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	61,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[60].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[60].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[60].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	62,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[61].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[61].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[61].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	63,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[62].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[62].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[62].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	64,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[63].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[63].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[63].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	65,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[64].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[64].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[64].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	66,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[65].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[65].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[65].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	67,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[66].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[66].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[66].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	68,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[67].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[67].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[67].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	69,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[68].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[68].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[68].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	70,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[69].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[69].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[69].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	71,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[70].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[70].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[70].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	72,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[71].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[71].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[71].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	73,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[72].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[72].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[72].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	74,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[73].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[73].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[73].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	75,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[74].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[74].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[74].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	76,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[75].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[75].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[75].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	77,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[76].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[76].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[76].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	78,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[77].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[77].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[77].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	79,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[78].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[78].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[78].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	80,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[79].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[79].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[79].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	81,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[80].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[80].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[80].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	82,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[81].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[81].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[81].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	83,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[82].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[82].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[82].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	84,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[83].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[83].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[83].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	85,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[84].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[84].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[84].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	86,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[85].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[85].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[85].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	87,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[86].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[86].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[86].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	88,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[87].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[87].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[87].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	89,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[88].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[88].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[88].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	90,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[89].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[89].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[89].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	91,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[90].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[90].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[90].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	92,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[91].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[91].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[91].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	93,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[92].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[92].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[92].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	94,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[93].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[93].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[93].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	95,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[94].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[94].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[94].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	96,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[95].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[95].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[95].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	97,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[96].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[96].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[96].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	98,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[97].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[97].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[97].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	99,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[98].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[98].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[98].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	100,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[99].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[99].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[99].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	101,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[100].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[100].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[100].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	102,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[101].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[101].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[101].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	103,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[102].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[102].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[102].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	104,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[103].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[103].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[103].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	105,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[104].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[104].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[104].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	106,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[105].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[105].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[105].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	107,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[106].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[106].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[106].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	108,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[107].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[107].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[107].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	109,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[108].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[108].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[108].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	110,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[109].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[109].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[109].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	111,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[110].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[110].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[110].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	112,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[111].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[111].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[111].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	113,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[112].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[112].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[112].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	114,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[113].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[113].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[113].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	115,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[114].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[114].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[114].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	116,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[115].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[115].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[115].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	117,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[116].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[116].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[116].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	118,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[117].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[117].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[117].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	119,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[118].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[118].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[118].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	120,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[119].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[119].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[119].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	121,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[120].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[120].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[120].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	122,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[121].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[121].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[121].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	123,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[122].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[122].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[122].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	124,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[123].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[123].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[123].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	125,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[124].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[124].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[124].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	126,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[125].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[125].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[125].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	127,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[126].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[126].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[126].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	128,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[127].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[127].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[127].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	129,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[128].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[128].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[128].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	130,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[129].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[129].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[129].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	131,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[130].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[130].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[130].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	132,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[131].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[131].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[131].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	133,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[132].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[132].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[132].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	134,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[133].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[133].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[133].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	135,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[134].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[134].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[134].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	136,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[135].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[135].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[135].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	137,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[136].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[136].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[136].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	138,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[137].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[137].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[137].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	139,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[138].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[138].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[138].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	140,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[139].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[139].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[139].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	141,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[140].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[140].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[140].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	142,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[141].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[141].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[141].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	143,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[142].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[142].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[142].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	144,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[143].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[143].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[143].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	145,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[144].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[144].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[144].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	146,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[145].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[145].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[145].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	147,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[146].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[146].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[146].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	148,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[147].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[147].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[147].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	149,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[148].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[148].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[148].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	150,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[149].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[149].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[149].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	151,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[150].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[150].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[150].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	152,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[151].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[151].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[151].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	153,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[152].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[152].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[152].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	154,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[153].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[153].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[153].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	155,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[154].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[154].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[154].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	156,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[155].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[155].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[155].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	157,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[156].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[156].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[156].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	158,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[157].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[157].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[157].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	159,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[158].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[158].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[158].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	160,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[159].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[159].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[159].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	161,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[160].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[160].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[160].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	162,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[161].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[161].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[161].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	163,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[162].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[162].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[162].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	164,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[163].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[163].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[163].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	165,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[164].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[164].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[164].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	166,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[165].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[165].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[165].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	167,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[166].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[166].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[166].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	168,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[167].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[167].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[167].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	169,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[168].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[168].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[168].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	170,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[169].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[169].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[169].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	171,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[170].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[170].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[170].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	172,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[171].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[171].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[171].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	173,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[172].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[172].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[172].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	174,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[173].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[173].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[173].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	175,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[174].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[174].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[174].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	176,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[175].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[175].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[175].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	177,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[176].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[176].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[176].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	178,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[177].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[177].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[177].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	179,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[178].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[178].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[178].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	180,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[179].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[179].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[179].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	181,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[180].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[180].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[180].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	182,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[181].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[181].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[181].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	183,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[182].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[182].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[182].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	184,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[183].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[183].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[183].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	185,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[184].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[184].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[184].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	186,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[185].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[185].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[185].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	187,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[186].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[186].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[186].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	188,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[187].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[187].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[187].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	189,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[188].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[188].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[188].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	190,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[189].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[189].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[189].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	191,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[190].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[190].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[190].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	192,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[191].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[191].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[191].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	193,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[192].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[192].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[192].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	194,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[193].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[193].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[193].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	195,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[194].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[194].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[194].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	196,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[195].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[195].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[195].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	197,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[196].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[196].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[196].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	198,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[197].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[197].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[197].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	199,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[198].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[198].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[198].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	200,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[199].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[199].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[199].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	201,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[200].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[200].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[200].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	202,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[201].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[201].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[201].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	203,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[202].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[202].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[202].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	204,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[203].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[203].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[203].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	205,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[204].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[204].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[204].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	206,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[205].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[205].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[205].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	207,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[206].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[206].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[206].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	208,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[207].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[207].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[207].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	209,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[208].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[208].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[208].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	210,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[209].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[209].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[209].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	211,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[210].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[210].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[210].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	212,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[211].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[211].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[211].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	213,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[212].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[212].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[212].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	214,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[213].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[213].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[213].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	215,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[214].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[214].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[214].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	216,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[215].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[215].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[215].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	217,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[216].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[216].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[216].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	218,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[217].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[217].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[217].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	219,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[218].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[218].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[218].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	220,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[219].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[219].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[219].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	221,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[220].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[220].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[220].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	222,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[221].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[221].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[221].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	223,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[222].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[222].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[222].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	224,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[223].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[223].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[223].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	225,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[224].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[224].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[224].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	226,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[225].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[225].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[225].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	227,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[226].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[226].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[226].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	228,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[227].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[227].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[227].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	229,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[228].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[228].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[228].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	230,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[229].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[229].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[229].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	231,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[230].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[230].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[230].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	232,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[231].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[231].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[231].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	233,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[232].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[232].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[232].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	234,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[233].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[233].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[233].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	235,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[234].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[234].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[234].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	236,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[235].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[235].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[235].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	237,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[236].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[236].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[236].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	238,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[237].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[237].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[237].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	239,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[238].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[238].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[238].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	240,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[239].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[239].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[239].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	241,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[240].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[240].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[240].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	242,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[241].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[241].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[241].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	243,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[242].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[242].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[242].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	244,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[243].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[243].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[243].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	245,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[244].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[244].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[244].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	246,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[245].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[245].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[245].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	247,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[246].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[246].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[246].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	248,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[247].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[247].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[247].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	249,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[248].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[248].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[248].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	250,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[249].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[249].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[249].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	251,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[250].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[250].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[250].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	252,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[251].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[251].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[251].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	253,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[252].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[252].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[252].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	254,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[253].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[253].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[253].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	255,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[254].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[254].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[254].Value)},

	{"INST_CMPRS_SEG_SAMPLES",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	256,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[255].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegSamples[255].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegSamples[255].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[0].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[0].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[1].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[1].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[2].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[2].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	4,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[3].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[3].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[3].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	5,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[4].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[4].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[4].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	6,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[5].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[5].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[5].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	7,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[6].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[6].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[6].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	8,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[7].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[7].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[7].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	9,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[8].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[8].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[8].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	10,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[9].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[9].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[9].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	11,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[10].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[10].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[10].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	12,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[11].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[11].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[11].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	13,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[12].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[12].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[12].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	14,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[13].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[13].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[13].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	15,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[14].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[14].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[14].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	16,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[15].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[15].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[15].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	17,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[16].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[16].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[16].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	18,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[17].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[17].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[17].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	19,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[18].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[18].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[18].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	20,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[19].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[19].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[19].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	21,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[20].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[20].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[20].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	22,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[21].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[21].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[21].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	23,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[22].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[22].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[22].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	24,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[23].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[23].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[23].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	25,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[24].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[24].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[24].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	26,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[25].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[25].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[25].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	27,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[26].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[26].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[26].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	28,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[27].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[27].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[27].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	29,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[28].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[28].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[28].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	30,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[29].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[29].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[29].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	31,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[30].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[30].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[30].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	32,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[31].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[31].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[31].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	33,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[32].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[32].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[32].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	34,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[33].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[33].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[33].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	35,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[34].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[34].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[34].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	36,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[35].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[35].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[35].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	37,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[36].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[36].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[36].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	38,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[37].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[37].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[37].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	39,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[38].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[38].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[38].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	40,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[39].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[39].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[39].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	41,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[40].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[40].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[40].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	42,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[41].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[41].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[41].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	43,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[42].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[42].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[42].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	44,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[43].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[43].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[43].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	45,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[44].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[44].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[44].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	46,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[45].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[45].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[45].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	47,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[46].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[46].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[46].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	48,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[47].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[47].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[47].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	49,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[48].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[48].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[48].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	50,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[49].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[49].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[49].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	51,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[50].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[50].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[50].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	52,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[51].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[51].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[51].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	53,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[52].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[52].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[52].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	54,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[53].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[53].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[53].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	55,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[54].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[54].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[54].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	56,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[55].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[55].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[55].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	57,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[56].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[56].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[56].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	58,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[57].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[57].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[57].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	59,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[58].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[58].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[58].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	60,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[59].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[59].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[59].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	61,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[60].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[60].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[60].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	62,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[61].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[61].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[61].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	63,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[62].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[62].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[62].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	64,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[63].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[63].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[63].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	65,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[64].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[64].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[64].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	66,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[65].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[65].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[65].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	67,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[66].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[66].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[66].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	68,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[67].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[67].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[67].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	69,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[68].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[68].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[68].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	70,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[69].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[69].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[69].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	71,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[70].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[70].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[70].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	72,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[71].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[71].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[71].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	73,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[72].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[72].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[72].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	74,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[73].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[73].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[73].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	75,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[74].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[74].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[74].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	76,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[75].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[75].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[75].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	77,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[76].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[76].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[76].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	78,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[77].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[77].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[77].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	79,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[78].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[78].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[78].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	80,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[79].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[79].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[79].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	81,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[80].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[80].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[80].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	82,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[81].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[81].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[81].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	83,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[82].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[82].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[82].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	84,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[83].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[83].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[83].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	85,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[84].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[84].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[84].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	86,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[85].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[85].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[85].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	87,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[86].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[86].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[86].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	88,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[87].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[87].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[87].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	89,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[88].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[88].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[88].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	90,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[89].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[89].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[89].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	91,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[90].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[90].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[90].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	92,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[91].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[91].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[91].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	93,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[92].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[92].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[92].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	94,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[93].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[93].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[93].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	95,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[94].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[94].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[94].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	96,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[95].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[95].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[95].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	97,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[96].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[96].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[96].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	98,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[97].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[97].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[97].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	99,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[98].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[98].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[98].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	100,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[99].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[99].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[99].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	101,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[100].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[100].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[100].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	102,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[101].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[101].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[101].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	103,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[102].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[102].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[102].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	104,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[103].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[103].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[103].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	105,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[104].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[104].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[104].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	106,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[105].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[105].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[105].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	107,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[106].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[106].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[106].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	108,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[107].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[107].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[107].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	109,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[108].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[108].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[108].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	110,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[109].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[109].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[109].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	111,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[110].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[110].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[110].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	112,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[111].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[111].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[111].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	113,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[112].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[112].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[112].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	114,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[113].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[113].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[113].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	115,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[114].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[114].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[114].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	116,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[115].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[115].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[115].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	117,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[116].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[116].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[116].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	118,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[117].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[117].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[117].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	119,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[118].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[118].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[118].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	120,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[119].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[119].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[119].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	121,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[120].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[120].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[120].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	122,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[121].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[121].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[121].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	123,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[122].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[122].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[122].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	124,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[123].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[123].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[123].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	125,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[124].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[124].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[124].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	126,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[125].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[125].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[125].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	127,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[126].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[126].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[126].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	128,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[127].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[127].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[127].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	129,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[128].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[128].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[128].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	130,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[129].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[129].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[129].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	131,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[130].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[130].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[130].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	132,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[131].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[131].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[131].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	133,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[132].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[132].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[132].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	134,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[133].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[133].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[133].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	135,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[134].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[134].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[134].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	136,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[135].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[135].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[135].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	137,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[136].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[136].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[136].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	138,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[137].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[137].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[137].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	139,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[138].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[138].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[138].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	140,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[139].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[139].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[139].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	141,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[140].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[140].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[140].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	142,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[141].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[141].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[141].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	143,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[142].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[142].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[142].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	144,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[143].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[143].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[143].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	145,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[144].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[144].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[144].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	146,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[145].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[145].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[145].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	147,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[146].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[146].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[146].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	148,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[147].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[147].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[147].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	149,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[148].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[148].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[148].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	150,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[149].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[149].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[149].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	151,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[150].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[150].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[150].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	152,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[151].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[151].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[151].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	153,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[152].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[152].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[152].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	154,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[153].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[153].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[153].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	155,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[154].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[154].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[154].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	156,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[155].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[155].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[155].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	157,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[156].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[156].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[156].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	158,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[157].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[157].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[157].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	159,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[158].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[158].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[158].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	160,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[159].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[159].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[159].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	161,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[160].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[160].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[160].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	162,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[161].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[161].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[161].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	163,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[162].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[162].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[162].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	164,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[163].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[163].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[163].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	165,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[164].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[164].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[164].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	166,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[165].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[165].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[165].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	167,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[166].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[166].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[166].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	168,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[167].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[167].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[167].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	169,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[168].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[168].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[168].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	170,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[169].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[169].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[169].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	171,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[170].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[170].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[170].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	172,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[171].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[171].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[171].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	173,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[172].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[172].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[172].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	174,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[173].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[173].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[173].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	175,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[174].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[174].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[174].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	176,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[175].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[175].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[175].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	177,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[176].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[176].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[176].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	178,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[177].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[177].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[177].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	179,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[178].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[178].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[178].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	180,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[179].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[179].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[179].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	181,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[180].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[180].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[180].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	182,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[181].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[181].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[181].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	183,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[182].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[182].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[182].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	184,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[183].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[183].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[183].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	185,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[184].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[184].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[184].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	186,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[185].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[185].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[185].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	187,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[186].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[186].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[186].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	188,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[187].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[187].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[187].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	189,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[188].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[188].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[188].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	190,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[189].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[189].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[189].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	191,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[190].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[190].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[190].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	192,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[191].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[191].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[191].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	193,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[192].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[192].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[192].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	194,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[193].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[193].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[193].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	195,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[194].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[194].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[194].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	196,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[195].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[195].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[195].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	197,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[196].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[196].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[196].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	198,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[197].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[197].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[197].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	199,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[198].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[198].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[198].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	200,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[199].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[199].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[199].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	201,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[200].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[200].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[200].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	202,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[201].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[201].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[201].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	203,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[202].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[202].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[202].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	204,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[203].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[203].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[203].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	205,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[204].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[204].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[204].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	206,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[205].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[205].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[205].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	207,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[206].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[206].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[206].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	208,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[207].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[207].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[207].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	209,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[208].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[208].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[208].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	210,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[209].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[209].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[209].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	211,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[210].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[210].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[210].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	212,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[211].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[211].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[211].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	213,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[212].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[212].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[212].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	214,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[213].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[213].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[213].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	215,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[214].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[214].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[214].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	216,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[215].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[215].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[215].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	217,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[216].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[216].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[216].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	218,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[217].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[217].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[217].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	219,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[218].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[218].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[218].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	220,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[219].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[219].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[219].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	221,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[220].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[220].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[220].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	222,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[221].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[221].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[221].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	223,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[222].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[222].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[222].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	224,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[223].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[223].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[223].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	225,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[224].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[224].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[224].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	226,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[225].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[225].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[225].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	227,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[226].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[226].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[226].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	228,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[227].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[227].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[227].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	229,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[228].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[228].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[228].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	230,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[229].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[229].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[229].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	231,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[230].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[230].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[230].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	232,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[231].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[231].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[231].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	233,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[232].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[232].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[232].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	234,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[233].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[233].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[233].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	235,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[234].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[234].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[234].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	236,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[235].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[235].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[235].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	237,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[236].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[236].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[236].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	238,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[237].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[237].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[237].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	239,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[238].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[238].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[238].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	240,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[239].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[239].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[239].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	241,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[240].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[240].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[240].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	242,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[241].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[241].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[241].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	243,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[242].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[242].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[242].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	244,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[243].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[243].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[243].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	245,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[244].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[244].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[244].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	246,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[245].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[245].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[245].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	247,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[246].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[246].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[246].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	248,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[247].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[247].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[247].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	249,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[248].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[248].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[248].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	250,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[249].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[249].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[249].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	251,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[250].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[250].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[250].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	252,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[251].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[251].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[251].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	253,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[252].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[252].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[252].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	254,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[253].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[253].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[253].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	255,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[254].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[254].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[254].Value)},

	{"INST_CMPRS_SEG_MISSING_PIXELS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	256,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[255].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegMissingPixels[255].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegMissingPixels[255].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[0].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[0].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[1].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[1].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[2].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[2].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	4,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[3].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[3].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[3].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	5,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[4].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[4].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[4].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	6,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[5].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[5].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[5].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	7,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[6].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[6].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[6].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	8,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[7].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[7].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[7].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	9,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[8].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[8].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[8].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	10,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[9].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[9].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[9].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	11,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[10].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[10].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[10].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	12,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[11].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[11].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[11].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	13,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[12].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[12].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[12].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	14,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[13].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[13].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[13].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	15,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[14].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[14].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[14].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	16,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[15].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[15].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[15].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	17,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[16].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[16].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[16].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	18,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[17].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[17].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[17].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	19,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[18].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[18].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[18].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	20,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[19].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[19].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[19].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	21,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[20].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[20].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[20].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	22,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[21].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[21].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[21].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	23,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[22].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[22].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[22].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	24,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[23].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[23].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[23].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	25,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[24].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[24].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[24].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	26,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[25].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[25].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[25].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	27,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[26].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[26].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[26].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	28,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[27].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[27].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[27].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	29,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[28].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[28].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[28].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	30,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[29].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[29].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[29].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	31,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[30].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[30].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[30].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	32,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[31].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[31].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[31].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	33,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[32].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[32].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[32].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	34,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[33].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[33].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[33].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	35,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[34].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[34].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[34].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	36,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[35].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[35].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[35].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	37,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[36].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[36].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[36].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	38,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[37].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[37].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[37].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	39,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[38].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[38].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[38].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	40,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[39].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[39].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[39].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	41,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[40].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[40].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[40].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	42,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[41].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[41].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[41].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	43,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[42].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[42].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[42].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	44,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[43].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[43].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[43].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	45,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[44].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[44].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[44].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	46,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[45].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[45].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[45].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	47,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[46].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[46].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[46].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	48,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[47].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[47].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[47].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	49,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[48].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[48].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[48].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	50,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[49].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[49].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[49].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	51,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[50].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[50].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[50].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	52,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[51].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[51].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[51].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	53,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[52].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[52].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[52].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	54,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[53].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[53].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[53].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	55,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[54].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[54].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[54].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	56,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[55].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[55].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[55].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	57,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[56].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[56].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[56].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	58,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[57].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[57].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[57].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	59,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[58].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[58].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[58].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	60,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[59].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[59].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[59].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	61,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[60].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[60].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[60].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	62,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[61].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[61].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[61].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	63,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[62].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[62].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[62].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	64,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[63].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[63].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[63].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	65,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[64].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[64].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[64].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	66,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[65].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[65].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[65].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	67,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[66].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[66].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[66].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	68,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[67].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[67].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[67].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	69,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[68].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[68].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[68].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	70,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[69].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[69].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[69].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	71,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[70].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[70].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[70].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	72,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[71].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[71].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[71].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	73,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[72].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[72].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[72].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	74,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[73].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[73].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[73].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	75,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[74].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[74].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[74].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	76,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[75].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[75].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[75].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	77,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[76].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[76].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[76].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	78,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[77].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[77].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[77].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	79,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[78].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[78].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[78].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	80,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[79].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[79].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[79].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	81,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[80].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[80].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[80].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	82,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[81].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[81].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[81].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	83,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[82].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[82].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[82].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	84,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[83].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[83].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[83].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	85,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[84].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[84].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[84].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	86,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[85].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[85].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[85].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	87,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[86].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[86].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[86].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	88,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[87].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[87].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[87].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	89,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[88].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[88].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[88].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	90,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[89].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[89].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[89].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	91,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[90].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[90].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[90].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	92,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[91].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[91].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[91].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	93,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[92].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[92].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[92].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	94,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[93].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[93].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[93].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	95,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[94].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[94].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[94].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	96,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[95].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[95].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[95].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	97,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[96].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[96].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[96].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	98,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[97].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[97].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[97].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	99,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[98].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[98].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[98].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	100,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[99].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[99].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[99].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	101,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[100].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[100].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[100].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	102,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[101].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[101].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[101].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	103,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[102].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[102].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[102].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	104,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[103].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[103].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[103].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	105,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[104].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[104].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[104].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	106,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[105].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[105].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[105].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	107,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[106].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[106].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[106].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	108,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[107].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[107].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[107].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	109,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[108].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[108].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[108].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	110,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[109].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[109].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[109].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	111,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[110].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[110].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[110].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	112,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[111].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[111].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[111].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	113,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[112].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[112].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[112].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	114,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[113].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[113].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[113].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	115,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[114].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[114].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[114].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	116,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[115].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[115].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[115].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	117,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[116].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[116].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[116].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	118,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[117].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[117].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[117].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	119,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[118].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[118].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[118].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	120,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[119].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[119].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[119].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	121,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[120].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[120].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[120].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	122,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[121].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[121].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[121].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	123,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[122].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[122].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[122].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	124,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[123].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[123].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[123].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	125,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[124].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[124].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[124].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	126,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[125].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[125].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[125].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	127,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[126].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[126].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[126].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	128,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[127].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[127].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[127].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	129,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[128].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[128].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[128].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	130,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[129].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[129].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[129].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	131,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[130].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[130].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[130].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	132,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[131].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[131].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[131].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	133,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[132].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[132].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[132].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	134,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[133].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[133].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[133].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	135,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[134].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[134].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[134].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	136,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[135].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[135].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[135].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	137,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[136].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[136].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[136].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	138,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[137].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[137].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[137].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	139,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[138].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[138].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[138].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	140,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[139].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[139].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[139].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	141,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[140].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[140].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[140].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	142,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[141].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[141].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[141].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	143,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[142].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[142].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[142].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	144,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[143].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[143].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[143].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	145,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[144].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[144].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[144].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	146,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[145].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[145].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[145].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	147,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[146].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[146].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[146].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	148,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[147].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[147].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[147].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	149,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[148].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[148].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[148].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	150,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[149].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[149].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[149].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	151,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[150].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[150].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[150].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	152,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[151].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[151].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[151].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	153,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[152].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[152].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[152].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	154,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[153].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[153].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[153].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	155,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[154].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[154].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[154].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	156,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[155].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[155].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[155].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	157,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[156].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[156].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[156].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	158,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[157].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[157].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[157].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	159,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[158].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[158].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[158].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	160,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[159].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[159].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[159].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	161,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[160].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[160].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[160].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	162,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[161].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[161].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[161].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	163,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[162].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[162].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[162].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	164,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[163].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[163].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[163].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	165,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[164].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[164].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[164].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	166,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[165].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[165].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[165].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	167,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[166].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[166].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[166].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	168,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[167].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[167].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[167].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	169,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[168].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[168].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[168].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	170,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[169].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[169].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[169].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	171,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[170].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[170].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[170].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	172,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[171].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[171].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[171].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	173,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[172].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[172].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[172].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	174,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[173].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[173].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[173].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	175,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[174].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[174].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[174].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	176,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[175].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[175].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[175].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	177,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[176].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[176].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[176].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	178,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[177].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[177].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[177].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	179,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[178].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[178].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[178].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	180,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[179].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[179].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[179].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	181,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[180].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[180].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[180].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	182,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[181].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[181].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[181].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	183,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[182].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[182].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[182].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	184,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[183].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[183].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[183].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	185,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[184].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[184].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[184].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	186,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[185].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[185].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[185].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	187,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[186].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[186].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[186].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	188,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[187].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[187].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[187].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	189,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[188].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[188].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[188].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	190,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[189].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[189].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[189].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	191,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[190].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[190].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[190].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	192,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[191].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[191].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[191].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	193,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[192].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[192].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[192].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	194,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[193].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[193].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[193].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	195,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[194].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[194].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[194].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	196,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[195].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[195].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[195].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	197,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[196].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[196].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[196].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	198,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[197].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[197].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[197].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	199,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[198].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[198].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[198].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	200,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[199].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[199].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[199].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	201,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[200].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[200].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[200].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	202,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[201].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[201].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[201].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	203,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[202].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[202].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[202].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	204,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[203].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[203].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[203].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	205,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[204].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[204].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[204].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	206,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[205].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[205].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[205].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	207,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[206].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[206].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[206].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	208,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[207].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[207].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[207].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	209,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[208].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[208].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[208].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	210,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[209].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[209].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[209].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	211,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[210].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[210].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[210].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	212,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[211].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[211].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[211].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	213,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[212].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[212].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[212].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	214,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[213].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[213].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[213].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	215,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[214].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[214].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[214].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	216,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[215].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[215].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[215].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	217,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[216].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[216].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[216].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	218,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[217].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[217].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[217].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	219,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[218].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[218].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[218].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	220,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[219].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[219].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[219].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	221,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[220].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[220].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[220].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	222,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[221].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[221].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[221].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	223,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[222].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[222].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[222].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	224,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[223].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[223].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[223].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	225,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[224].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[224].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[224].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	226,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[225].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[225].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[225].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	227,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[226].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[226].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[226].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	228,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[227].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[227].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[227].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	229,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[228].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[228].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[228].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	230,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[229].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[229].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[229].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	231,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[230].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[230].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[230].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	232,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[231].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[231].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[231].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	233,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[232].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[232].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[232].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	234,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[233].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[233].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[233].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	235,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[234].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[234].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[234].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	236,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[235].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[235].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[235].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	237,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[236].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[236].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[236].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	238,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[237].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[237].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[237].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	239,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[238].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[238].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[238].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	240,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[239].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[239].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[239].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	241,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[240].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[240].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[240].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	242,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[241].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[241].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[241].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	243,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[242].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[242].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[242].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	244,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[243].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[243].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[243].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	245,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[244].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[244].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[244].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	246,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[245].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[245].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[245].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	247,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[246].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[246].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[246].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	248,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[247].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[247].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[247].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	249,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[248].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[248].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[248].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	250,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[249].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[249].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[249].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	251,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[250].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[250].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[250].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	252,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[251].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[251].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[251].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	253,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[252].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[252].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[252].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	254,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[253].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[253].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[253].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	255,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[254].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[254].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[254].Value)},

	{"INST_CMPRS_SEGMENT_QUALITY",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	256,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[255].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegQuality[255].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegQuality[255].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[0].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[0].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[0].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	2,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[1].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[1].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[1].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	3,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[2].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[2].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[2].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	4,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[3].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[3].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[3].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	5,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[4].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[4].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[4].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	6,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[5].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[5].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[5].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	7,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[6].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[6].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[6].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	8,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[7].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[7].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[7].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	9,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[8].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[8].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[8].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	10,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[9].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[9].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[9].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	11,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[10].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[10].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[10].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	12,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[11].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[11].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[11].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	13,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[12].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[12].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[12].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	14,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[13].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[13].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[13].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	15,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[14].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[14].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[14].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	16,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[15].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[15].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[15].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	17,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[16].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[16].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[16].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	18,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[17].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[17].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[17].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	19,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[18].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[18].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[18].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	20,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[19].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[19].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[19].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	21,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[20].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[20].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[20].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	22,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[21].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[21].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[21].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	23,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[22].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[22].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[22].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	24,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[23].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[23].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[23].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	25,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[24].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[24].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[24].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	26,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[25].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[25].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[25].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	27,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[26].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[26].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[26].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	28,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[27].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[27].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[27].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	29,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[28].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[28].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[28].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	30,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[29].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[29].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[29].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	31,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[30].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[30].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[30].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	32,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[31].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[31].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[31].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	33,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[32].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[32].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[32].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	34,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[33].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[33].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[33].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	35,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[34].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[34].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[34].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	36,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[35].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[35].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[35].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	37,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[36].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[36].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[36].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	38,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[37].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[37].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[37].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	39,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[38].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[38].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[38].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	40,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[39].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[39].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[39].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	41,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[40].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[40].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[40].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	42,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[41].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[41].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[41].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	43,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[42].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[42].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[42].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	44,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[43].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[43].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[43].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	45,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[44].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[44].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[44].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	46,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[45].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[45].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[45].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	47,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[46].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[46].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[46].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	48,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[47].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[47].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[47].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	49,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[48].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[48].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[48].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	50,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[49].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[49].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[49].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	51,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[50].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[50].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[50].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	52,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[51].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[51].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[51].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	53,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[52].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[52].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[52].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	54,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[53].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[53].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[53].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	55,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[54].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[54].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[54].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	56,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[55].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[55].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[55].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	57,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[56].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[56].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[56].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	58,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[57].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[57].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[57].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	59,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[58].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[58].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[58].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	60,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[59].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[59].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[59].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	61,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[60].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[60].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[60].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	62,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[61].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[61].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[61].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	63,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[62].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[62].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[62].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	64,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[63].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[63].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[63].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	65,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[64].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[64].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[64].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	66,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[65].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[65].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[65].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	67,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[66].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[66].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[66].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	68,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[67].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[67].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[67].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	69,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[68].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[68].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[68].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	70,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[69].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[69].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[69].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	71,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[70].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[70].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[70].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	72,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[71].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[71].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[71].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	73,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[72].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[72].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[72].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	74,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[73].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[73].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[73].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	75,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[74].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[74].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[74].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	76,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[75].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[75].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[75].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	77,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[76].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[76].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[76].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	78,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[77].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[77].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[77].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	79,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[78].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[78].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[78].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	80,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[79].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[79].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[79].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	81,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[80].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[80].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[80].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	82,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[81].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[81].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[81].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	83,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[82].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[82].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[82].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	84,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[83].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[83].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[83].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	85,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[84].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[84].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[84].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	86,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[85].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[85].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[85].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	87,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[86].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[86].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[86].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	88,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[87].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[87].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[87].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	89,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[88].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[88].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[88].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	90,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[89].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[89].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[89].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	91,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[90].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[90].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[90].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	92,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[91].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[91].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[91].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	93,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[92].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[92].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[92].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	94,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[93].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[93].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[93].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	95,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[94].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[94].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[94].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	96,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[95].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[95].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[95].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	97,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[96].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[96].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[96].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	98,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[97].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[97].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[97].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	99,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[98].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[98].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[98].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	100,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[99].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[99].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[99].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	101,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[100].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[100].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[100].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	102,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[101].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[101].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[101].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	103,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[102].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[102].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[102].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	104,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[103].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[103].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[103].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	105,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[104].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[104].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[104].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	106,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[105].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[105].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[105].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	107,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[106].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[106].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[106].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	108,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[107].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[107].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[107].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	109,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[108].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[108].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[108].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	110,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[109].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[109].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[109].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	111,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[110].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[110].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[110].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	112,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[111].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[111].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[111].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	113,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[112].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[112].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[112].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	114,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[113].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[113].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[113].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	115,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[114].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[114].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[114].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	116,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[115].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[115].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[115].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	117,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[116].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[116].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[116].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	118,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[117].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[117].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[117].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	119,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[118].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[118].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[118].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	120,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[119].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[119].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[119].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	121,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[120].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[120].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[120].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	122,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[121].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[121].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[121].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	123,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[122].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[122].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[122].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	124,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[123].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[123].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[123].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	125,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[124].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[124].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[124].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	126,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[125].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[125].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[125].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	127,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[126].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[126].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[126].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	128,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[127].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[127].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[127].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	129,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[128].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[128].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[128].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	130,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[129].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[129].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[129].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	131,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[130].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[130].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[130].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	132,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[131].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[131].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[131].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	133,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[132].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[132].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[132].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	134,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[133].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[133].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[133].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	135,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[134].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[134].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[134].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	136,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[135].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[135].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[135].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	137,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[136].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[136].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[136].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	138,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[137].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[137].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[137].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	139,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[138].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[138].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[138].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	140,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[139].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[139].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[139].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	141,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[140].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[140].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[140].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	142,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[141].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[141].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[141].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	143,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[142].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[142].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[142].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	144,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[143].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[143].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[143].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	145,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[144].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[144].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[144].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	146,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[145].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[145].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[145].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	147,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[146].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[146].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[146].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	148,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[147].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[147].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[147].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	149,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[148].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[148].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[148].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	150,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[149].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[149].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[149].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	151,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[150].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[150].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[150].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	152,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[151].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[151].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[151].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	153,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[152].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[152].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[152].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	154,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[153].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[153].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[153].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	155,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[154].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[154].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[154].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	156,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[155].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[155].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[155].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	157,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[156].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[156].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[156].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	158,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[157].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[157].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[157].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	159,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[158].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[158].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[158].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	160,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[159].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[159].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[159].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	161,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[160].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[160].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[160].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	162,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[161].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[161].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[161].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	163,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[162].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[162].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[162].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	164,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[163].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[163].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[163].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	165,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[164].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[164].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[164].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	166,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[165].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[165].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[165].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	167,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[166].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[166].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[166].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	168,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[167].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[167].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[167].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	169,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[168].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[168].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[168].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	170,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[169].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[169].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[169].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	171,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[170].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[170].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[170].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	172,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[171].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[171].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[171].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	173,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[172].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[172].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[172].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	174,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[173].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[173].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[173].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	175,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[174].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[174].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[174].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	176,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[175].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[175].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[175].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	177,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[176].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[176].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[176].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	178,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[177].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[177].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[177].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	179,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[178].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[178].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[178].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	180,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[179].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[179].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[179].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	181,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[180].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[180].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[180].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	182,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[181].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[181].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[181].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	183,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[182].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[182].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[182].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	184,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[183].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[183].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[183].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	185,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[184].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[184].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[184].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	186,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[185].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[185].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[185].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	187,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[186].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[186].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[186].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	188,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[187].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[187].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[187].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	189,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[188].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[188].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[188].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	190,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[189].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[189].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[189].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	191,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[190].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[190].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[190].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	192,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[191].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[191].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[191].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	193,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[192].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[192].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[192].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	194,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[193].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[193].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[193].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	195,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[194].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[194].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[194].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	196,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[195].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[195].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[195].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	197,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[196].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[196].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[196].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	198,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[197].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[197].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[197].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	199,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[198].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[198].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[198].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	200,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[199].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[199].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[199].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	201,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[200].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[200].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[200].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	202,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[201].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[201].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[201].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	203,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[202].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[202].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[202].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	204,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[203].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[203].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[203].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	205,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[204].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[204].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[204].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	206,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[205].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[205].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[205].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	207,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[206].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[206].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[206].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	208,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[207].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[207].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[207].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	209,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[208].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[208].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[208].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	210,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[209].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[209].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[209].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	211,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[210].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[210].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[210].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	212,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[211].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[211].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[211].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	213,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[212].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[212].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[212].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	214,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[213].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[213].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[213].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	215,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[214].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[214].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[214].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	216,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[215].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[215].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[215].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	217,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[216].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[216].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[216].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	218,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[217].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[217].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[217].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	219,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[218].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[218].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[218].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	220,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[219].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[219].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[219].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	221,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[220].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[220].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[220].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	222,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[221].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[221].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[221].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	223,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[222].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[222].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[222].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	224,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[223].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[223].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[223].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	225,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[224].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[224].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[224].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	226,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[225].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[225].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[225].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	227,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[226].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[226].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[226].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	228,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[227].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[227].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[227].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	229,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[228].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[228].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[228].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	230,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[229].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[229].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[229].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	231,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[230].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[230].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[230].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	232,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[231].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[231].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[231].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	233,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[232].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[232].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[232].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	234,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[233].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[233].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[233].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	235,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[234].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[234].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[234].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	236,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[235].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[235].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[235].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	237,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[236].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[236].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[236].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	238,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[237].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[237].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[237].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	239,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[238].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[238].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[238].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	240,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[239].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[239].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[239].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	241,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[240].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[240].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[240].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	242,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[241].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[241].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[241].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	243,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[242].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[242].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[242].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	244,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[243].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[243].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[243].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	245,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[244].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[244].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[244].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	246,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[245].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[245].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[245].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	247,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[246].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[246].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[246].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	248,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[247].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[247].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[247].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	249,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[248].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[248].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[248].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	250,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[249].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[249].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[249].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	251,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[250].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[250].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[250].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	252,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[251].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[251].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[251].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	253,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[252].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[252].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[252].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	254,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[253].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[253].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[253].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	255,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[254].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[254].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[254].Value)},

	{"INST_CMPRS_SEGMENT_STATUS",		"INT",		LBL_OPTIONAL,
		LBL_CONTINUE,	1,	256,	LBL_NULL,
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[255].Value),
		LBL_OFFSET(PhxLblCompressionParams_typ, InstCmprsSegmentStatus[255].Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsSegmentStatus[255].Value)},

	{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}};

   static LblApiProcess_typ	Label = {
	LabelTbl,	"PROPERTY",	"PROPERTY",	"COMPRESSION_PARMS",
	LBL_NULL };
	
/******************************************************************************
 *				PHX_LBL_IDENTIFIER
 *
 *****************************************************************************/
int     PhxLblCompressionParams(
  int   Unit,
  int   Obtain,
  PhxLblCompressionParams_typ      *LabelItems,
  int	Instance)
{ int   RtnStatus;
   LblApiCntrl_typ	Cntrl;
/*
   static LblApiElement_typ    LabelTbl[1804];

   LabelTbl[1].KeywordList = "ERROR_PIXELS";
   LabelTbl[1].Format = "INT";
   LabelTbl[1].Required = LBL_OPTIONAL;
   LabelTbl[1].Continuation =  LBL_NO_CONT;
   LabelTbl[1].MaxElements = 1;
   LabelTbl[1].Element = 1;
   LabelTbl[1].KeywordUsed = LBL_NULL;
   LabelTbl[1].ValueOffset = LBL_OFFSET(PhxLblCompressionParams_typ,ErrorPixels.Value);
   LabelTbl[1].ValidOffset = LBL_OFFSET(PhxLblCompressionParams_typ,ErrorPixels.Valid);
   LabelTbl[1].RtnElementOffset = LBL_NO_RETURN;
   LabelTbl[1].MemoryAllocated =  LBL_SIZE(ErrorPixels.Value);

   LabelTbl[2].KeywordList = "COMP_AC_IDX";
   LabelTbl[2].Format = "INT";
   LabelTbl[2].Required = LBL_OPTIONAL;
   LabelTbl[2].Continuation =  LBL_NO_CONT;
   LabelTbl[2].MaxElements = 1;
   LabelTbl[2].Element = 1;
   LabelTbl[2].KeywordUsed = LBL_NULL;
   LabelTbl[2].ValueOffset = LBL_OFFSET(PhxLblCompressionParams_typ,InstCmprsACIdx.Value);
   LabelTbl[2].ValidOffset = LBL_OFFSET(PhxLblCompressionParams_typ,InstCmprsACIdx.Valid);
   LabelTbl[2].RtnElementOffset = LBL_NO_RETURN;
   LabelTbl[2].MemoryAllocated =  LBL_SIZE(InstCmprsACIdx.Value);

   LabelTbl[3].KeywordList = "COMP_DC_IDX";
   LabelTbl[3].Format = "INT";
   LabelTbl[3].Required = LBL_OPTIONAL;
   LabelTbl[3].Continuation =  LBL_NO_CONT;
   LabelTbl[3].MaxElements = 1;
   LabelTbl[3].Element = 1;
   LabelTbl[3].KeywordUsed = LBL_NULL;
   LabelTbl[3].ValueOffset = LBL_OFFSET(PhxLblCompressionParams_typ,InstCmprsDCIdx.Value);
   LabelTbl[3].ValidOffset = LBL_OFFSET(PhxLblCompressionParams_typ,InstCmprsDCIdx.Valid);
   LabelTbl[3].RtnElementOffset = LBL_NO_RETURN;
   LabelTbl[3].MemoryAllocated =  LBL_SIZE(InstCmprsDCIdx.Value);

   LabelTbl[4].KeywordList = "COMP_Q_IDX";
   LabelTbl[4].Format = "INT";
   LabelTbl[4].Required = LBL_OPTIONAL;
   LabelTbl[4].Continuation =  LBL_NO_CONT;
   LabelTbl[4].MaxElements = 1;
   LabelTbl[4].Element = 1;
   LabelTbl[4].KeywordUsed = LBL_NULL;
   LabelTbl[4].ValueOffset = LBL_OFFSET(PhxLblCompressionParams_typ,InstCmprsQIdx.Value);
   LabelTbl[4].ValidOffset = LBL_OFFSET(PhxLblCompressionParams_typ,InstCmprsQIdx.Valid);
   LabelTbl[4].RtnElementOffset = LBL_NO_RETURN;
   LabelTbl[4].MemoryAllocated =  LBL_SIZE(InstCmprsQIdx.Value);

   LabelTbl[5].KeywordList = "INST_CMPRS_MODE";
   LabelTbl[5].Format = "INT";
   LabelTbl[5].Required = LBL_OPTIONAL;
   LabelTbl[5].Continuation =  LBL_NO_CONT;
   LabelTbl[5].MaxElements = 1;
   LabelTbl[5].Element = 1;
   LabelTbl[5].KeywordUsed = LBL_NULL;
   LabelTbl[5].ValueOffset = LBL_OFFSET(PhxLblCompressionParams_typ,InstCmprsMode.Value);
   LabelTbl[5].ValidOffset = LBL_OFFSET(PhxLblCompressionParams_typ,InstCmprsMode.Valid);
   LabelTbl[5].RtnElementOffset = LBL_NO_RETURN;
   LabelTbl[5].MemoryAllocated =  LBL_SIZE(InstCmprsMode.Value);

   LabelTbl[6].KeywordList = "INST_CMPRS_NAME";
   LabelTbl[6].Format = "STRING";
   LabelTbl[6].Required = LBL_OPTIONAL;
   LabelTbl[6].Continuation =  LBL_NO_CONT;
   LabelTbl[6].MaxElements = 1;
   LabelTbl[6].Element = 1;
   LabelTbl[6].KeywordUsed = LBL_NULL;
   LabelTbl[6].ValueOffset = LBL_OFFSET(PhxLblCompressionParams_typ,InstCmprsQIdx.Value);
   LabelTbl[6].ValidOffset = LBL_OFFSET(PhxLblCompressionParams_typ,InstCmprsQIdx.Valid);
   LabelTbl[6].RtnElementOffset = LBL_NO_RETURN;
   LabelTbl[6].MemoryAllocated =  LBL_SIZE(InstCmprsQIdx.Value);

   LabelTbl[7].KeywordList = "INST_CMPRS_QUALITY";
   LabelTbl[7].Format = "INT";
   LabelTbl[7].Required = LBL_OPTIONAL;
   LabelTbl[7].Continuation =  LBL_NO_CONT;
   LabelTbl[7].MaxElements = 1;
   LabelTbl[7].Element = 1;
   LabelTbl[7].KeywordUsed = LBL_NULL;
   LabelTbl[7].ValueOffset = LBL_OFFSET(PhxLblCompressionParams_typ,InstCmprsQuality.Value);
   LabelTbl[7].ValidOffset = LBL_OFFSET(PhxLblCompressionParams_typ,InstCmprsQuality.Valid);
   LabelTbl[7].RtnElementOffset = LBL_NO_RETURN;
   LabelTbl[7].MemoryAllocated =  LBL_SIZE(InstCmprsQuality.Value);

   LabelTbl[8].KeywordList = "INST_CMPRS_RATE";
   LabelTbl[8].Format = "REAL";
   LabelTbl[8].Required = LBL_OPTIONAL;
   LabelTbl[8].Continuation =  LBL_NO_CONT;
   LabelTbl[8].MaxElements = 1;
   LabelTbl[8].Element = 1;
   LabelTbl[8].KeywordUsed = LBL_NULL;
   LabelTbl[8].ValueOffset = LBL_OFFSET(PhxLblCompressionParams_typ,InstCmprsRate.Value);
   LabelTbl[8].ValidOffset = LBL_OFFSET(PhxLblCompressionParams_typ,InstCmprsRate.Valid);
   LabelTbl[8].RtnElementOffset = LBL_NO_RETURN;
   LabelTbl[8].MemoryAllocated =  LBL_SIZE(InstCmprsRate.Value);

   LabelTbl[9].KeywordList = "INST_CMPRS_RATIO";
   LabelTbl[9].Format = "REAL";
   LabelTbl[9].Required = LBL_OPTIONAL;
   LabelTbl[9].Continuation =  LBL_NO_CONT;
   LabelTbl[9].MaxElements = 1;
   LabelTbl[9].Element = 1;
   LabelTbl[9].KeywordUsed = LBL_NULL;
   LabelTbl[9].ValueOffset = LBL_OFFSET(PhxLblCompressionParams_typ,InstCmprsRatio.Value);
   LabelTbl[9].ValidOffset = LBL_OFFSET(PhxLblCompressionParams_typ,InstCmprsRatio.Valid);
   LabelTbl[9].RtnElementOffset = LBL_NO_RETURN;
   LabelTbl[9].MemoryAllocated =  LBL_SIZE(InstCmprsRatio.Value);

   LabelTbl[10].KeywordList = "INST_CMPRS_SEGMENTS";
   LabelTbl[10].Format = "INT";
   LabelTbl[10].Required = LBL_OPTIONAL;
   LabelTbl[10].Continuation =  LBL_NO_CONT;
   LabelTbl[10].MaxElements = 1;
   LabelTbl[10].Element = 1;
   LabelTbl[10].KeywordUsed = LBL_NULL;
   LabelTbl[10].ValueOffset = LBL_OFFSET(PhxLblCompressionParams_typ,InstCmprsSegments.Value);
   LabelTbl[10].ValidOffset = LBL_OFFSET(PhxLblCompressionParams_typ,InstCmprsSegments.Valid);
   LabelTbl[10].RtnElementOffset = LBL_NO_RETURN;
   LabelTbl[10].MemoryAllocated =  LBL_SIZE(InstCmprsSegments.Value);

   int i;
   for(i=11;i<267;i++){
      LabelTbl[i].KeywordList = "INST_CMPRS_SEGMENT_QUALITY";
      LabelTbl[i].Format = "REAL";
      LabelTbl[i].Required = LBL_OPTIONAL;
      LabelTbl[i].Continuation =  LBL_NO_CONT;
      LabelTbl[i].MaxElements = 1; 
      LabelTbl[i].Element = i-11+1;
      LabelTbl[i].KeywordUsed = LBL_NULL;
      LabelTbl[i].ValueOffset = LBL_OFFSET(PhxLblCompressionParams_typ,InstCmprsSegQuality[i-11].Value);
      LabelTbl[i].ValidOffset = LBL_OFFSET(PhxLblCompressionParams_typ,InstCmprsSegQuality[i-11].Valid);
      LabelTbl[i].RtnElementOffset = LBL_NO_RETURN;
      LabelTbl[i].MemoryAllocated =  LBL_SIZE(InstCmprsSegQuality[i-11].Value);
  }

  for(i=267;i<523;i++){
      LabelTbl[i].KeywordList = "INST_CMPRS_SEGMENT_STATUS";
      LabelTbl[i].Format = "INT";
      LabelTbl[i].Required = LBL_OPTIONAL;
      LabelTbl[i].Continuation =  LBL_NO_CONT;
      LabelTbl[i].MaxElements = 1;
      LabelTbl[i].Element = i-267+1;
      LabelTbl[i].KeywordUsed = LBL_NULL;
      LabelTbl[i].ValueOffset = LBL_OFFSET(PhxLblCompressionParams_typ,InstCmprsSegmentStatus[i-267].Value);
      LabelTbl[i].ValidOffset = LBL_OFFSET(PhxLblCompressionParams_typ,InstCmprsSegmentStatus[i-267].Valid);
      LabelTbl[i].RtnElementOffset = LBL_NO_RETURN;
      LabelTbl[i].MemoryAllocated =  LBL_SIZE(InstCmprsSegmentStatus[i-267].Value);
  }

  for(i=523;i<779;i++){
      LabelTbl[i].KeywordList = "INST_CMPRS_SEG_FIRST_LINE";
      LabelTbl[i].Format = "INT";
      LabelTbl[i].Required = LBL_OPTIONAL;
      LabelTbl[i].Continuation =  LBL_NO_CONT;
      LabelTbl[i].MaxElements = 1;
      LabelTbl[i].Element = i-523+1;
      LabelTbl[i].KeywordUsed = LBL_NULL;
      LabelTbl[i].ValueOffset = LBL_OFFSET(PhxLblCompressionParams_typ,InstCmprsSegFirstLine[i-523].Value);
      LabelTbl[i].ValidOffset = LBL_OFFSET(PhxLblCompressionParams_typ,InstCmprsSegFirstLine[i-523].Valid);
      LabelTbl[i].RtnElementOffset = LBL_NO_RETURN;
      LabelTbl[i].MemoryAllocated =  LBL_SIZE(InstCmprsSegFirstLine[i-523].Value);
  }

  for(i=779;i<1035;i++){
      LabelTbl[i].KeywordList = "INST_CMPRS_SEG_FIRST_LINE_SAMP";
      LabelTbl[i].Format = "INT";
      LabelTbl[i].Required = LBL_OPTIONAL;
      LabelTbl[i].Continuation =  LBL_NO_CONT;
      LabelTbl[i].MaxElements = 1;
      LabelTbl[i].Element = i-779+1;
      LabelTbl[i].KeywordUsed = LBL_NULL;
      LabelTbl[i].ValueOffset = LBL_OFFSET(PhxLblCompressionParams_typ,InstCmprsSegFirstLineSamp[i-779].Value);
      LabelTbl[i].ValidOffset = LBL_OFFSET(PhxLblCompressionParams_typ,InstCmprsSegFirstLineSamp[i-779].Valid);
      LabelTbl[i].RtnElementOffset = LBL_NO_RETURN;
      LabelTbl[i].MemoryAllocated =  LBL_SIZE(InstCmprsSegFirstLineSamp[i-779].Value);
  }

  for(i=1035;i<1291;i++){
      LabelTbl[i].KeywordList = "INST_CMPRS_SEG_LINES";
      LabelTbl[i].Format = "INT";
      LabelTbl[i].Required = LBL_OPTIONAL;
      LabelTbl[i].Continuation =  LBL_NO_CONT;
      LabelTbl[i].MaxElements = 1;
      LabelTbl[i].Element = i-1035+1;
      LabelTbl[i].KeywordUsed = LBL_NULL;
      LabelTbl[i].ValueOffset = LBL_OFFSET(PhxLblCompressionParams_typ,InstCmprsSegSamples[i-1035].Value);
      LabelTbl[i].ValidOffset = LBL_OFFSET(PhxLblCompressionParams_typ,InstCmprsSegSamples[i-1035].Valid);
      LabelTbl[i].RtnElementOffset = LBL_NO_RETURN;
      LabelTbl[i].MemoryAllocated =  LBL_SIZE(InstCmprsSegSamples[i-1035].Value);
  }

  for(i=1291;i<1547;i++){
      LabelTbl[i].KeywordList = "INST_CMPRS_SEG_SAMPLES";
      LabelTbl[i].Format = "INT";
      LabelTbl[i].Required = LBL_OPTIONAL;
      LabelTbl[i].Continuation =  LBL_NO_CONT;
      LabelTbl[i].MaxElements = 1;
      LabelTbl[i].Element = i-1291+1;
      LabelTbl[i].KeywordUsed = LBL_NULL;
      LabelTbl[i].ValueOffset = LBL_OFFSET(PhxLblCompressionParams_typ,InstCmprsSegSamples[i-1291].Value);
      LabelTbl[i].ValidOffset = LBL_OFFSET(PhxLblCompressionParams_typ,InstCmprsSegSamples[i-1291].Valid);
      LabelTbl[i].RtnElementOffset = LBL_NO_RETURN;
      LabelTbl[i].MemoryAllocated =  LBL_SIZE(InstCmprsSegSamples[i-1291].Value);
  }

  for(i=1547;i<1803;i++){
      LabelTbl[i].KeywordList = "INST_CMPRS_SEG_MISSING_PIXELS";
      LabelTbl[i].Format = "INT";
      LabelTbl[i].Required = LBL_OPTIONAL;
      LabelTbl[i].Continuation =  LBL_NO_CONT;
      LabelTbl[i].MaxElements = 1;
      LabelTbl[i].Element = i-1547+1;
      LabelTbl[i].KeywordUsed = LBL_NULL;
      LabelTbl[i].ValueOffset = LBL_OFFSET(PhxLblCompressionParams_typ,InstCmprsSegSamples[i-1547].Value);
      LabelTbl[i].ValidOffset = LBL_OFFSET(PhxLblCompressionParams_typ,InstCmprsSegSamples[i-1547].Valid);
      LabelTbl[i].RtnElementOffset = LBL_NO_RETURN;
      LabelTbl[i].MemoryAllocated =  LBL_SIZE(InstCmprsSegSamples[i-1547].Value);
  }

   LabelTbl[1803].KeywordList = 0;
   LabelTbl[1803].Format = 0;
   LabelTbl[1803].Required = 0;
   LabelTbl[1803].Continuation = 0;
   LabelTbl[1803].MaxElements = 0;
   LabelTbl[1803].Element = 0;
   LabelTbl[1803].KeywordUsed = 0;
   LabelTbl[1803].ValueOffset = 0;
   LabelTbl[1803].ValidOffset = 0;
   LabelTbl[1803].RtnElementOffset = 0;
   LabelTbl[1803].MemoryAllocated = 0;


  LblApiProcess_typ	Label;
  Label.Table = LabelTbl;
  Label.Type,"PROPERTY";
  Label.NameKeyword,"PROPERTY";
  Label.NameValue,"COMPRESSION_PARAMS";
  //Label.Buffer = LBL_NULL;
*/

  Label.Buffer = (void *)LabelItems;
  //Label.BufferSize = sizeof(PhxLblCompressionParams_typ);
  Label.BufferSize = sizeof(PhxLblCompressionParams_typ);

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
void     PhxLblPrintCompressionParams(
  PhxLblCompressionParams_typ	*LabelItems)
{
/*
   static LblApiElement_typ    LabelTbl[1804];

   LabelTbl[1].KeywordList = "ERROR_PIXELS";
   LabelTbl[1].Format = "INT";
   LabelTbl[1].Required = LBL_OPTIONAL;
   LabelTbl[1].Continuation =  LBL_NO_CONT;
   LabelTbl[1].MaxElements = 1;
   LabelTbl[1].Element = 1;
   LabelTbl[1].KeywordUsed = LBL_NULL;
   LabelTbl[1].ValueOffset = LBL_OFFSET(PhxLblCompressionParams_typ,ErrorPixels.Value);
   LabelTbl[1].ValidOffset = LBL_OFFSET(PhxLblCompressionParams_typ,ErrorPixels.Valid);
   LabelTbl[1].RtnElementOffset = LBL_NO_RETURN;
   LabelTbl[1].MemoryAllocated =  LBL_SIZE(ErrorPixels.Value);

   LabelTbl[2].KeywordList = "COMP_AC_IDX";
   LabelTbl[2].Format = "INT";
   LabelTbl[2].Required = LBL_OPTIONAL;
   LabelTbl[2].Continuation =  LBL_NO_CONT;
   LabelTbl[2].MaxElements = 1;
   LabelTbl[2].Element = 1;
   LabelTbl[2].KeywordUsed = LBL_NULL;
   LabelTbl[2].ValueOffset = LBL_OFFSET(PhxLblCompressionParams_typ,InstCmprsACIdx.Value);
   LabelTbl[2].ValidOffset = LBL_OFFSET(PhxLblCompressionParams_typ,InstCmprsACIdx.Valid);
   LabelTbl[2].RtnElementOffset = LBL_NO_RETURN;
   LabelTbl[2].MemoryAllocated =  LBL_SIZE(InstCmprsACIdx.Value);

   LabelTbl[3].KeywordList = "COMP_DC_IDX";
   LabelTbl[3].Format = "INT";
   LabelTbl[3].Required = LBL_OPTIONAL;
   LabelTbl[3].Continuation =  LBL_NO_CONT;
   LabelTbl[3].MaxElements = 1;
   LabelTbl[3].Element = 1;
   LabelTbl[3].KeywordUsed = LBL_NULL;
   LabelTbl[3].ValueOffset = LBL_OFFSET(PhxLblCompressionParams_typ,InstCmprsDCIdx.Value);
   LabelTbl[3].ValidOffset = LBL_OFFSET(PhxLblCompressionParams_typ,InstCmprsDCIdx.Valid);
   LabelTbl[3].RtnElementOffset = LBL_NO_RETURN;
   LabelTbl[3].MemoryAllocated =  LBL_SIZE(InstCmprsDCIdx.Value);

   LabelTbl[4].KeywordList = "COMP_Q_IDX";
   LabelTbl[4].Format = "INT";
   LabelTbl[4].Required = LBL_OPTIONAL;
   LabelTbl[4].Continuation =  LBL_NO_CONT;
   LabelTbl[4].MaxElements = 1;
   LabelTbl[4].Element = 1;
   LabelTbl[4].KeywordUsed = LBL_NULL;
   LabelTbl[4].ValueOffset = LBL_OFFSET(PhxLblCompressionParams_typ,InstCmprsQIdx.Value);
   LabelTbl[4].ValidOffset = LBL_OFFSET(PhxLblCompressionParams_typ,InstCmprsQIdx.Valid);
   LabelTbl[4].RtnElementOffset = LBL_NO_RETURN;
   LabelTbl[4].MemoryAllocated =  LBL_SIZE(InstCmprsQIdx.Value);

   LabelTbl[5].KeywordList = "INST_CMPRS_MODE";
   LabelTbl[5].Format = "INT";
   LabelTbl[5].Required = LBL_OPTIONAL;
   LabelTbl[5].Continuation =  LBL_NO_CONT;
   LabelTbl[5].MaxElements = 1;
   LabelTbl[5].Element = 1;
   LabelTbl[5].KeywordUsed = LBL_NULL;
   LabelTbl[5].ValueOffset = LBL_OFFSET(PhxLblCompressionParams_typ,InstCmprsMode.Value);
   LabelTbl[5].ValidOffset = LBL_OFFSET(PhxLblCompressionParams_typ,InstCmprsMode.Valid);
   LabelTbl[5].RtnElementOffset = LBL_NO_RETURN;
   LabelTbl[5].MemoryAllocated =  LBL_SIZE(InstCmprsMode.Value);

   LabelTbl[6].KeywordList = "INST_CMPRS_NAME";
   LabelTbl[6].Format = "STRING";
   LabelTbl[6].Required = LBL_OPTIONAL;
   LabelTbl[6].Continuation =  LBL_NO_CONT;
   LabelTbl[6].MaxElements = 1;
   LabelTbl[6].Element = 1;
   LabelTbl[6].KeywordUsed = LBL_NULL;
   LabelTbl[6].ValueOffset = LBL_OFFSET(PhxLblCompressionParams_typ,InstCmprsQIdx.Value);
   LabelTbl[6].ValidOffset = LBL_OFFSET(PhxLblCompressionParams_typ,InstCmprsQIdx.Valid);
   LabelTbl[6].RtnElementOffset = LBL_NO_RETURN;
   LabelTbl[6].MemoryAllocated =  LBL_SIZE(InstCmprsQIdx.Value);

   LabelTbl[7].KeywordList = "INST_CMPRS_QUALITY";
   LabelTbl[7].Format = "INT";
   LabelTbl[7].Required = LBL_OPTIONAL;
   LabelTbl[7].Continuation =  LBL_NO_CONT;
   LabelTbl[7].MaxElements = 1;
   LabelTbl[7].Element = 1;
   LabelTbl[7].KeywordUsed = LBL_NULL;
   LabelTbl[7].ValueOffset = LBL_OFFSET(PhxLblCompressionParams_typ,InstCmprsQuality.Value);
   LabelTbl[7].ValidOffset = LBL_OFFSET(PhxLblCompressionParams_typ,InstCmprsQuality.Valid);
   LabelTbl[7].RtnElementOffset = LBL_NO_RETURN;
   LabelTbl[7].MemoryAllocated =  LBL_SIZE(InstCmprsQuality.Value);

   LabelTbl[8].KeywordList = "INST_CMPRS_RATE";
   LabelTbl[8].Format = "REAL";
   LabelTbl[8].Required = LBL_OPTIONAL;
   LabelTbl[8].Continuation =  LBL_NO_CONT;
   LabelTbl[8].MaxElements = 1;
   LabelTbl[8].Element = 1;
   LabelTbl[8].KeywordUsed = LBL_NULL;
   LabelTbl[8].ValueOffset = LBL_OFFSET(PhxLblCompressionParams_typ,InstCmprsRate.Value);
   LabelTbl[8].ValidOffset = LBL_OFFSET(PhxLblCompressionParams_typ,InstCmprsRate.Valid);
   LabelTbl[8].RtnElementOffset = LBL_NO_RETURN;
   LabelTbl[8].MemoryAllocated =  LBL_SIZE(InstCmprsRate.Value);

   LabelTbl[9].KeywordList = "INST_CMPRS_RATIO";
   LabelTbl[9].Format = "REAL";
   LabelTbl[9].Required = LBL_OPTIONAL;
   LabelTbl[9].Continuation =  LBL_NO_CONT;
   LabelTbl[9].MaxElements = 1;
   LabelTbl[9].Element = 1;
   LabelTbl[9].KeywordUsed = LBL_NULL;
   LabelTbl[9].ValueOffset = LBL_OFFSET(PhxLblCompressionParams_typ,InstCmprsRatio.Value);
   LabelTbl[9].ValidOffset = LBL_OFFSET(PhxLblCompressionParams_typ,InstCmprsRatio.Valid);
   LabelTbl[9].RtnElementOffset = LBL_NO_RETURN;
   LabelTbl[9].MemoryAllocated =  LBL_SIZE(InstCmprsRatio.Value);

   LabelTbl[10].KeywordList = "INST_CMPRS_SEGMENTS";
   LabelTbl[10].Format = "INT";
   LabelTbl[10].Required = LBL_OPTIONAL;
   LabelTbl[10].Continuation =  LBL_NO_CONT;
   LabelTbl[10].MaxElements = 1;
   LabelTbl[10].Element = 1;
   LabelTbl[10].KeywordUsed = LBL_NULL;
   LabelTbl[10].ValueOffset = LBL_OFFSET(PhxLblCompressionParams_typ,InstCmprsSegments.Value);
   LabelTbl[10].ValidOffset = LBL_OFFSET(PhxLblCompressionParams_typ,InstCmprsSegments.Valid);
   LabelTbl[10].RtnElementOffset = LBL_NO_RETURN;
   LabelTbl[10].MemoryAllocated =  LBL_SIZE(InstCmprsSegments.Value);

   int i;
   for(i=11;i<267;i++){
      LabelTbl[i].KeywordList = "INST_CMPRS_SEGMENT_QUALITY";
      LabelTbl[i].Format = "REAL";
      LabelTbl[i].Required = LBL_OPTIONAL;
      LabelTbl[i].Continuation =  LBL_NO_CONT;
      LabelTbl[i].MaxElements = 1;
      LabelTbl[i].Element = 1;
      LabelTbl[i].KeywordUsed = LBL_NULL;
      LabelTbl[i].ValueOffset = LBL_OFFSET(PhxLblCompressionParams_typ,InstCmprsSegQuality[i-11].Value);
      LabelTbl[i].ValidOffset = LBL_OFFSET(PhxLblCompressionParams_typ,InstCmprsSegQuality[i-11].Valid);
      LabelTbl[i].RtnElementOffset = LBL_NO_RETURN;
      LabelTbl[i].MemoryAllocated =  LBL_SIZE(InstCmprsSegQuality[i-11].Value);
  }

  for(i=267;i<523;i++){
      LabelTbl[i].KeywordList = "INST_CMPRS_SEGMENT_STATUS";
      LabelTbl[i].Format = "INT";
      LabelTbl[i].Required = LBL_OPTIONAL;
      LabelTbl[i].Continuation =  LBL_NO_CONT;
      LabelTbl[i].MaxElements = 1;
      LabelTbl[i].Element = 1;
      LabelTbl[i].KeywordUsed = LBL_NULL;
      LabelTbl[i].ValueOffset = LBL_OFFSET(PhxLblCompressionParams_typ,InstCmprsSegmentStatus[i-267].Value);
      LabelTbl[i].ValidOffset = LBL_OFFSET(PhxLblCompressionParams_typ,InstCmprsSegmentStatus[i-267].Valid);
      LabelTbl[i].RtnElementOffset = LBL_NO_RETURN;
      LabelTbl[i].MemoryAllocated =  LBL_SIZE(InstCmprsSegmentStatus[i-267].Value);
  }

  for(i=523;i<779;i++){
      LabelTbl[i].KeywordList = "INST_CMPRS_SEG_FIRST_LINE";
      LabelTbl[i].Format = "INT";
      LabelTbl[i].Required = LBL_OPTIONAL;
      LabelTbl[i].Continuation =  LBL_NO_CONT;
      LabelTbl[i].MaxElements = 1;
      LabelTbl[i].Element = 1;
      LabelTbl[i].KeywordUsed = LBL_NULL;
      LabelTbl[i].ValueOffset = LBL_OFFSET(PhxLblCompressionParams_typ,InstCmprsSegFirstLine[i-523].Value);
      LabelTbl[i].ValidOffset = LBL_OFFSET(PhxLblCompressionParams_typ,InstCmprsSegFirstLine[i-523].Valid);
      LabelTbl[i].RtnElementOffset = LBL_NO_RETURN;
      LabelTbl[i].MemoryAllocated =  LBL_SIZE(InstCmprsSegFirstLine[i-523].Value);
  }

  for(i=779;i<1035;i++){
      LabelTbl[i].KeywordList = "INST_CMPRS_SEG_FIRST_LINE_SAMP";
      LabelTbl[i].Format = "INT";
      LabelTbl[i].Required = LBL_OPTIONAL;
      LabelTbl[i].Continuation =  LBL_NO_CONT;
      LabelTbl[i].MaxElements = 1;
      LabelTbl[i].Element = 1;
      LabelTbl[i].KeywordUsed = LBL_NULL;
      LabelTbl[i].ValueOffset = LBL_OFFSET(PhxLblCompressionParams_typ,InstCmprsSegFirstLineSamp[i-779].Value);
      LabelTbl[i].ValidOffset = LBL_OFFSET(PhxLblCompressionParams_typ,InstCmprsSegFirstLineSamp[i-779].Valid);
      LabelTbl[i].RtnElementOffset = LBL_NO_RETURN;
      LabelTbl[i].MemoryAllocated =  LBL_SIZE(InstCmprsSegFirstLineSamp[i-779].Value);
  }

  for(i=1035;i<1291;i++){
      LabelTbl[i].KeywordList = "INST_CMPRS_SEG_LINES";
      LabelTbl[i].Format = "INT";
      LabelTbl[i].Required = LBL_OPTIONAL;
      LabelTbl[i].Continuation =  LBL_NO_CONT;
      LabelTbl[i].MaxElements = 1;
      LabelTbl[i].Element = 1;
      LabelTbl[i].KeywordUsed = LBL_NULL;
      LabelTbl[i].ValueOffset = LBL_OFFSET(PhxLblCompressionParams_typ,InstCmprsSegSamples[i-1035].Value);
      LabelTbl[i].ValidOffset = LBL_OFFSET(PhxLblCompressionParams_typ,InstCmprsSegSamples[i-1035].Valid);
      LabelTbl[i].RtnElementOffset = LBL_NO_RETURN;
      LabelTbl[i].MemoryAllocated =  LBL_SIZE(InstCmprsSegSamples[i-1035].Value);
  }

  for(i=1291;i<1547;i++){
      LabelTbl[i].KeywordList = "INST_CMPRS_SEG_SAMPLES";
      LabelTbl[i].Format = "INT";
      LabelTbl[i].Required = LBL_OPTIONAL;
      LabelTbl[i].Continuation =  LBL_NO_CONT;
      LabelTbl[i].MaxElements = 1;
      LabelTbl[i].Element = 1;
      LabelTbl[i].KeywordUsed = LBL_NULL;
      LabelTbl[i].ValueOffset = LBL_OFFSET(PhxLblCompressionParams_typ,InstCmprsSegSamples[i-1291].Value);
      LabelTbl[i].ValidOffset = LBL_OFFSET(PhxLblCompressionParams_typ,InstCmprsSegSamples[i-1291].Valid);
      LabelTbl[i].RtnElementOffset = LBL_NO_RETURN;
      LabelTbl[i].MemoryAllocated =  LBL_SIZE(InstCmprsSegSamples[i-1291].Value);
  }

  for(i=1547;i<1803;i++){
      LabelTbl[i].KeywordList = "INST_CMPRS_SEG_MISSING_PIXELS";
      LabelTbl[i].Format = "INT";
      LabelTbl[i].Required = LBL_OPTIONAL;
      LabelTbl[i].Continuation =  LBL_NO_CONT;
      LabelTbl[i].MaxElements = 1;
      LabelTbl[i].Element = 1;
      LabelTbl[i].KeywordUsed = LBL_NULL;
      LabelTbl[i].ValueOffset = LBL_OFFSET(PhxLblCompressionParams_typ,InstCmprsSegSamples[i-1547].Value);
      LabelTbl[i].ValidOffset = LBL_OFFSET(PhxLblCompressionParams_typ,InstCmprsSegSamples[i-1547].Valid);
      LabelTbl[i].RtnElementOffset = LBL_NO_RETURN;
      LabelTbl[i].MemoryAllocated =  LBL_SIZE(InstCmprsSegSamples[i-1547].Value);
  }

   LabelTbl[1803].KeywordList = 0;
   LabelTbl[1803].Format = 0;
   LabelTbl[1803].Required = 0;
   LabelTbl[1803].Continuation = 0;
   LabelTbl[1803].MaxElements = 0;
   LabelTbl[1803].Element = 0;
   LabelTbl[1803].KeywordUsed = 0;
   LabelTbl[1803].ValueOffset = 0;
   LabelTbl[1803].ValidOffset = 0;
   LabelTbl[1803].RtnElementOffset = 0;
   LabelTbl[1803].MemoryAllocated = 0;


  LblApiProcess_typ	Label;
  Label.Table = LabelTbl;
  Label.Type,"PROPERTY";
  Label.NameKeyword,"PROPERTY";
  Label.NameValue,"COMPRESSION_PARAMS";
  //Label., LBL_NULL };
*/
  Label.Buffer = (void *)LabelItems;

  PrintLabelElements( &Label );

  return;
}

/******************************************************************************
 *				LBL_TEST_IDENTIFIER
 *
 *****************************************************************************/
void     PhxLblTestCompressionParams(
  PhxLblCompressionParams_typ	*LabelItems)
{
/*
   static LblApiElement_typ    LabelTbl[1804];

   LabelTbl[1].KeywordList = "ERROR_PIXELS";
   LabelTbl[1].Format = "INT";
   LabelTbl[1].Required = LBL_OPTIONAL;
   LabelTbl[1].Continuation =  LBL_NO_CONT;
   LabelTbl[1].MaxElements = 1;
   LabelTbl[1].Element = 1;
   LabelTbl[1].KeywordUsed = LBL_NULL;
   LabelTbl[1].ValueOffset = LBL_OFFSET(PhxLblCompressionParams_typ,ErrorPixels.Value);
   LabelTbl[1].ValidOffset = LBL_OFFSET(PhxLblCompressionParams_typ,ErrorPixels.Valid);
   LabelTbl[1].RtnElementOffset = LBL_NO_RETURN;
   LabelTbl[1].MemoryAllocated =  LBL_SIZE(ErrorPixels.Value);

   LabelTbl[2].KeywordList = "COMP_AC_IDX";
   LabelTbl[2].Format = "INT";
   LabelTbl[2].Required = LBL_OPTIONAL;
   LabelTbl[2].Continuation =  LBL_NO_CONT;
   LabelTbl[2].MaxElements = 1;
   LabelTbl[2].Element = 1;
   LabelTbl[2].KeywordUsed = LBL_NULL;
   LabelTbl[2].ValueOffset = LBL_OFFSET(PhxLblCompressionParams_typ,InstCmprsACIdx.Value);
   LabelTbl[2].ValidOffset = LBL_OFFSET(PhxLblCompressionParams_typ,InstCmprsACIdx.Valid);
   LabelTbl[2].RtnElementOffset = LBL_NO_RETURN;
   LabelTbl[2].MemoryAllocated =  LBL_SIZE(InstCmprsACIdx.Value);

   LabelTbl[3].KeywordList = "COMP_DC_IDX";
   LabelTbl[3].Format = "INT";
   LabelTbl[3].Required = LBL_OPTIONAL;
   LabelTbl[3].Continuation =  LBL_NO_CONT;
   LabelTbl[3].MaxElements = 1;
   LabelTbl[3].Element = 1;
   LabelTbl[3].KeywordUsed = LBL_NULL;
   LabelTbl[3].ValueOffset = LBL_OFFSET(PhxLblCompressionParams_typ,InstCmprsDCIdx.Value);
   LabelTbl[3].ValidOffset = LBL_OFFSET(PhxLblCompressionParams_typ,InstCmprsDCIdx.Valid);
   LabelTbl[3].RtnElementOffset = LBL_NO_RETURN;
   LabelTbl[3].MemoryAllocated =  LBL_SIZE(InstCmprsDCIdx.Value);

   LabelTbl[4].KeywordList = "COMP_Q_IDX";
   LabelTbl[4].Format = "INT";
   LabelTbl[4].Required = LBL_OPTIONAL;
   LabelTbl[4].Continuation =  LBL_NO_CONT;
   LabelTbl[4].MaxElements = 1;
   LabelTbl[4].Element = 1;
   LabelTbl[4].KeywordUsed = LBL_NULL;
   LabelTbl[4].ValueOffset = LBL_OFFSET(PhxLblCompressionParams_typ,InstCmprsQIdx.Value);
   LabelTbl[4].ValidOffset = LBL_OFFSET(PhxLblCompressionParams_typ,InstCmprsQIdx.Valid);
   LabelTbl[4].RtnElementOffset = LBL_NO_RETURN;
   LabelTbl[4].MemoryAllocated =  LBL_SIZE(InstCmprsQIdx.Value);

   LabelTbl[5].KeywordList = "INST_CMPRS_MODE";
   LabelTbl[5].Format = "INT";
   LabelTbl[5].Required = LBL_OPTIONAL;
   LabelTbl[5].Continuation =  LBL_NO_CONT;
   LabelTbl[5].MaxElements = 1;
   LabelTbl[5].Element = 1;
   LabelTbl[5].KeywordUsed = LBL_NULL;
   LabelTbl[5].ValueOffset = LBL_OFFSET(PhxLblCompressionParams_typ,InstCmprsMode.Value);
   LabelTbl[5].ValidOffset = LBL_OFFSET(PhxLblCompressionParams_typ,InstCmprsMode.Valid);
   LabelTbl[5].RtnElementOffset = LBL_NO_RETURN;
   LabelTbl[5].MemoryAllocated =  LBL_SIZE(InstCmprsMode.Value);

   LabelTbl[6].KeywordList = "INST_CMPRS_NAME";
   LabelTbl[6].Format = "STRING";
   LabelTbl[6].Required = LBL_OPTIONAL;
   LabelTbl[6].Continuation =  LBL_NO_CONT;
   LabelTbl[6].MaxElements = 1;
   LabelTbl[6].Element = 1;
   LabelTbl[6].KeywordUsed = LBL_NULL;
   LabelTbl[6].ValueOffset = LBL_OFFSET(PhxLblCompressionParams_typ,InstCmprsQIdx.Value);
   LabelTbl[6].ValidOffset = LBL_OFFSET(PhxLblCompressionParams_typ,InstCmprsQIdx.Valid);
   LabelTbl[6].RtnElementOffset = LBL_NO_RETURN;
   LabelTbl[6].MemoryAllocated =  LBL_SIZE(InstCmprsQIdx.Value);

   LabelTbl[7].KeywordList = "INST_CMPRS_QUALITY";
   LabelTbl[7].Format = "INT";
   LabelTbl[7].Required = LBL_OPTIONAL;
   LabelTbl[7].Continuation =  LBL_NO_CONT;
   LabelTbl[7].MaxElements = 1;
   LabelTbl[7].Element = 1;
   LabelTbl[7].KeywordUsed = LBL_NULL;
   LabelTbl[7].ValueOffset = LBL_OFFSET(PhxLblCompressionParams_typ,InstCmprsQuality.Value);
   LabelTbl[7].ValidOffset = LBL_OFFSET(PhxLblCompressionParams_typ,InstCmprsQuality.Valid);
   LabelTbl[7].RtnElementOffset = LBL_NO_RETURN;
   LabelTbl[7].MemoryAllocated =  LBL_SIZE(InstCmprsQuality.Value);

   LabelTbl[8].KeywordList = "INST_CMPRS_RATE";
   LabelTbl[8].Format = "REAL";
   LabelTbl[8].Required = LBL_OPTIONAL;
   LabelTbl[8].Continuation =  LBL_NO_CONT;
   LabelTbl[8].MaxElements = 1;
   LabelTbl[8].Element = 1;
   LabelTbl[8].KeywordUsed = LBL_NULL;
   LabelTbl[8].ValueOffset = LBL_OFFSET(PhxLblCompressionParams_typ,InstCmprsRate.Value);
   LabelTbl[8].ValidOffset = LBL_OFFSET(PhxLblCompressionParams_typ,InstCmprsRate.Valid);
   LabelTbl[8].RtnElementOffset = LBL_NO_RETURN;
   LabelTbl[8].MemoryAllocated =  LBL_SIZE(InstCmprsRate.Value);

   LabelTbl[9].KeywordList = "INST_CMPRS_RATIO";
   LabelTbl[9].Format = "REAL";
   LabelTbl[9].Required = LBL_OPTIONAL;
   LabelTbl[9].Continuation =  LBL_NO_CONT;
   LabelTbl[9].MaxElements = 1;
   LabelTbl[9].Element = 1;
   LabelTbl[9].KeywordUsed = LBL_NULL;
   LabelTbl[9].ValueOffset = LBL_OFFSET(PhxLblCompressionParams_typ,InstCmprsRatio.Value);
   LabelTbl[9].ValidOffset = LBL_OFFSET(PhxLblCompressionParams_typ,InstCmprsRatio.Valid);
   LabelTbl[9].RtnElementOffset = LBL_NO_RETURN;
   LabelTbl[9].MemoryAllocated =  LBL_SIZE(InstCmprsRatio.Value);

   LabelTbl[10].KeywordList = "INST_CMPRS_SEGMENTS";
   LabelTbl[10].Format = "INT";
   LabelTbl[10].Required = LBL_OPTIONAL;
   LabelTbl[10].Continuation =  LBL_NO_CONT;
   LabelTbl[10].MaxElements = 1;
   LabelTbl[10].Element = 1;
   LabelTbl[10].KeywordUsed = LBL_NULL;
   LabelTbl[10].ValueOffset = LBL_OFFSET(PhxLblCompressionParams_typ,InstCmprsSegments.Value);
   LabelTbl[10].ValidOffset = LBL_OFFSET(PhxLblCompressionParams_typ,InstCmprsSegments.Valid);
   LabelTbl[10].RtnElementOffset = LBL_NO_RETURN;
   LabelTbl[10].MemoryAllocated =  LBL_SIZE(InstCmprsSegments.Value);

   int i;
   for(i=11;i<267;i++){
      LabelTbl[i].KeywordList = "INST_CMPRS_SEGMENT_QUALITY";
      LabelTbl[i].Format = "REAL";
      LabelTbl[i].Required = LBL_OPTIONAL;
      LabelTbl[i].Continuation =  LBL_NO_CONT;
      LabelTbl[i].MaxElements = 1;
      LabelTbl[i].Element = 1;
      LabelTbl[i].KeywordUsed = LBL_NULL;
      LabelTbl[i].ValueOffset = LBL_OFFSET(PhxLblCompressionParams_typ,InstCmprsSegQuality[i-11].Value);
      LabelTbl[i].ValidOffset = LBL_OFFSET(PhxLblCompressionParams_typ,InstCmprsSegQuality[i-11].Valid);
      LabelTbl[i].RtnElementOffset = LBL_NO_RETURN;
      LabelTbl[i].MemoryAllocated =  LBL_SIZE(InstCmprsSegQuality[i-11].Value);
  }

  for(i=267;i<523;i++){
      LabelTbl[i].KeywordList = "INST_CMPRS_SEGMENT_STATUS";
      LabelTbl[i].Format = "INT";
      LabelTbl[i].Required = LBL_OPTIONAL;
      LabelTbl[i].Continuation =  LBL_NO_CONT;
      LabelTbl[i].MaxElements = 1;
      LabelTbl[i].Element = 1;
      LabelTbl[i].KeywordUsed = LBL_NULL;
      LabelTbl[i].ValueOffset = LBL_OFFSET(PhxLblCompressionParams_typ,InstCmprsSegmentStatus[i-267].Value);
      LabelTbl[i].ValidOffset = LBL_OFFSET(PhxLblCompressionParams_typ,InstCmprsSegmentStatus[i-267].Valid);
      LabelTbl[i].RtnElementOffset = LBL_NO_RETURN;
      LabelTbl[i].MemoryAllocated =  LBL_SIZE(InstCmprsSegmentStatus[i-267].Value);
  }

  for(i=523;i<779;i++){
      LabelTbl[i].KeywordList = "INST_CMPRS_SEG_FIRST_LINE";
      LabelTbl[i].Format = "INT";
      LabelTbl[i].Required = LBL_OPTIONAL;
      LabelTbl[i].Continuation =  LBL_NO_CONT;
      LabelTbl[i].MaxElements = 1;
      LabelTbl[i].Element = 1;
      LabelTbl[i].KeywordUsed = LBL_NULL;
      LabelTbl[i].ValueOffset = LBL_OFFSET(PhxLblCompressionParams_typ,InstCmprsSegFirstLine[i-523].Value);
      LabelTbl[i].ValidOffset = LBL_OFFSET(PhxLblCompressionParams_typ,InstCmprsSegFirstLine[i-523].Valid);
      LabelTbl[i].RtnElementOffset = LBL_NO_RETURN;
      LabelTbl[i].MemoryAllocated =  LBL_SIZE(InstCmprsSegFirstLine[i-523].Value);
  }

  for(i=779;i<1035;i++){
      LabelTbl[i].KeywordList = "INST_CMPRS_SEG_FIRST_LINE_SAMP";
      LabelTbl[i].Format = "INT";
      LabelTbl[i].Required = LBL_OPTIONAL;
      LabelTbl[i].Continuation =  LBL_NO_CONT;
      LabelTbl[i].MaxElements = 1;
      LabelTbl[i].Element = 1;
      LabelTbl[i].KeywordUsed = LBL_NULL;
      LabelTbl[i].ValueOffset = LBL_OFFSET(PhxLblCompressionParams_typ,InstCmprsSegFirstLineSamp[i-779].Value);
      LabelTbl[i].ValidOffset = LBL_OFFSET(PhxLblCompressionParams_typ,InstCmprsSegFirstLineSamp[i-779].Valid);
      LabelTbl[i].RtnElementOffset = LBL_NO_RETURN;
      LabelTbl[i].MemoryAllocated =  LBL_SIZE(InstCmprsSegFirstLineSamp[i-779].Value);
  }

  for(i=1035;i<1291;i++){
      LabelTbl[i].KeywordList = "INST_CMPRS_SEG_LINES";
      LabelTbl[i].Format = "INT";
      LabelTbl[i].Required = LBL_OPTIONAL;
      LabelTbl[i].Continuation =  LBL_NO_CONT;
      LabelTbl[i].MaxElements = 1;
      LabelTbl[i].Element = 1;
      LabelTbl[i].KeywordUsed = LBL_NULL;
      LabelTbl[i].ValueOffset = LBL_OFFSET(PhxLblCompressionParams_typ,InstCmprsSegSamples[i-1035].Value);
      LabelTbl[i].ValidOffset = LBL_OFFSET(PhxLblCompressionParams_typ,InstCmprsSegSamples[i-1035].Valid);
      LabelTbl[i].RtnElementOffset = LBL_NO_RETURN;
      LabelTbl[i].MemoryAllocated =  LBL_SIZE(InstCmprsSegSamples[i-1035].Value);
  }

  for(i=1291;i<1547;i++){
      LabelTbl[i].KeywordList = "INST_CMPRS_SEG_SAMPLES";
      LabelTbl[i].Format = "INT";
      LabelTbl[i].Required = LBL_OPTIONAL;
      LabelTbl[i].Continuation =  LBL_NO_CONT;
      LabelTbl[i].MaxElements = 1;
      LabelTbl[i].Element = 1;
      LabelTbl[i].KeywordUsed = LBL_NULL;
      LabelTbl[i].ValueOffset = LBL_OFFSET(PhxLblCompressionParams_typ,InstCmprsSegSamples[i-1291].Value);
      LabelTbl[i].ValidOffset = LBL_OFFSET(PhxLblCompressionParams_typ,InstCmprsSegSamples[i-1291].Valid);
      LabelTbl[i].RtnElementOffset = LBL_NO_RETURN;
      LabelTbl[i].MemoryAllocated =  LBL_SIZE(InstCmprsSegSamples[i-1291].Value);
  }

  for(i=1547;i<1803;i++){
      LabelTbl[i].KeywordList = "INST_CMPRS_SEG_MISSING_PIXELS";
      LabelTbl[i].Format = "INT";
      LabelTbl[i].Required = LBL_OPTIONAL;
      LabelTbl[i].Continuation =  LBL_NO_CONT;
      LabelTbl[i].MaxElements = 1;
      LabelTbl[i].Element = 1;
      LabelTbl[i].KeywordUsed = LBL_NULL;
      LabelTbl[i].ValueOffset = LBL_OFFSET(PhxLblCompressionParams_typ,InstCmprsSegSamples[i-1547].Value);
      LabelTbl[i].ValidOffset = LBL_OFFSET(PhxLblCompressionParams_typ,InstCmprsSegSamples[i-1547].Valid);
      LabelTbl[i].RtnElementOffset = LBL_NO_RETURN;
      LabelTbl[i].MemoryAllocated =  LBL_SIZE(InstCmprsSegSamples[i-1547].Value);
  }

   LabelTbl[1803].KeywordList = 0;
   LabelTbl[1803].Format = 0;
   LabelTbl[1803].Required = 0;
   LabelTbl[1803].Continuation = 0;
   LabelTbl[1803].MaxElements = 0;
   LabelTbl[1803].Element = 0;
   LabelTbl[1803].KeywordUsed = 0;
   LabelTbl[1803].ValueOffset = 0;
   LabelTbl[1803].ValidOffset = 0;
   LabelTbl[1803].RtnElementOffset = 0;
   LabelTbl[1803].MemoryAllocated = 0;


  LblApiProcess_typ	Label;
  Label.Table = LabelTbl;
  Label.Type,"PROPERTY";
  Label.NameKeyword,"PROPERTY";
  Label.NameValue,"COMPRESSION_PARAMS";
  //Label., LBL_NULL };

*/
  Label.Buffer = (void *)LabelItems;

  TestLoadLabelElements( &Label );

  return;
}
