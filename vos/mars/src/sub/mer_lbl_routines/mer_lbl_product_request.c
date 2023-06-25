/**  Copyright (c) 2002, California Institute of Technology             **/
/**  U. S. Government sponsorship under NASA contract is acknowledged   **/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "mer_lbl_product_request.h"

/******************************************************************************
 *				MER_LBL_<PRODUCT>_REQUEST
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
 *  MerLbl<product>Request.  This routine requires exactly 4 parameters.
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

#define  LBL_SIZE(x)	sizeof(((MerLblProdRequest_typ *)0)->x)

/************************/
/***   IMAGE REQUEST  ***/
/************************/
static LblApiElement_typ	IR_LabelTbl[] = {
	{"SOURCE_ID",				"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(MerLblProdRequest_typ, SourceId.Value),
		LBL_OFFSET(MerLblProdRequest_typ, SourceId.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SourceId.Value)},

	{"GROUP_APPLICABILITY_FLAG",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(MerLblProdRequest_typ, GroupApplicabilityFlag.Value),
		LBL_OFFSET(MerLblProdRequest_typ, GroupApplicabilityFlag.Valid),
		LBL_NO_RETURN,	LBL_SIZE(GroupApplicabilityFlag.Value)},

	{"DOWNLOAD_PRIORITY",			"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(MerLblProdRequest_typ, DownloadPriority.Value),
                LBL_OFFSET(MerLblProdRequest_typ, DownloadPriority.Valid),
                LBL_NO_RETURN,  LBL_SIZE(DownloadPriority.Value)},

	{"PIXEL_DOWNSAMPLE_OPTION",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(MerLblProdRequest_typ, PixelDownsampleOption.Value),
		LBL_OFFSET(MerLblProdRequest_typ, PixelDownsampleOption.Valid),
		LBL_NO_RETURN,	LBL_SIZE(PixelDownsampleOption.Value)},

	{"PIXEL_AVERAGING_HEIGHT",		"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(MerLblProdRequest_typ, PixelAveragingHeight.Value),
                LBL_OFFSET(MerLblProdRequest_typ, PixelAveragingHeight.Valid),
                LBL_NO_RETURN,  LBL_SIZE(PixelAveragingHeight.Value)},

	{"PIXEL_AVERAGING_WIDTH",		"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(MerLblProdRequest_typ, PixelAveragingWidth.Value),
                LBL_OFFSET(MerLblProdRequest_typ, PixelAveragingWidth.Valid),
                LBL_NO_RETURN,  LBL_SIZE(PixelAveragingWidth.Value)},

	{"SAMPLE_BIT_MODE_ID",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(MerLblProdRequest_typ, SampleBitModeId.Value),
		LBL_OFFSET(MerLblProdRequest_typ, SampleBitModeId.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SampleBitModeId.Value)},

	{"INST_CMPRS_MODE"	,		"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(MerLblProdRequest_typ, InstCmprsMode.Value),
		LBL_OFFSET(MerLblProdRequest_typ, InstCmprsMode.Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsMode.Value)},

	{"INST_CMPRS_RATE",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(MerLblProdRequest_typ, InstCmprsRate.Value),
                LBL_OFFSET(MerLblProdRequest_typ, InstCmprsRate.Valid),
                LBL_NO_RETURN,  LBL_SIZE(InstCmprsRate.Value)},

	{"INST_CMPRS_QUALITY",			"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(MerLblProdRequest_typ, InstCmprsQuality.Value),
                LBL_OFFSET(MerLblProdRequest_typ, InstCmprsQuality.Valid),
                LBL_NO_RETURN,  LBL_SIZE(InstCmprsQuality.Value)},

	{"INST_CMPRS_FILTER"	,		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(MerLblProdRequest_typ, InstCmprsFilter.Value),
		LBL_OFFSET(MerLblProdRequest_typ, InstCmprsFilter.Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsFilter.Value)},

	{"INST_DECOMP_STAGES",			"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(MerLblProdRequest_typ, InstDecompStages.Value),
                LBL_OFFSET(MerLblProdRequest_typ, InstDecompStages.Valid),
                LBL_NO_RETURN,  LBL_SIZE(InstDecompStages.Value)},

	{"INST_CMPRS_SEGMENTS",			"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(MerLblProdRequest_typ, InstCmprsSegments.Value),
                LBL_OFFSET(MerLblProdRequest_typ, InstCmprsSegments.Valid),
                LBL_NO_RETURN,  LBL_SIZE(InstCmprsSegments.Value)},

	{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}};

/********************************/
/*** REFERENCE PIXEL REQUEST  ***/
/********************************/
static LblApiElement_typ	RP_LabelTbl[] = {
	{"SOURCE_ID",				"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(MerLblProdRequest_typ, SourceId.Value),
		LBL_OFFSET(MerLblProdRequest_typ, SourceId.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SourceId.Value)},

	{"GROUP_APPLICABILITY_FLAG",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(MerLblProdRequest_typ, GroupApplicabilityFlag.Value),
		LBL_OFFSET(MerLblProdRequest_typ, GroupApplicabilityFlag.Valid),
		LBL_NO_RETURN,	LBL_SIZE(GroupApplicabilityFlag.Value)},

	{"DOWNLOAD_PRIORITY",			"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(MerLblProdRequest_typ, DownloadPriority.Value),
                LBL_OFFSET(MerLblProdRequest_typ, DownloadPriority.Valid),
                LBL_NO_RETURN,  LBL_SIZE(DownloadPriority.Value)},

	{"INST_CMPRS_MODE"	,		"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(MerLblProdRequest_typ, InstCmprsMode.Value),
		LBL_OFFSET(MerLblProdRequest_typ, InstCmprsMode.Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsMode.Value)},

	{"INST_CMPRS_RATE",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(MerLblProdRequest_typ, InstCmprsRate.Value),
                LBL_OFFSET(MerLblProdRequest_typ, InstCmprsRate.Valid),
                LBL_NO_RETURN,  LBL_SIZE(InstCmprsRate.Value)},

	{"INST_CMPRS_QUALITY",			"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(MerLblProdRequest_typ, InstCmprsQuality.Value),
                LBL_OFFSET(MerLblProdRequest_typ, InstCmprsQuality.Valid),
                LBL_NO_RETURN,  LBL_SIZE(InstCmprsQuality.Value)},

	{"INST_CMPRS_FILTER"	,		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(MerLblProdRequest_typ, InstCmprsFilter.Value),
		LBL_OFFSET(MerLblProdRequest_typ, InstCmprsFilter.Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsFilter.Value)},

	{"INST_DECOMP_STAGES",			"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(MerLblProdRequest_typ, InstDecompStages.Value),
                LBL_OFFSET(MerLblProdRequest_typ, InstDecompStages.Valid),
                LBL_NO_RETURN,  LBL_SIZE(InstDecompStages.Value)},

	{"INST_CMPRS_SEGMENTS",			"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(MerLblProdRequest_typ, InstCmprsSegments.Value),
                LBL_OFFSET(MerLblProdRequest_typ, InstCmprsSegments.Valid),
                LBL_NO_RETURN,  LBL_SIZE(InstCmprsSegments.Value)},

	{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}};

/***************************/
/***  THUMBNAIL REQUEST  ***/
/***************************/
static LblApiElement_typ	TN_LabelTbl[] = {
	{"SOURCE_ID",				"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(MerLblProdRequest_typ, SourceId.Value),
		LBL_OFFSET(MerLblProdRequest_typ, SourceId.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SourceId.Value)},

	{"GROUP_APPLICABILITY_FLAG",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(MerLblProdRequest_typ, GroupApplicabilityFlag.Value),
		LBL_OFFSET(MerLblProdRequest_typ, GroupApplicabilityFlag.Valid),
		LBL_NO_RETURN,	LBL_SIZE(GroupApplicabilityFlag.Value)},

	{"DOWNLOAD_PRIORITY",			"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(MerLblProdRequest_typ, DownloadPriority.Value),
                LBL_OFFSET(MerLblProdRequest_typ, DownloadPriority.Valid),
                LBL_NO_RETURN,  LBL_SIZE(DownloadPriority.Value)},

	{"LINES",				"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(MerLblProdRequest_typ, Lines.Value),
                LBL_OFFSET(MerLblProdRequest_typ, Lines.Valid),
                LBL_NO_RETURN,  LBL_SIZE(Lines.Value)},

	{"LINE_SAMPLES",			"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(MerLblProdRequest_typ, LineSamples.Value),
                LBL_OFFSET(MerLblProdRequest_typ, LineSamples.Valid),
                LBL_NO_RETURN,  LBL_SIZE(LineSamples.Value)},

	{"SAMPLE_BIT_MODE_ID",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(MerLblProdRequest_typ, SampleBitModeId.Value),
		LBL_OFFSET(MerLblProdRequest_typ, SampleBitModeId.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SampleBitModeId.Value)},

	{"INST_CMPRS_MODE"	,		"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(MerLblProdRequest_typ, InstCmprsMode.Value),
		LBL_OFFSET(MerLblProdRequest_typ, InstCmprsMode.Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsMode.Value)},

	{"INST_CMPRS_RATE",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(MerLblProdRequest_typ, InstCmprsRate.Value),
                LBL_OFFSET(MerLblProdRequest_typ, InstCmprsRate.Valid),
                LBL_NO_RETURN,  LBL_SIZE(InstCmprsRate.Value)},

	{"INST_CMPRS_QUALITY",			"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(MerLblProdRequest_typ, InstCmprsQuality.Value),
                LBL_OFFSET(MerLblProdRequest_typ, InstCmprsQuality.Valid),
                LBL_NO_RETURN,  LBL_SIZE(InstCmprsQuality.Value)},

	{"INST_CMPRS_FILTER"	,		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(MerLblProdRequest_typ, InstCmprsFilter.Value),
		LBL_OFFSET(MerLblProdRequest_typ, InstCmprsFilter.Valid),
		LBL_NO_RETURN,	LBL_SIZE(InstCmprsFilter.Value)},

	{"INST_DECOMP_STAGES",			"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(MerLblProdRequest_typ, InstDecompStages.Value),
                LBL_OFFSET(MerLblProdRequest_typ, InstDecompStages.Valid),
                LBL_NO_RETURN,  LBL_SIZE(InstDecompStages.Value)},

	{"INST_CMPRS_SEGMENTS",			"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(MerLblProdRequest_typ, InstCmprsSegments.Value),
                LBL_OFFSET(MerLblProdRequest_typ, InstCmprsSegments.Valid),
                LBL_NO_RETURN,  LBL_SIZE(InstCmprsSegments.Value)},

	{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}};

/**************************/
/***  SUBFRAME REQUEST  ***/
/**************************/
static LblApiElement_typ	SB_LabelTbl[] = {
	{"SOURCE_ID",				"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(MerLblProdRequest_typ, SourceId.Value),
		LBL_OFFSET(MerLblProdRequest_typ, SourceId.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SourceId.Value)},

	{"GROUP_APPLICABILITY_FLAG",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(MerLblProdRequest_typ, GroupApplicabilityFlag.Value),
		LBL_OFFSET(MerLblProdRequest_typ, GroupApplicabilityFlag.Valid),
		LBL_NO_RETURN,	LBL_SIZE(GroupApplicabilityFlag.Value)},

	{"SUBFRAME_TYPE"	,		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(MerLblProdRequest_typ, SubframeType.Value),
		LBL_OFFSET(MerLblProdRequest_typ, SubframeType.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SubframeType.Value)},

	{"FIRST_LINE",				"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(MerLblProdRequest_typ, FirstLine.Value),
                LBL_OFFSET(MerLblProdRequest_typ, FirstLine.Valid),
                LBL_NO_RETURN,  LBL_SIZE(FirstLine.Value)},

	{"FIRST_LINE_SAMPLE",			"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(MerLblProdRequest_typ, FirstLineSample.Value),
                LBL_OFFSET(MerLblProdRequest_typ, FirstLineSample.Valid),
                LBL_NO_RETURN,  LBL_SIZE(FirstLineSample.Value)},

	{"LINES",				"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(MerLblProdRequest_typ, Lines.Value),
                LBL_OFFSET(MerLblProdRequest_typ, Lines.Valid),
                LBL_NO_RETURN,  LBL_SIZE(Lines.Value)},

	{"LINE_SAMPLES",			"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(MerLblProdRequest_typ, LineSamples.Value),
                LBL_OFFSET(MerLblProdRequest_typ, LineSamples.Valid),
                LBL_NO_RETURN,  LBL_SIZE(LineSamples.Value)},

	{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}};

/*************************/
/***  ROW_SUM REQUEST  ***/
/*************************/
static LblApiElement_typ	RS_LabelTbl[] = {
	{"SOURCE_ID",				"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(MerLblProdRequest_typ, SourceId.Value),
		LBL_OFFSET(MerLblProdRequest_typ, SourceId.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SourceId.Value)},

	{"GROUP_APPLICABILITY_FLAG",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(MerLblProdRequest_typ, GroupApplicabilityFlag.Value),
		LBL_OFFSET(MerLblProdRequest_typ, GroupApplicabilityFlag.Valid),
		LBL_NO_RETURN,	LBL_SIZE(GroupApplicabilityFlag.Value)},

	{"DOWNLOAD_PRIORITY",			"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(MerLblProdRequest_typ, DownloadPriority.Value),
                LBL_OFFSET(MerLblProdRequest_typ, DownloadPriority.Valid),
                LBL_NO_RETURN,  LBL_SIZE(DownloadPriority.Value)},

	{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}};

/****************************/
/***  COLUMN_SUM REQUEST  ***/
/****************************/
static LblApiElement_typ	CS_LabelTbl[] = {
	{"SOURCE_ID",				"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(MerLblProdRequest_typ, SourceId.Value),
		LBL_OFFSET(MerLblProdRequest_typ, SourceId.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SourceId.Value)},

	{"GROUP_APPLICABILITY_FLAG",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(MerLblProdRequest_typ, GroupApplicabilityFlag.Value),
		LBL_OFFSET(MerLblProdRequest_typ, GroupApplicabilityFlag.Valid),
		LBL_NO_RETURN,	LBL_SIZE(GroupApplicabilityFlag.Value)},

	{"DOWNLOAD_PRIORITY",			"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(MerLblProdRequest_typ, DownloadPriority.Value),
                LBL_OFFSET(MerLblProdRequest_typ, DownloadPriority.Valid),
                LBL_NO_RETURN,  LBL_SIZE(DownloadPriority.Value)},

	{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}};

/**************************/
/***  SUN_FIND REQUEST  ***/
/**************************/
static LblApiElement_typ	SF_LabelTbl[] = {
	{"SOURCE_ID",				"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(MerLblProdRequest_typ, SourceId.Value),
		LBL_OFFSET(MerLblProdRequest_typ, SourceId.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SourceId.Value)},

	{"GROUP_APPLICABILITY_FLAG",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(MerLblProdRequest_typ, GroupApplicabilityFlag.Value),
		LBL_OFFSET(MerLblProdRequest_typ, GroupApplicabilityFlag.Valid),
		LBL_NO_RETURN,	LBL_SIZE(GroupApplicabilityFlag.Value)},

	{"LINES",				"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(MerLblProdRequest_typ, Lines.Value),
                LBL_OFFSET(MerLblProdRequest_typ, Lines.Valid),
                LBL_NO_RETURN,  LBL_SIZE(Lines.Value)},

	{"LINE_SAMPLES",			"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(MerLblProdRequest_typ, LineSamples.Value),
                LBL_OFFSET(MerLblProdRequest_typ, LineSamples.Valid),
                LBL_NO_RETURN,  LBL_SIZE(LineSamples.Value)},

	{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}};

/***************************/
/***  HISTOGRAM REQUEST  ***/
/***************************/
static LblApiElement_typ	HG_LabelTbl[] = {
	{"SOURCE_ID",				"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(MerLblProdRequest_typ, SourceId.Value),
		LBL_OFFSET(MerLblProdRequest_typ, SourceId.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SourceId.Value)},

	{"GROUP_APPLICABILITY_FLAG",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(MerLblProdRequest_typ, GroupApplicabilityFlag.Value),
		LBL_OFFSET(MerLblProdRequest_typ, GroupApplicabilityFlag.Valid),
		LBL_NO_RETURN,	LBL_SIZE(GroupApplicabilityFlag.Value)},

	{"DOWNLOAD_PRIORITY",			"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(MerLblProdRequest_typ, DownloadPriority.Value),
                LBL_OFFSET(MerLblProdRequest_typ, DownloadPriority.Valid),
                LBL_NO_RETURN,  LBL_SIZE(DownloadPriority.Value)},

	{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}};


/***  Table Definitions  ***/

static LblApiProcess_typ	IR_Label = {
	IR_LabelTbl,	"PROPERTY",	"PROPERTY",
	"IMAGE_REQUEST_PARMS",	LBL_NULL };
static LblApiProcess_typ	RP_Label = {
	RP_LabelTbl, 	"PROPERTY",	"PROPERTY",
	"REFERENCE_PIXEL_REQUEST_PARMS",	LBL_NULL };
static LblApiProcess_typ	TN_Label = {
	TN_LabelTbl,	"PROPERTY",	"PROPERTY",
	"THUMBNAIL_REQUEST_PARMS",	LBL_NULL };
static LblApiProcess_typ	SB_Label = {
	SB_LabelTbl,	"PROPERTY",	"PROPERTY",
	"SUBFRAME_REQUEST_PARMS",	LBL_NULL };
static LblApiProcess_typ	RS_Label = {
	RS_LabelTbl,	"PROPERTY",	"PROPERTY",
	"ROW_SUM_REQUEST_PARMS",	LBL_NULL };
static LblApiProcess_typ	CS_Label = {
	CS_LabelTbl,	"PROPERTY",	"PROPERTY",
	"COLUMN_SUM_REQUEST_PARMS",	LBL_NULL };
static LblApiProcess_typ	SF_Label = {
	SF_LabelTbl,	"PROPERTY",	"PROPERTY",
	"SUN_FIND_REQUEST_PARMS",	LBL_NULL };
static LblApiProcess_typ	HG_Label = {
	HG_LabelTbl,	"PROPERTY",	"PROPERTY",
	"HISTOGRAM_REQUEST_PARMS",	LBL_NULL };
/*
 *  IMAGE REQUEST
 */
/******************************************************************************
 *				MER_LBL_IMAGE_REQUEST
 *
 *****************************************************************************/
int     MerLblImageRequest(
  int   Unit,
  int   Obtain,
  MerLblProdRequest_typ      *LabelItems,
  int	Instance)
{ int   RtnStatus;
  LblApiCntrl_typ	Cntrl;

  IR_Label.Buffer = (void *)LabelItems;
  IR_Label.BufferSize = sizeof(MerLblProdRequest_typ);

  memset(&Cntrl,0,sizeof(LblApiCntrl_typ));
  Cntrl.Instance = Instance;
  Cntrl.FileUnit = Unit;
  Cntrl.Obtain = Obtain;
  Cntrl.ProceedOnError = LBL_TRUE;

  RtnStatus = LblProcessor(&Cntrl, &IR_Label);

  return (RtnStatus);
}

/******************************************************************************
 *				LBL_PRINT_IMAGE_REQUEST
 *
 *****************************************************************************/
void	MerLblPrintImageRequest(
  MerLblProdRequest_typ	*LabelItems)
{
  IR_Label.Buffer = (void *)LabelItems;

  PrintLabelElements( &IR_Label );

  return;
}

/******************************************************************************
 *				LBL_TEST_IMAGE_REQUEST
 *
 *****************************************************************************/
void	MerLblTestImageRequest(
  MerLblProdRequest_typ	*LabelItems)
{
  IR_Label.Buffer = (void *)LabelItems;

  TestLoadLabelElements( &IR_Label );

  return;
}

/*
 * REFERENCE PIXEL REQUEST
 */
/******************************************************************************
 *				MER_LBL_REF_PIXEL_REQUEST
 *
 *****************************************************************************/
int     MerLblRefPixelRequest(
  int   Unit,
  int   Obtain,
  MerLblProdRequest_typ      *LabelItems,
  int	Instance)
{ int   RtnStatus;
  LblApiCntrl_typ	Cntrl;

  RP_Label.Buffer = (void *)LabelItems;
  RP_Label.BufferSize = sizeof(MerLblProdRequest_typ);

  memset(&Cntrl,0,sizeof(LblApiCntrl_typ));
  Cntrl.Instance = Instance;
  Cntrl.FileUnit = Unit;
  Cntrl.Obtain = Obtain;
  Cntrl.ProceedOnError = LBL_TRUE;

  RtnStatus = LblProcessor(&Cntrl, &RP_Label);

  return (RtnStatus);
}

/******************************************************************************
 *				LBL_PRINT_REF_PIXEL_REQUEST
 *
 *****************************************************************************/
void	MerLblPrintRefPixelRequest(
  MerLblProdRequest_typ	*LabelItems)
{
  RP_Label.Buffer = (void *)LabelItems;

  PrintLabelElements( &RP_Label );

  return;
}

/******************************************************************************
 *				LBL_TEST_REF_PIXEL_REQUEST
 *
 *****************************************************************************/
void	MerLblTestRefPixelRequest(
  MerLblProdRequest_typ	*LabelItems)
{
  RP_Label.Buffer = (void *)LabelItems;

  TestLoadLabelElements( &RP_Label );

  return;
}

/*
 * THUMBNAIL REQUEST
 */
/******************************************************************************
 *				MER_LBL_THUMBNAIL_REQUEST
 *
 *****************************************************************************/
int     MerLblThumbnailRequest(
  int   Unit,
  int   Obtain,
  MerLblProdRequest_typ      *LabelItems,
  int	Instance)
{ int   RtnStatus;
  LblApiCntrl_typ	Cntrl;

  TN_Label.Buffer = (void *)LabelItems;
  TN_Label.BufferSize = sizeof(MerLblProdRequest_typ);

  memset(&Cntrl,0,sizeof(LblApiCntrl_typ));
  Cntrl.Instance = Instance;
  Cntrl.FileUnit = Unit;
  Cntrl.Obtain = Obtain;
  Cntrl.ProceedOnError = LBL_TRUE;

  RtnStatus = LblProcessor(&Cntrl, &TN_Label);

  return (RtnStatus);
}

/******************************************************************************
 *				LBL_PRINT_THUMBNAIL_REQUEST
 *
 *****************************************************************************/
void	MerLblPrintThumbnailRequest(
  MerLblProdRequest_typ	*LabelItems)
{
  TN_Label.Buffer = (void *)LabelItems;

  PrintLabelElements( &TN_Label );

  return;
}

/******************************************************************************
 *				LBL_TEST_THUMBNAIL_REQUEST
 *
 *****************************************************************************/
void	MerLblTestThumbnailRequest(
  MerLblProdRequest_typ	*LabelItems)
{
  TN_Label.Buffer = (void *)LabelItems;

  TestLoadLabelElements( &TN_Label );

  return;
}

/*
 * SUBFRAME REQUEST
 */
/******************************************************************************
 *				MER_LBL_SUBFRAME_REQUEST
 *
 *****************************************************************************/
int     MerLblSubframeRequest(
  int   Unit,
  int   Obtain,
  MerLblProdRequest_typ      *LabelItems,
  int	Instance)
{ int   RtnStatus;
  LblApiCntrl_typ	Cntrl;

  SB_Label.Buffer = (void *)LabelItems;
  SB_Label.BufferSize = sizeof(MerLblProdRequest_typ);

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
void	MerLblPrintSubframeRequest(
  MerLblProdRequest_typ	*LabelItems)
{
  SB_Label.Buffer = (void *)LabelItems;

  PrintLabelElements( &SB_Label );

  return;
}

/******************************************************************************
 *				LBL_TEST_SUBFRAME_REQUEST
 *
 *****************************************************************************/
void	MerLblTestSubframeRequest(
  MerLblProdRequest_typ	*LabelItems)
{
  SB_Label.Buffer = (void *)LabelItems;

  TestLoadLabelElements( &SB_Label );

  return;
}

/*
 * ROW_SUM REQUEST
 */
/******************************************************************************
 *				MER_LBL_ROW_SUM_REQUEST
 *
 *****************************************************************************/
int     MerLblRowSumRequest(
  int   Unit,
  int   Obtain,
  MerLblProdRequest_typ      *LabelItems,
  int	Instance)
{ int   RtnStatus;
  LblApiCntrl_typ	Cntrl;

  RS_Label.Buffer = (void *)LabelItems;
  RS_Label.BufferSize = sizeof(MerLblProdRequest_typ);

  memset(&Cntrl,0,sizeof(LblApiCntrl_typ));
  Cntrl.Instance = Instance;
  Cntrl.FileUnit = Unit;
  Cntrl.Obtain = Obtain;
  Cntrl.ProceedOnError = LBL_TRUE;

  RtnStatus = LblProcessor(&Cntrl, &RS_Label);

  return (RtnStatus);
}

/******************************************************************************
 *				LBL_PRINT_ROW_SUM_REQUEST
 *
 *****************************************************************************/
void	MerLblPrintRowSumRequest(
  MerLblProdRequest_typ	*LabelItems)
{
  RS_Label.Buffer = (void *)LabelItems;

  PrintLabelElements( &RS_Label );

  return;
}

/******************************************************************************
 *				LBL_TEST_ROW_SUM_REQUEST
 *
 *****************************************************************************/
void	MerLblTestRowSumRequest(
  MerLblProdRequest_typ	*LabelItems)
{
  RS_Label.Buffer = (void *)LabelItems;

  TestLoadLabelElements( &RS_Label );

  return;
}

/*
 * COLUMN_SUM REQUEST
 */
/******************************************************************************
 *				MER_LBL_COLUMN_SUM_REQUEST
 *
 *****************************************************************************/
int     MerLblColSumRequest(
  int   Unit,
  int   Obtain,
  MerLblProdRequest_typ      *LabelItems,
  int	Instance)
{ int   RtnStatus;
  LblApiCntrl_typ	Cntrl;

  CS_Label.Buffer = (void *)LabelItems;
  CS_Label.BufferSize = sizeof(MerLblProdRequest_typ);

  memset(&Cntrl,0,sizeof(LblApiCntrl_typ));
  Cntrl.Instance = Instance;
  Cntrl.FileUnit = Unit;
  Cntrl.Obtain = Obtain;
  Cntrl.ProceedOnError = LBL_TRUE;

  RtnStatus = LblProcessor(&Cntrl, &CS_Label);

  return (RtnStatus);
}

/******************************************************************************
 *				LBL_PRINT_COLUMN_SUM_REQUEST
 *
 *****************************************************************************/
void	MerLblPrintColSumRequest(
  MerLblProdRequest_typ	*LabelItems)
{
  CS_Label.Buffer = (void *)LabelItems;

  PrintLabelElements( &CS_Label );

  return;
}

/******************************************************************************
 *				LBL_TEST_COLUMN_SUM_REQUEST
 *
 *****************************************************************************/
void	MerLblTestColSumRequest(
  MerLblProdRequest_typ	*LabelItems)
{
  CS_Label.Buffer = (void *)LabelItems;

  TestLoadLabelElements( &CS_Label );

  return;
}

/*
 * SUN_FIND REQUEST
 */
/******************************************************************************
 *				MER_LBL_SUN_FIND_REQUEST
 *
 *****************************************************************************/
int     MerLblSunFindRequest(
  int   Unit,
  int   Obtain,
  MerLblProdRequest_typ      *LabelItems,
  int	Instance)
{ int   RtnStatus;
  LblApiCntrl_typ	Cntrl;

  SF_Label.Buffer = (void *)LabelItems;
  SF_Label.BufferSize = sizeof(MerLblProdRequest_typ);

  memset(&Cntrl,0,sizeof(LblApiCntrl_typ));
  Cntrl.Instance = Instance;
  Cntrl.FileUnit = Unit;
  Cntrl.Obtain = Obtain;
  Cntrl.ProceedOnError = LBL_TRUE;

  RtnStatus = LblProcessor(&Cntrl, &SF_Label);

  return (RtnStatus);
}

/******************************************************************************
 *				LBL_PRINT_SUN_FIND_REQUEST
 *
 *****************************************************************************/
void	MerLblPrintSunFindRequest(
  MerLblProdRequest_typ	*LabelItems)
{
  SF_Label.Buffer = (void *)LabelItems;

  PrintLabelElements( &SF_Label );

  return;
}

/******************************************************************************
 *				LBL_TEST_SUN_FIND_REQUEST
 *
 *****************************************************************************/
void	MerLblTestSunFindRequest(
  MerLblProdRequest_typ	*LabelItems)
{
  SF_Label.Buffer = (void *)LabelItems;

  TestLoadLabelElements( &SF_Label );

  return;
}

/*
 * HISTOGRAM REQUEST
 */
/******************************************************************************
 *				MER_LBL_HISTOGRAM_REQUEST
 *
 *****************************************************************************/
int     MerLblHistogramRequest(
  int   Unit,
  int   Obtain,
  MerLblProdRequest_typ      *LabelItems,
  int	Instance)
{ int   RtnStatus;
  LblApiCntrl_typ	Cntrl;

  HG_Label.Buffer = (void *)LabelItems;
  HG_Label.BufferSize = sizeof(MerLblProdRequest_typ);

  memset(&Cntrl,0,sizeof(LblApiCntrl_typ));
  Cntrl.Instance = Instance;
  Cntrl.FileUnit = Unit;
  Cntrl.Obtain = Obtain;
  Cntrl.ProceedOnError = LBL_TRUE;

  RtnStatus = LblProcessor(&Cntrl, &HG_Label);

  return (RtnStatus);
}

/******************************************************************************
 *				LBL_PRINT_HISTOGRAM_REQUEST
 *
 *****************************************************************************/
void	MerLblPrintHistogramRequest(
  MerLblProdRequest_typ	*LabelItems)
{
  HG_Label.Buffer = (void *)LabelItems;

  PrintLabelElements( &HG_Label );

  return;
}

/******************************************************************************
 *				LBL_TEST_HISTOGRAM_REQUEST
 *
 *****************************************************************************/
void	MerLblTestHistogramRequest(
  MerLblProdRequest_typ	*LabelItems)
{
  HG_Label.Buffer = (void *)LabelItems;

  TestLoadLabelElements( &HG_Label );

  return;
}

