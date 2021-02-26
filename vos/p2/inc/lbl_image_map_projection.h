#ifndef MIPS_LBL_IMAGE_MAP_PROJECTION_INCLUDED
#define MIPS_LBL_IMAGE_MAP_PROJECTION_INCLUDED 1

#ifdef __cplusplus
extern "C" {
#endif

/**  Copyright (c) 1999, California Institute of Technology             **/
/**  U. S. Government sponsorship under NASA contract is acknowledged   **/

#include "lbl_gen_api.h"

/******************************************************************************
 *				LBL_IMAGE_MAP_PROJECTION
 *
 *	This module contains routines to help create, read/write and print
 *  a Image map projection property label.  It is part of the MIPL label API
 *  package, using a lower-level label processor to do the real work.  This
 *  package basically defines a table that the lower-level routines use.  The
 *  table is the bridge between how the application access the label elements,
 *  and how the label processor specifies the label components to the VICAR
 *  label Run Time Library (RTL).
 *
 *	The primary routine used by a typical application program is
 *  LblSurfaceProjection.  This routine requires exactly 4 parameters.
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
 *============================================================================
 * History of modifications:
 * ------------------------
 * Date         Who             Description
 * -----------  --------------- ---------------------------------------------
 * 2018-09-19   F. Ayoub        Initiated the file - highly based on surface
 *                              projection lablel
 *****************************************************************************/

typedef struct
	{
	LblApiRealItem_typ		AAxisRadius;
	LblApiTypeItem_typ		AAxisRadiusUnit;
	LblApiRealItem_typ		BAxisRadius;
	LblApiTypeItem_typ		BAxisRadiusUnit;
	LblApiRealItem_typ		CAxisRadius;
	LblApiTypeItem_typ		CAxisRadiusUnit;
	LblApiNameItem_typ		CoordinateSystemName;
	LblApiTypeItem_typ		MapProjectionType;
	LblApiDoubleItem_typ		LineProjectionOffset;
	LblApiDoubleItem_typ		SampleProjectionOffset;
	LblApiRealItem_typ		MapScale;
	LblApiTypeItem_typ		MapScaleUnit;
//	LblApiNameItem_typ		DataSetMapProjection;
	LblApiRealItem_typ		CenterLatitude;
	LblApiRealItem_typ		CenterLongitude;
	LblApiNameItem_typ		CoordinateSystemType;
	LblApiIntItem_typ		LineFirstPixel;
	LblApiIntItem_typ		LineLastPixel;
	LblApiRealItem_typ		MapProjectionRotation;
	LblApiDoubleItem_typ		MapResolution;
	LblApiTypeItem_typ		MapResolutionUnit;
	LblApiTypeItem_typ		PositiveLongitudeDirection;
	LblApiIntItem_typ		SampleFirstPixel;
	LblApiIntItem_typ		SampleLastPixel;
	LblApiDoubleItem_typ		MinimumLatitude;
	LblApiDoubleItem_typ		MaximumLatitude;
	LblApiDoubleItem_typ		WesternmostLongitude;
	LblApiDoubleItem_typ		EasternmostLongitude;
	} LblImageMapProjection_typ;

/***  Function Prototypes  ***/
int	LblImageMapProjection( int, int, LblImageMapProjection_typ *, int );
int	LblImageMapProjectionParms( int, int, LblImageMapProjection_typ *, int );
	/***  For development & internal use  ***/
int	LblImageMapProjectionApi( int, int, LblImageMapProjection_typ *, int );
void	LblSetImageMapProjection( const char * );
void	LblTestImageMapProjection( LblImageMapProjection_typ *);
void	LblPrintImageMapProjection( LblImageMapProjection_typ *);

#ifdef __cplusplus
}
#endif

#endif
