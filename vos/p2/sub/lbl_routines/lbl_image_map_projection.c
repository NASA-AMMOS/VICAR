/**  Copyright (c) 1999, California Institute of Technology             **/
/**  U. S. Government sponsorship under NASA contract is acknowledged   **/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "lbl_image_map_projection.h"

/******************************************************************************
 *				LBL_IMAGE_MAP_PROJECTION
 *
 *	This module contains routines to help create, read/write and print
 *  a Image Map Projection property label.  It is part of the MIPL label API
 *  package, using a lower-level label processor to do the real work.  This
 *  basically defines a table that the lower-level routines use.  The table
 *  is the bridge between how the application access the label elements, and
 *  how the label processor specifies the label components to the VICAR label
 *  Run Time Library (RTL).
 *
 *	The label processor interface structures and routines are defined in
 *  the file "lbl_gen_api.h" (Check the label processor documentation for
 *  how to create APIs like this one).  The application program interface
 *  structures are defined in the file "lbl_image_geometry.h".  The
 *  implementation supporting the interface is this module.
 *
 *	The primary routine used by a typical application program is
 *  LblImageMapProjection.  This routine requires exactly 4 parameters.
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

#define  LBL_SIZE(x)	sizeof(((LblImageMapProjection_typ *)0)->x)

static LblApiElement_typ	LabelTbl[] = {
	{"A_AXIS_RADIUS",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageMapProjection_typ, AAxisRadius.Value),
		LBL_OFFSET(LblImageMapProjection_typ, AAxisRadius.Valid),
		LBL_NO_RETURN,	LBL_SIZE(AAxisRadius.Value)},
	{"A_AXIS_RADIUS__UNIT",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageMapProjection_typ, AAxisRadiusUnit.Value),
		LBL_OFFSET(LblImageMapProjection_typ, AAxisRadiusUnit.Valid),
		LBL_NO_RETURN,	LBL_SIZE(AAxisRadiusUnit.Value)},
	{"B_AXIS_RADIUS",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageMapProjection_typ, BAxisRadius.Value),
		LBL_OFFSET(LblImageMapProjection_typ, BAxisRadius.Valid),
		LBL_NO_RETURN,	LBL_SIZE(AAxisRadius.Value)},
	{"B_AXIS_RADIUS__UNIT",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageMapProjection_typ, BAxisRadiusUnit.Value),
		LBL_OFFSET(LblImageMapProjection_typ, BAxisRadiusUnit.Valid),
		LBL_NO_RETURN,	LBL_SIZE(BAxisRadiusUnit.Value)},
	{"C_AXIS_RADIUS",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageMapProjection_typ, CAxisRadius.Value),
		LBL_OFFSET(LblImageMapProjection_typ, CAxisRadius.Valid),
		LBL_NO_RETURN,	LBL_SIZE(AAxisRadius.Value)},
	{"C_AXIS_RADIUS__UNIT",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageMapProjection_typ, CAxisRadiusUnit.Value),
		LBL_OFFSET(LblImageMapProjection_typ, CAxisRadiusUnit.Valid),
		LBL_NO_RETURN,	LBL_SIZE(CAxisRadiusUnit.Value)},
	{"COORDINATE_SYSTEM_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageMapProjection_typ, CoordinateSystemName.Value),
		LBL_OFFSET(LblImageMapProjection_typ, CoordinateSystemName.Valid),
		LBL_NO_RETURN,	LBL_SIZE(CoordinateSystemName.Value)},
	{"MAP_PROJECTION_TYPE",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageMapProjection_typ, MapProjectionType.Value),
		LBL_OFFSET(LblImageMapProjection_typ, MapProjectionType.Valid),
		LBL_NO_RETURN,	LBL_SIZE(MapProjectionType.Value)},
	{"LINE_PROJECTION_OFFSET",			"DOUB",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageMapProjection_typ, LineProjectionOffset.Value),
		LBL_OFFSET(LblImageMapProjection_typ, LineProjectionOffset.Valid),
		LBL_NO_RETURN,	LBL_SIZE(LineProjectionOffset.Value)},
	{"SAMPLE_PROJECTION_OFFSET",			"DOUB",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageMapProjection_typ, SampleProjectionOffset.Value),
		LBL_OFFSET(LblImageMapProjection_typ, SampleProjectionOffset.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SampleProjectionOffset.Value)},
	{"MAP_SCALE",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageMapProjection_typ, MapScale.Value),
		LBL_OFFSET(LblImageMapProjection_typ, MapScale.Valid),
		LBL_NO_RETURN,	LBL_SIZE(MapScale.Value)},
	{"MAP_SCALE__UNIT",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageMapProjection_typ, MapScaleUnit.Value),
		LBL_OFFSET(LblImageMapProjection_typ, MapScaleUnit.Valid),
		LBL_NO_RETURN,	LBL_SIZE(MapScaleUnit.Value)},
//	{"DATA_SET_MAP_PROJECTION",		"STRING",	LBL_OPTIONAL,
//		LBL_NO_CONT,	1,	1,	LBL_NULL,
//		LBL_OFFSET(LblImageMapProjection_typ, DataSetMapProjection.Value),
//		LBL_OFFSET(LblImageMapProjection_typ, DataSetMapProjection.Valid),
//		LBL_NO_RETURN,	LBL_SIZE(DataSetMapProjection.Value)},
	{"CENTER_LATITUDE",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageMapProjection_typ, CenterLatitude.Value),
		LBL_OFFSET(LblImageMapProjection_typ, CenterLatitude.Valid),
		LBL_NO_RETURN,	LBL_SIZE(CenterLatitude.Value)},
	{"CENTER_LONGITUDE",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageMapProjection_typ, CenterLongitude.Value),
		LBL_OFFSET(LblImageMapProjection_typ, CenterLongitude.Valid),
		LBL_NO_RETURN,	LBL_SIZE(CenterLongitude.Value)},
	{"COORDINATE_SYSTEM_TYPE",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageMapProjection_typ, CoordinateSystemType.Value),
		LBL_OFFSET(LblImageMapProjection_typ, CoordinateSystemType.Valid),
		LBL_NO_RETURN,	LBL_SIZE(CoordinateSystemType.Value)},
	{"LINE_FIRST_PIXEL",	"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageMapProjection_typ, LineFirstPixel.Value),
		LBL_OFFSET(LblImageMapProjection_typ, LineFirstPixel.Valid),
		LBL_NO_RETURN,	LBL_SIZE(LineFirstPixel.Value)},
	{"LINE_LAST_PIXEL",	"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageMapProjection_typ, LineLastPixel.Value),
		LBL_OFFSET(LblImageMapProjection_typ, LineLastPixel.Valid),
		LBL_NO_RETURN,	LBL_SIZE(LineLastPixel.Value)},
	{"MAP_PROJECTION_ROTATION",			"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageMapProjection_typ, MapProjectionRotation.Value),
		LBL_OFFSET(LblImageMapProjection_typ, MapProjectionRotation.Valid),
		LBL_NO_RETURN,	LBL_SIZE(MapProjectionRotation.Value)},
	{"MAP_RESOLUTION",			"DOUB",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageMapProjection_typ, MapResolution.Value),
		LBL_OFFSET(LblImageMapProjection_typ, MapResolution.Valid),
		LBL_NO_RETURN,	LBL_SIZE(MapResolution.Value)},
	{"MAP_RESOLUTION__UNIT",			"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageMapProjection_typ, MapResolutionUnit.Value),
		LBL_OFFSET(LblImageMapProjection_typ, MapResolutionUnit.Valid),
		LBL_NO_RETURN,	LBL_SIZE(MapResolutionUnit.Value)},
	{"POSITIVE_LONGITUDE_DIRECTION",			"STRING",	LBL_REQUIRED,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageMapProjection_typ, PositiveLongitudeDirection.Value),
		LBL_OFFSET(LblImageMapProjection_typ, PositiveLongitudeDirection.Valid),
		LBL_NO_RETURN,	LBL_SIZE(PositiveLongitudeDirection.Value)},
	{"SAMPLE_FIRST_PIXEL",	"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageMapProjection_typ, SampleFirstPixel.Value),
		LBL_OFFSET(LblImageMapProjection_typ, SampleFirstPixel.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SampleFirstPixel.Value)},
	{"SAMPLE_LAST_PIXEL",	"INT",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageMapProjection_typ, SampleLastPixel.Value),
		LBL_OFFSET(LblImageMapProjection_typ, SampleLastPixel.Valid),
		LBL_NO_RETURN,	LBL_SIZE(SampleLastPixel.Value)},
	{"MINIMUM_LATITUDE",			"DOUB",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageMapProjection_typ, MinimumLatitude.Value),
		LBL_OFFSET(LblImageMapProjection_typ, MinimumLatitude.Valid),
		LBL_NO_RETURN,	LBL_SIZE(MinimumLatitude.Value)},
	{"MAXIMUM_LATITUDE",			"DOUB",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageMapProjection_typ, MaximumLatitude.Value),
		LBL_OFFSET(LblImageMapProjection_typ, MaximumLatitude.Valid),
		LBL_NO_RETURN,	LBL_SIZE(MaximumLatitude.Value)},
	{"WESTERNMOST_LONGITUDE",			"DOUB",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageMapProjection_typ, WesternmostLongitude.Value),
		LBL_OFFSET(LblImageMapProjection_typ, WesternmostLongitude.Valid),
		LBL_NO_RETURN,	LBL_SIZE(WesternmostLongitude.Value)},
	{"EASTERNMOST_LONGITUDE",			"DOUB",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblImageMapProjection_typ, EasternmostLongitude.Value),
		LBL_OFFSET(LblImageMapProjection_typ, EasternmostLongitude.Valid),
		LBL_NO_RETURN,	LBL_SIZE(EasternmostLongitude.Value)},

	{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}};

static LblApiProcess_typ	Label = {
	LabelTbl,       "PROPERTY",     "PROPERTY",     LBL_PDS_STRING_NULL,
	LBL_NULL };

/******************************************************************************
 *				LBL_SET_IMAGE_MAP_PROJECTION
 *
 *****************************************************************************/
void     LblSetImageMapProjection(
  const char	*Name )
{
  Label.NameValue = Name;
  return;
}

/******************************************************************************
 *				LBL_IMAGE_MAP_PROJECTION
 *
 *****************************************************************************/
int     LblImageMapProjection(
  int   Unit,
  int   Obtain,
  LblImageMapProjection_typ      *LabelItems,
  int Instance)
{
  LblSetImageMapProjection("IMAGE_MAP_PROJECTION");
  return (LblImageMapProjectionApi(Unit,Obtain,LabelItems,Instance));
}

/******************************************************************************
 *				LBL_IMAGE_MAP_PROJECTION_PARMS
 *
 *****************************************************************************/
int     LblImageMapProjectionParms(
  int   Unit,
  int   Obtain,
  LblImageMapProjection_typ      *LabelItems,
  int Instance)
{
  LblSetImageMapProjection("IMAGE_MAP_PROJECTION_PARMS");
  return (LblImageMapProjectionApi(Unit,Obtain,LabelItems,Instance));
}

/******************************************************************************
 *				LBL_IMAGE_MAP_PROJECTION_API
 *
 *****************************************************************************/
int     LblImageMapProjectionApi(
  int   Unit,
  int   Obtain,
  LblImageMapProjection_typ      *LabelItems,
  int Instance)
{ int   RtnStatus;
  LblApiCntrl_typ	Cntrl;

  Label.Buffer = (void *)LabelItems;
  Label.BufferSize = sizeof(LblImageMapProjection_typ);

  memset(&Cntrl,0,sizeof(LblApiCntrl_typ));
  Cntrl.Instance = Instance;
  Cntrl.FileUnit = Unit;
  Cntrl.Obtain = Obtain;
  Cntrl.ProceedOnError = LBL_TRUE;

  RtnStatus = LblProcessor(&Cntrl, &Label);

  return (RtnStatus);
}

/******************************************************************************
 *				LBL_PRINT_IMAGE_MAP_PROJECTION
 *
 *****************************************************************************/
void	LblPrintImageMapProjection(
  LblImageMapProjection_typ	*LabelItems)
{
  Label.Buffer = (void *)LabelItems;

  PrintLabelElements( &Label );

  return;
}

/******************************************************************************
 *				LBL_TEST_IMAGE_MAP_PROJECTION
 *
 *****************************************************************************/
void	LblTestImageMapProjection(
  LblImageMapProjection_typ	*LabelItems)
{
  Label.Buffer = (void *)LabelItems;

  TestLoadLabelElements( &Label );

  return;
}
