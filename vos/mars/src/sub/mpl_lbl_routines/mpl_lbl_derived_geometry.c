/**  Copyright (c) 1995, California Institute of Technology             **/
/**  U. S. Government sponsorship under NASA contract is acknowledged   **/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#pragma set woff 1032

#include "mpl_lbl_derived_geometry.h"

/******************************************************************************
 *				MPL_LBL_DERIVED_GEOMETRY
 *
 *	This module contains routines to help create, read/write and print
 *  a Derived Geometry label.  It is part of the MIPL label API package,
 *  using a lower-level label processor to do the real work.  This package
 *  basically defines a table that the lower-level routines use.  The table
 *  is the bridge between how the application access the label elements, and
 *  how the label processor specifies the label components to the VICAR label
 *  Run Time Library (RTL).
 *
 *	The label processor interface structures and routines are defined in
 *  the file "lbl_gen_api.h" (Check the label processor documentation for
 *  how to create APIs like this one).  The application program interface
 *  structures are defined in the file "lbl_derived_geometry.h".  The
 *  implementation supporting the interface is this module.
 *
 *	The primary routine used by a typical application program is
 *  MplLblDerivedGeometry.  This routine requires exactly 4 parameters.
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

#define  LBL_SIZE(x)	sizeof(((MplLblDerivedGeometry_typ *)0)->x)

static LblApiElement_typ	LabelTbl[] = {
	{"MVACS_INSTRUMENT_AZIMUTH",		"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(MplLblDerivedGeometry_typ, MvacsInstrumentAzimuth.Value),
		LBL_OFFSET(MplLblDerivedGeometry_typ, MvacsInstrumentAzimuth.Valid),
		LBL_NO_RETURN,	LBL_SIZE(MvacsInstrumentAzimuth.Value)},

	{"MVACS_INSTRUMENT_ELEVATION",		"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(MplLblDerivedGeometry_typ, MvacsInstrumentElevation.Value),
		LBL_OFFSET(MplLblDerivedGeometry_typ, MvacsInstrumentElevation.Valid),
		LBL_NO_RETURN,	LBL_SIZE(MvacsInstrumentElevation.Value)},

	{"MVACS_INSTRUMENT_POSITION",	"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	3,	1,	LBL_NULL,
		LBL_OFFSET(MplLblDerivedGeometry_typ, MvacsInstrumentPosition.Value),
		LBL_OFFSET(MplLblDerivedGeometry_typ, MvacsInstrumentPosition.Valid),
		LBL_NO_RETURN,	LBL_SIZE(MvacsInstrumentPosition.Value)},

	{"MVACS_LOCAL_LEVEL_QUATERNION",	"REAL",		LBL_OPTIONAL,
		LBL_NO_CONT,	4,	1,	LBL_NULL,
		LBL_OFFSET(MplLblDerivedGeometry_typ, MvacsLocalLevelQuaternion.Value),
		LBL_OFFSET(MplLblDerivedGeometry_typ, MvacsLocalLevelQuaternion.Valid),
		LBL_NO_RETURN,	LBL_SIZE(MvacsLocalLevelQuaternion.Value)},

	{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}};

static LblApiProcess_typ	Label = {
	LabelTbl,       "PROPERTY",     "PROPERTY",     "DERIVED_GEOMETRY",
	LBL_NULL };

/******************************************************************************
 *				MPL_LBL_DERIVED_GEOMETRY
 *
 *****************************************************************************/
int     MplLblDerivedGeometry(
  int   Unit,
  int   Obtain,
  MplLblDerivedGeometry_typ      *LabelItems,
  int	Instance )
{ int   RtnStatus;
  LblApiCntrl_typ	Cntrl;

  Label.Buffer = (void *)LabelItems;
  Label.BufferSize = sizeof(MplLblDerivedGeometry_typ);


  memset(&Cntrl,0,sizeof(LblApiCntrl_typ));
  Cntrl.Instance = Instance;
  Cntrl.FileUnit = Unit;
  Cntrl.Obtain = Obtain;
  Cntrl.ProceedOnError = LBL_TRUE;

  RtnStatus = LblProcessor(&Cntrl, &Label);

  return (RtnStatus);
}

/******************************************************************************
 *				LBL_PRINT_DERIVED_GEOMETRY
 *
 *****************************************************************************/
void	MplLblPrintDerivedGeometry(
  MplLblDerivedGeometry_typ	*LabelItems)
{
  Label.Buffer = (void *)LabelItems;

  PrintLabelElements( &Label );

  return;
}

/******************************************************************************
 *				LBL_TEST_DERIVED_GEOMETRY
 *
 *****************************************************************************/
void	MplLblTestDerivedGeometry(
  MplLblDerivedGeometry_typ	*LabelItems)
{
  Label.Buffer = (void *)LabelItems;

  TestLoadLabelElements( &Label );

  return;
}
