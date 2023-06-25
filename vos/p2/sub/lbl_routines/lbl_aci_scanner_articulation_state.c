/**  Copyright (c) 2023, California Institute of Technology             **/
/**  U. S. Government sponsorship under NASA contract is acknowledged   **/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "lbl_aci_scanner_articulation_state.h"

/******************************************************************************
 *				LBL_ACI_SCANNER_ARTICULATION_STATE
 *
 *	This module contains routines to help create, read/write and print an ACI
 *  Scanner Articulation property label.  It is part of the MIPL label API,
 *  using a lower-level label processor to do the real work.  This package
 *  basically defines a table that the lower-level routines use.  The table
 *  is the bridge between how the application access the label elements, and
 *  how the label processor specifies the label components to the VICAR label
 *  Run Time Library (RTL).
 *
 *	The label processor interface structures and routines are defined in
 *  the file "lbl_gen_api.h" (Check the label processor documentation for
 *  how to create APIs like this one).  The application program interface
 *  structures are defined in the file "lbl_articulation.h".  The
 *  implementation supporting the interface is this module.
 *
 *	The primary routine used by a typical application program is
 *  LblAciScannerArticulationState.  This routine requires exactly 4 parameters.
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
 * 2022-02-23   M. Hess-Flores  First version of the file
 *****************************************************************************/

#define  LBL_SIZE(x)	sizeof(((LblAciScannerArticulationState_typ *)0)->x)

static LblApiElement_typ	LabelTbl[] = {
	{"ART_DEV_COMPONENT_STATE_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblAciScannerArticulationState_typ, ArticulationDeviceComponentStateName.Value),
		LBL_OFFSET(LblAciScannerArticulationState_typ, ArticulationDeviceComponentStateName.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceComponentStateName.Value)},

    {"ART_DEV_COMPONENT_STATE",     "STRING",       LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,      LBL_NULL,
        LBL_OFFSET(LblAciScannerArticulationState_typ, ArticulationDeviceComponentState.Value),
        LBL_OFFSET(LblAciScannerArticulationState_typ, ArticulationDeviceComponentState.Valid),
        LBL_NO_RETURN,  LBL_SIZE(ArticulationDeviceComponentState.Value)},

	{"ARTICULATION_DEVICE_ID",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblAciScannerArticulationState_typ, ArticulationDeviceId.Value),
		LBL_OFFSET(LblAciScannerArticulationState_typ, ArticulationDeviceId.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceId.Value)},

	{"ARTICULATION_DEVICE_NAME",		"STRING",	LBL_OPTIONAL,
		LBL_NO_CONT,	1,	1,	LBL_NULL,
		LBL_OFFSET(LblAciScannerArticulationState_typ, ArticulationDeviceName.Value),
		LBL_OFFSET(LblAciScannerArticulationState_typ, ArticulationDeviceName.Valid),
		LBL_NO_RETURN,	LBL_SIZE(ArticulationDeviceName.Value)},

	{0, 0, 0, 0, 0, 0, 0, 0, 0, 0}};

static LblApiProcess_typ	Label = {
	LabelTbl,       "PROPERTY",     "PROPERTY",	LBL_PDS_STRING_NULL,
	LBL_NULL };

/******************************************************************************
 *				LBL_SET_ACI_SCANNER_ARTICULATION_STATE
 *
 *****************************************************************************/
void     LblSetAciScannerArticulationState(
  const char	*Name )
{
  if (Name!=NULL)
    Label.NameValue = Name;
  return;
}

/******************************************************************************
 *				LBL_ACI_SCANNER_ARTICULATION_STATE
 *
 *****************************************************************************/
int     LblAciScannerArticulationState(
  int   Unit,
  int   Obtain,
  LblAciScannerArticulationState_typ      *LabelItems,
  int	Instance)
{
  LblSetAciScannerArticulationState("ACI_SCANNER_ARTICULATION_STATE");
  return (LblAciScannerArticulationStateApi(Unit,Obtain,LabelItems,Instance,(const char*)NULL));
}

/******************************************************************************
 *				LBL_ACI_SCANNER_ARTICULATION_STATE_API
 *
 *****************************************************************************/
int     LblAciScannerArticulationStateApi(
  int   Unit,
  int   Obtain,
  LblAciScannerArticulationState_typ      *LabelItems,
  int	Instance,
  const char* propertyName)
{ int   RtnStatus;
  LblApiCntrl_typ	Cntrl;
  LblSetAciScannerArticulationState(propertyName);
  Label.Buffer = (void *)LabelItems;
  Label.BufferSize = sizeof(LblAciScannerArticulationState_typ);

  memset(&Cntrl,0,sizeof(LblApiCntrl_typ));
  Cntrl.Instance = Instance;
  Cntrl.FileUnit = Unit;
  Cntrl.Obtain = Obtain;
  Cntrl.ProceedOnError = LBL_TRUE;

  RtnStatus = LblProcessor(&Cntrl, &Label);

  return (RtnStatus);
}

/******************************************************************************
 *				LBL_PRINT_ACI_SCANNER_ARTICULATION_STATE
 *
 *****************************************************************************/
void	LblPrintAciScannerArticulationState(
  LblAciScannerArticulationState_typ	*LabelItems)
{
  Label.Buffer = (void *)LabelItems;

  PrintLabelElements( &Label );

  return;
}

/******************************************************************************
 *				LBL_TEST_ACI_SCANNER_ARTICULATION_STATE
 *
 *****************************************************************************/
void	LblTestAciScannerArticulationState(
  LblAciScannerArticulationState_typ	*LabelItems)
{
  Label.Buffer = (void *)LabelItems;

  TestLoadLabelElements( &Label );

  return;
}
