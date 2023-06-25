#ifndef MIPS_LBL_ACI_SCANNER_ARTICULATION_STATE_INCLUDED
#define MIPS_LBL_ACI_SCANNER_ARTICULATION_STATE_INCLUDED 1

#ifdef __cplusplus
extern "C" {
#endif

/**  Copyright (c) 2023, California Institute of Technology             **/
/**  U. S. Government sponsorship under NASA contract is acknowledged   **/

#include "lbl_gen_api.h"

/******************************************************************************
 *				LBL_ACI_SCANNER_ARTICULATION_STATE
 *
 *	This module contains routines to help create, read/write and print
 *  a Articulation property label for ACI.  It is part of the MIPL label API
 *  package, using a lower-level label processor to do the real work.  This 
 *  package basically defines a table that the lower-level routines use.  The 
 *  table is the bridge between how the application access the label elements, 
 *  and how the label processor specifies the label components to the VICAR 
 *  label Run Time Library (RTL).
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
 * 2023-02-23   M. Hess-Flores  First version of the file
 *****************************************************************************/
#define ACI_SCANNER_ARTICULATION_STATE_PROPERTY_NAME "ACI_SCANNER_ARTICULATION_STATE"

typedef struct
	{
    LblApiNameItem_typ      ArticulationDeviceComponentStateName;
    LblApiIdItem_typ        ArticulationDeviceComponentState;
	LblApiIdItem_typ		ArticulationDeviceId;
	LblApiNameItem_typ		ArticulationDeviceName;
	} LblAciScannerArticulationState_typ;

/***  Function prototypes  ***/
int	LblAciScannerArticulationState( int, int, LblAciScannerArticulationState_typ *, int );
/***  For development & internal use  ***/
int	LblAciScannerArticulationStateApi( int, int, LblAciScannerArticulationState_typ *, int , const char*);
void	LblSetAciScannerArticulationState( const char * );
void	LblTestAciScannerArticulationState( LblAciScannerArticulationState_typ 
*);
void	LblPrintAciScannerArticulationState( LblAciScannerArticulationState_typ 
*);

#ifdef __cplusplus
}
#endif

#endif
