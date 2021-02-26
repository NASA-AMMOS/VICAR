/**  Copyright (c) 1995, California Institute of Technology             **/
/**  U. S. Government sponsorship under NASA contract is acknowledged   **/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "lbl_illumination_parms.h"

/******************************************************************************
 *				LBL_ILLUMINATION_PARMS
 *
 *	This module contains routines to help create, read/write and print
 *  an PIXL MCC Img Parms property label.  It is part of the MIPL label API package,
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
 *  LblIlluminationParms.  This routine requires exactly 4 parameters.
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
 * 2019-03-07   H. Lee          Initial version 
 *****************************************************************************/

#define  LBL_SIZE(x)	sizeof(((LblIlluminationParms_typ *)0)->x)

static LblApiElement_typ	LabelTbl[] = {
    {"ILLUMINATION_DEVICE_NAME",             "STRING",   LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, IlluminationDeviceName.Value),
        LBL_OFFSET(LblIlluminationParms_typ, IlluminationDeviceName.Valid),
        LBL_NO_RETURN,  LBL_SIZE(IlluminationDeviceName.Value)},

    {"ILLUMINATION_LED_NAME",               "STRING",   LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, IlluminationLedName[0].Value),
        LBL_OFFSET(LblIlluminationParms_typ, IlluminationLedName[0].Valid),
        LBL_NO_RETURN,  LBL_SIZE(IlluminationLedName[0].Value)},

    {"ILLUMINATION_LED_NAME",               "STRING",   LBL_OPTIONAL,
        LBL_NO_CONT,    1,  2,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, IlluminationLedName[1].Value),
        LBL_OFFSET(LblIlluminationParms_typ, IlluminationLedName[1].Valid),
        LBL_NO_RETURN,  LBL_SIZE(IlluminationLedName[1].Value)},

    {"ILLUMINATION_LED_NAME",               "STRING",   LBL_OPTIONAL,
        LBL_NO_CONT,    1,  3,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, IlluminationLedName[2].Value),
        LBL_OFFSET(LblIlluminationParms_typ, IlluminationLedName[2].Valid),
        LBL_NO_RETURN,  LBL_SIZE(IlluminationLedName[2].Value)},

    {"ILLUMINATION_LED_NAME",               "STRING",   LBL_OPTIONAL,
        LBL_NO_CONT,    1,  4,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, IlluminationLedName[3].Value),
        LBL_OFFSET(LblIlluminationParms_typ, IlluminationLedName[3].Valid),
        LBL_NO_RETURN,  LBL_SIZE(IlluminationLedName[3].Value)},

    {"ILLUMINATION_LED_NAME",               "STRING",   LBL_OPTIONAL,
        LBL_NO_CONT,    1,  5,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, IlluminationLedName[4].Value),
        LBL_OFFSET(LblIlluminationParms_typ, IlluminationLedName[4].Valid),
        LBL_NO_RETURN,  LBL_SIZE(IlluminationLedName[4].Value)},

    {"ILLUMINATION_LED_NAME",               "STRING",   LBL_OPTIONAL,
        LBL_NO_CONT,    1,  6,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, IlluminationLedName[5].Value),
        LBL_OFFSET(LblIlluminationParms_typ, IlluminationLedName[5].Valid),
        LBL_NO_RETURN,  LBL_SIZE(IlluminationLedName[5].Value)},

    {"ILLUMINATION_LED_NAME",               "STRING",   LBL_OPTIONAL,
        LBL_NO_CONT,    1,  7,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, IlluminationLedName[6].Value),
        LBL_OFFSET(LblIlluminationParms_typ, IlluminationLedName[6].Valid),
        LBL_NO_RETURN,  LBL_SIZE(IlluminationLedName[6].Value)},

    {"ILLUMINATION_LED_NAME",               "STRING",   LBL_OPTIONAL,
        LBL_NO_CONT,    1,  8,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, IlluminationLedName[7].Value),
        LBL_OFFSET(LblIlluminationParms_typ, IlluminationLedName[7].Valid),
        LBL_NO_RETURN,  LBL_SIZE(IlluminationLedName[7].Value)},

    {"ILLUMINATION_LED_NAME",               "STRING",   LBL_OPTIONAL,
        LBL_NO_CONT,    1,  9,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, IlluminationLedName[8].Value),
        LBL_OFFSET(LblIlluminationParms_typ, IlluminationLedName[8].Valid),
        LBL_NO_RETURN,  LBL_SIZE(IlluminationLedName[8].Value)},

    {"ILLUMINATION_LED_NAME",               "STRING",   LBL_OPTIONAL,
        LBL_NO_CONT,    1,  10,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, IlluminationLedName[9].Value),
        LBL_OFFSET(LblIlluminationParms_typ, IlluminationLedName[9].Valid),
        LBL_NO_RETURN,  LBL_SIZE(IlluminationLedName[9].Value)},

    {"ILLUMINATION_LED_NAME",               "STRING",   LBL_OPTIONAL,
        LBL_NO_CONT,    1,  11,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, IlluminationLedName[10].Value),
        LBL_OFFSET(LblIlluminationParms_typ, IlluminationLedName[10].Valid),
        LBL_NO_RETURN,  LBL_SIZE(IlluminationLedName[10].Value)},

    {"ILLUMINATION_LED_NAME",               "STRING",   LBL_OPTIONAL,
        LBL_NO_CONT,    1,  12,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, IlluminationLedName[11].Value),
        LBL_OFFSET(LblIlluminationParms_typ, IlluminationLedName[11].Valid),
        LBL_NO_RETURN,  LBL_SIZE(IlluminationLedName[11].Value)},

    {"ILLUMINATION_LED_NAME",               "STRING",   LBL_OPTIONAL,
        LBL_NO_CONT,    1,  13,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, IlluminationLedName[12].Value),
        LBL_OFFSET(LblIlluminationParms_typ, IlluminationLedName[12].Valid),
        LBL_NO_RETURN,  LBL_SIZE(IlluminationLedName[12].Value)},

    {"ILLUMINATION_LED_NAME",               "STRING",   LBL_OPTIONAL,
        LBL_NO_CONT,    1,  14,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, IlluminationLedName[13].Value),
        LBL_OFFSET(LblIlluminationParms_typ, IlluminationLedName[13].Valid),
        LBL_NO_RETURN,  LBL_SIZE(IlluminationLedName[13].Value)},

    {"ILLUMINATION_LED_STATE",              "STRING",   LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, IlluminationLedState[0].Value),
        LBL_OFFSET(LblIlluminationParms_typ, IlluminationLedState[0].Valid),
        LBL_NO_RETURN,  LBL_SIZE(IlluminationLedState[0].Value)},

    {"ILLUMINATION_LED_STATE",              "STRING",   LBL_OPTIONAL,
        LBL_NO_CONT,    1,  2,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, IlluminationLedState[1].Value),
        LBL_OFFSET(LblIlluminationParms_typ, IlluminationLedState[1].Valid),
        LBL_NO_RETURN,  LBL_SIZE(IlluminationLedState[1].Value)},

    {"ILLUMINATION_LED_STATE",              "STRING",   LBL_OPTIONAL,
        LBL_NO_CONT,    1,  3,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, IlluminationLedState[2].Value),
        LBL_OFFSET(LblIlluminationParms_typ, IlluminationLedState[2].Valid),
        LBL_NO_RETURN,  LBL_SIZE(IlluminationLedState[2].Value)},

    {"ILLUMINATION_LED_STATE",              "STRING",   LBL_OPTIONAL,
        LBL_NO_CONT,    1,  4,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, IlluminationLedState[3].Value),
        LBL_OFFSET(LblIlluminationParms_typ, IlluminationLedState[3].Valid),
        LBL_NO_RETURN,  LBL_SIZE(IlluminationLedState[3].Value)},

    {"ILLUMINATION_LED_STATE",              "STRING",   LBL_OPTIONAL,
        LBL_NO_CONT,    1,  5,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, IlluminationLedState[4].Value),
        LBL_OFFSET(LblIlluminationParms_typ, IlluminationLedState[4].Valid),
        LBL_NO_RETURN,  LBL_SIZE(IlluminationLedState[4].Value)},

    {"ILLUMINATION_LED_STATE",              "STRING",   LBL_OPTIONAL,
        LBL_NO_CONT,    1,  6,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, IlluminationLedState[5].Value),
        LBL_OFFSET(LblIlluminationParms_typ, IlluminationLedState[5].Valid),
        LBL_NO_RETURN,  LBL_SIZE(IlluminationLedState[5].Value)},

    {"ILLUMINATION_LED_STATE",              "STRING",   LBL_OPTIONAL,
        LBL_NO_CONT,    1,  7,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, IlluminationLedState[6].Value),
        LBL_OFFSET(LblIlluminationParms_typ, IlluminationLedState[6].Valid),
        LBL_NO_RETURN,  LBL_SIZE(IlluminationLedState[6].Value)},

    {"ILLUMINATION_LED_STATE",              "STRING",   LBL_OPTIONAL,
        LBL_NO_CONT,    1,  8,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, IlluminationLedState[7].Value),
        LBL_OFFSET(LblIlluminationParms_typ, IlluminationLedState[7].Valid),
        LBL_NO_RETURN,  LBL_SIZE(IlluminationLedState[7].Value)},

    {"ILLUMINATION_LED_STATE",              "STRING",   LBL_OPTIONAL,
        LBL_NO_CONT,    1,  9,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, IlluminationLedState[8].Value),
        LBL_OFFSET(LblIlluminationParms_typ, IlluminationLedState[8].Valid),
        LBL_NO_RETURN,  LBL_SIZE(IlluminationLedState[8].Value)},

    {"ILLUMINATION_LED_STATE",              "STRING",   LBL_OPTIONAL,
        LBL_NO_CONT,    1,  10,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, IlluminationLedState[9].Value),
        LBL_OFFSET(LblIlluminationParms_typ, IlluminationLedState[9].Valid),
        LBL_NO_RETURN,  LBL_SIZE(IlluminationLedState[9].Value)},

    {"ILLUMINATION_LED_STATE",              "STRING",   LBL_OPTIONAL,
        LBL_NO_CONT,    1,  11,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, IlluminationLedState[10].Value),
        LBL_OFFSET(LblIlluminationParms_typ, IlluminationLedState[10].Valid),
        LBL_NO_RETURN,  LBL_SIZE(IlluminationLedState[10].Value)},

    {"ILLUMINATION_LED_STATE",              "STRING",   LBL_OPTIONAL,
        LBL_NO_CONT,    1,  12,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, IlluminationLedState[11].Value),
        LBL_OFFSET(LblIlluminationParms_typ, IlluminationLedState[11].Valid),
        LBL_NO_RETURN,  LBL_SIZE(IlluminationLedState[11].Value)},

    {"ILLUMINATION_LED_STATE",              "STRING",   LBL_OPTIONAL,
        LBL_NO_CONT,    1,  13,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, IlluminationLedState[12].Value),
        LBL_OFFSET(LblIlluminationParms_typ, IlluminationLedState[12].Valid),
        LBL_NO_RETURN,  LBL_SIZE(IlluminationLedState[12].Value)},

    {"ILLUMINATION_LED_STATE",              "STRING",   LBL_OPTIONAL,
        LBL_NO_CONT,    1,  14,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, IlluminationLedState[13].Value),
        LBL_OFFSET(LblIlluminationParms_typ, IlluminationLedState[13].Valid),
        LBL_NO_RETURN,  LBL_SIZE(IlluminationLedState[13].Value)},

    {"ILLUM_LED_WAVELENGTH",                 "INT",      LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, IllumLedWaveLength[0].Value),
        LBL_OFFSET(LblIlluminationParms_typ, IllumLedWaveLength[0].Valid),
        LBL_NO_RETURN,  LBL_SIZE(IllumLedWaveLength[0].Value)},

    {"ILLUM_LED_WAVELENGTH",                 "INT",      LBL_OPTIONAL,
        LBL_NO_CONT,    1,  2,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, IllumLedWaveLength[1].Value),
        LBL_OFFSET(LblIlluminationParms_typ, IllumLedWaveLength[1].Valid),
        LBL_NO_RETURN,  LBL_SIZE(IllumLedWaveLength[1].Value)},

    {"ILLUM_LED_WAVELENGTH",                 "INT",      LBL_OPTIONAL,
        LBL_NO_CONT,    1,  3,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, IllumLedWaveLength[2].Value),
        LBL_OFFSET(LblIlluminationParms_typ, IllumLedWaveLength[2].Valid),
        LBL_NO_RETURN,  LBL_SIZE(IllumLedWaveLength[2].Value)},

    {"ILLUM_LED_WAVELENGTH",                 "INT",      LBL_OPTIONAL,
        LBL_NO_CONT,    1,  4,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, IllumLedWaveLength[3].Value),
        LBL_OFFSET(LblIlluminationParms_typ, IllumLedWaveLength[3].Valid),
        LBL_NO_RETURN,  LBL_SIZE(IllumLedWaveLength[3].Value)},

    {"ILLUM_LED_WAVELENGTH",                 "INT",      LBL_OPTIONAL,
        LBL_NO_CONT,    1,  5,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, IllumLedWaveLength[4].Value),
        LBL_OFFSET(LblIlluminationParms_typ, IllumLedWaveLength[4].Valid),
        LBL_NO_RETURN,  LBL_SIZE(IllumLedWaveLength[4].Value)},

    {"ILLUM_LED_WAVELENGTH",                 "INT",      LBL_OPTIONAL,
        LBL_NO_CONT,    1,  6,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, IllumLedWaveLength[5].Value),
        LBL_OFFSET(LblIlluminationParms_typ, IllumLedWaveLength[5].Valid),
        LBL_NO_RETURN,  LBL_SIZE(IllumLedWaveLength[5].Value)},

    {"ILLUM_LED_WAVELENGTH",                 "INT",      LBL_OPTIONAL,
        LBL_NO_CONT,    1,  7,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, IllumLedWaveLength[6].Value),
        LBL_OFFSET(LblIlluminationParms_typ, IllumLedWaveLength[6].Valid),
        LBL_NO_RETURN,  LBL_SIZE(IllumLedWaveLength[6].Value)},

    {"ILLUM_LED_WAVELENGTH",                 "INT",      LBL_OPTIONAL,
        LBL_NO_CONT,    1,  8,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, IllumLedWaveLength[7].Value),
        LBL_OFFSET(LblIlluminationParms_typ, IllumLedWaveLength[7].Valid),
        LBL_NO_RETURN,  LBL_SIZE(IllumLedWaveLength[7].Value)},

    {"ILLUM_LED_WAVELENGTH",                 "INT",      LBL_OPTIONAL,
        LBL_NO_CONT,    1,  9,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, IllumLedWaveLength[8].Value),
        LBL_OFFSET(LblIlluminationParms_typ, IllumLedWaveLength[8].Valid),
        LBL_NO_RETURN,  LBL_SIZE(IllumLedWaveLength[8].Value)},

    {"ILLUM_LED_WAVELENGTH",                 "INT",      LBL_OPTIONAL,
        LBL_NO_CONT,    1,  10,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, IllumLedWaveLength[9].Value),
        LBL_OFFSET(LblIlluminationParms_typ, IllumLedWaveLength[9].Valid),
        LBL_NO_RETURN,  LBL_SIZE(IllumLedWaveLength[9].Value)},

    {"ILLUM_LED_WAVELENGTH",                 "INT",      LBL_OPTIONAL,
        LBL_NO_CONT,    1,  11,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, IllumLedWaveLength[10].Value),
        LBL_OFFSET(LblIlluminationParms_typ, IllumLedWaveLength[10].Valid),
        LBL_NO_RETURN,  LBL_SIZE(IllumLedWaveLength[10].Value)},

    {"ILLUM_LED_WAVELENGTH",                 "INT",      LBL_OPTIONAL,
        LBL_NO_CONT,    1,  12,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, IllumLedWaveLength[11].Value),
        LBL_OFFSET(LblIlluminationParms_typ, IllumLedWaveLength[11].Valid),
        LBL_NO_RETURN,  LBL_SIZE(IllumLedWaveLength[1].Value)},

    {"ILLUM_LED_WAVELENGTH",                 "INT",      LBL_OPTIONAL,
        LBL_NO_CONT,    1,  13,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, IllumLedWaveLength[12].Value),
        LBL_OFFSET(LblIlluminationParms_typ, IllumLedWaveLength[12].Valid),
        LBL_NO_RETURN,  LBL_SIZE(IllumLedWaveLength[12].Value)},

    {"ILLUM_LED_WAVELENGTH",                 "INT",      LBL_OPTIONAL,
        LBL_NO_CONT,    1,  14,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, IllumLedWaveLength[13].Value),
        LBL_OFFSET(LblIlluminationParms_typ, IllumLedWaveLength[13].Valid),
        LBL_NO_RETURN,  LBL_SIZE(IllumLedWaveLength[13].Value)},

    {"ILLUM_LED_WAVELENGTH__UNIT",            "STRING",      LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, IllumLedWaveLengthUnit[0].Value),
        LBL_OFFSET(LblIlluminationParms_typ, IllumLedWaveLengthUnit[0].Valid),
        LBL_NO_RETURN,  LBL_SIZE(IllumLedWaveLengthUnit[0].Value)},

    {"ILLUM_LED_WAVELENGTH__UNIT",            "STRING",      LBL_OPTIONAL,
        LBL_NO_CONT,    1,  2,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, IllumLedWaveLengthUnit[1].Value),
        LBL_OFFSET(LblIlluminationParms_typ, IllumLedWaveLengthUnit[1].Valid),
        LBL_NO_RETURN,  LBL_SIZE(IllumLedWaveLengthUnit[1].Value)},

    {"ILLUM_LED_WAVELENGTH__UNIT",            "STRING",      LBL_OPTIONAL,
        LBL_NO_CONT,    1,  3,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, IllumLedWaveLengthUnit[2].Value),
        LBL_OFFSET(LblIlluminationParms_typ, IllumLedWaveLengthUnit[2].Valid),
        LBL_NO_RETURN,  LBL_SIZE(IllumLedWaveLengthUnit[2].Value)},

    {"ILLUM_LED_WAVELENGTH__UNIT",            "STRING",      LBL_OPTIONAL,
        LBL_NO_CONT,    1,  4,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, IllumLedWaveLengthUnit[3].Value),
        LBL_OFFSET(LblIlluminationParms_typ, IllumLedWaveLengthUnit[3].Valid),
        LBL_NO_RETURN,  LBL_SIZE(IllumLedWaveLengthUnit[3].Value)},

    {"ILLUM_LED_WAVELENGTH__UNIT",            "STRING",      LBL_OPTIONAL,
        LBL_NO_CONT,    1,  5,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, IllumLedWaveLengthUnit[4].Value),
        LBL_OFFSET(LblIlluminationParms_typ, IllumLedWaveLengthUnit[4].Valid),
        LBL_NO_RETURN,  LBL_SIZE(IllumLedWaveLengthUnit[4].Value)},

    {"ILLUM_LED_WAVELENGTH__UNIT",            "STRING",      LBL_OPTIONAL,
        LBL_NO_CONT,    1,  6,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, IllumLedWaveLengthUnit[5].Value),
        LBL_OFFSET(LblIlluminationParms_typ, IllumLedWaveLengthUnit[5].Valid),
        LBL_NO_RETURN,  LBL_SIZE(IllumLedWaveLengthUnit[5].Value)},

    {"ILLUM_LED_WAVELENGTH__UNIT",            "STRING",      LBL_OPTIONAL,
        LBL_NO_CONT,    1,  7,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, IllumLedWaveLengthUnit[6].Value),
        LBL_OFFSET(LblIlluminationParms_typ, IllumLedWaveLengthUnit[6].Valid),
        LBL_NO_RETURN,  LBL_SIZE(IllumLedWaveLengthUnit[6].Value)},

    {"ILLUM_LED_WAVELENGTH__UNIT",            "STRING",      LBL_OPTIONAL,
        LBL_NO_CONT,    1,  8,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, IllumLedWaveLengthUnit[7].Value),
        LBL_OFFSET(LblIlluminationParms_typ, IllumLedWaveLengthUnit[7].Valid),
        LBL_NO_RETURN,  LBL_SIZE(IllumLedWaveLengthUnit[7].Value)},

    {"ILLUM_LED_WAVELENGTH__UNIT",            "STRING",      LBL_OPTIONAL,
        LBL_NO_CONT,    1,  9,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, IllumLedWaveLengthUnit[8].Value),
        LBL_OFFSET(LblIlluminationParms_typ, IllumLedWaveLengthUnit[8].Valid),
        LBL_NO_RETURN,  LBL_SIZE(IllumLedWaveLengthUnit[8].Value)},

    {"ILLUM_LED_WAVELENGTH__UNIT",            "STRING",      LBL_OPTIONAL,
        LBL_NO_CONT,    1,  10,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, IllumLedWaveLengthUnit[9].Value),
        LBL_OFFSET(LblIlluminationParms_typ, IllumLedWaveLengthUnit[9].Valid),
        LBL_NO_RETURN,  LBL_SIZE(IllumLedWaveLengthUnit[9].Value)},

    {"ILLUM_LED_WAVELENGTH__UNIT",            "STRING",      LBL_OPTIONAL,
        LBL_NO_CONT,    1,  11,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, IllumLedWaveLengthUnit[10].Value),
        LBL_OFFSET(LblIlluminationParms_typ, IllumLedWaveLengthUnit[10].Valid),
        LBL_NO_RETURN,  LBL_SIZE(IllumLedWaveLengthUnit[10].Value)},

    {"ILLUM_LED_WAVELENGTH__UNIT",            "STRING",      LBL_OPTIONAL,
        LBL_NO_CONT,    1,  12,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, IllumLedWaveLengthUnit[11].Value),
        LBL_OFFSET(LblIlluminationParms_typ, IllumLedWaveLengthUnit[11].Valid),
        LBL_NO_RETURN,  LBL_SIZE(IllumLedWaveLengthUnit[11].Value)},

    {"ILLUM_LED_WAVELENGTH__UNIT",            "STRING",      LBL_OPTIONAL,
        LBL_NO_CONT,    1,  13,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, IllumLedWaveLengthUnit[12].Value),
        LBL_OFFSET(LblIlluminationParms_typ, IllumLedWaveLengthUnit[12].Valid),
        LBL_NO_RETURN,  LBL_SIZE(IllumLedWaveLengthUnit[12].Value)},

    {"ILLUM_LED_WAVELENGTH__UNIT",            "STRING",      LBL_OPTIONAL,
        LBL_NO_CONT,    1,  14,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, IllumLedWaveLengthUnit[13].Value),
        LBL_OFFSET(LblIlluminationParms_typ, IllumLedWaveLengthUnit[13].Valid),
        LBL_NO_RETURN,  LBL_SIZE(IllumLedWaveLengthUnit[13].Value)},

    {"PIXL_ILLUM_CURRENT",                 "INT",      LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumCurrent[0].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumCurrent[0].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumCurrent[0].Value)},

    {"PIXL_ILLUM_CURRENT",                 "INT",      LBL_OPTIONAL,
        LBL_NO_CONT,    1,  2,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumCurrent[1].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumCurrent[1].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumCurrent[1].Value)},

    {"PIXL_ILLUM_CURRENT",                 "INT",      LBL_OPTIONAL,
        LBL_NO_CONT,    1,  3,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumCurrent[2].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumCurrent[2].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumCurrent[2].Value)},

    {"PIXL_ILLUM_CURRENT",                 "INT",      LBL_OPTIONAL,
        LBL_NO_CONT,    1,  4,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumCurrent[3].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumCurrent[3].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumCurrent[3].Value)},

    {"PIXL_ILLUM_CURRENT",                 "INT",      LBL_OPTIONAL,
        LBL_NO_CONT,    1,  5,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumCurrent[4].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumCurrent[4].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumCurrent[4].Value)},

    {"PIXL_ILLUM_CURRENT",                 "INT",      LBL_OPTIONAL,
        LBL_NO_CONT,    1,  6,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumCurrent[5].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumCurrent[5].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumCurrent[5].Value)},

    {"PIXL_ILLUM_CURRENT",                 "INT",      LBL_OPTIONAL,
        LBL_NO_CONT,    1,  7,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumCurrent[6].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumCurrent[6].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumCurrent[6].Value)},

    {"PIXL_ILLUM_CURRENT",                 "INT",      LBL_OPTIONAL,
        LBL_NO_CONT,    1,  8,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumCurrent[7].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumCurrent[7].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumCurrent[7].Value)},

    {"PIXL_ILLUM_CURRENT",                 "INT",      LBL_OPTIONAL,
        LBL_NO_CONT,    1,  9,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumCurrent[8].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumCurrent[8].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumCurrent[8].Value)},

    {"PIXL_ILLUM_CURRENT",                 "INT",      LBL_OPTIONAL,
        LBL_NO_CONT,    1,  10,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumCurrent[9].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumCurrent[9].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumCurrent[9].Value)},

    {"PIXL_ILLUM_CURRENT",                 "INT",      LBL_OPTIONAL,
        LBL_NO_CONT,    1,  11,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumCurrent[10].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumCurrent[10].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumCurrent[10].Value)},

    {"PIXL_ILLUM_CURRENT",                 "INT",      LBL_OPTIONAL,
        LBL_NO_CONT,    1,  12,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumCurrent[11].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumCurrent[11].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumCurrent[11].Value)},

    {"PIXL_ILLUM_CURRENT",                 "INT",      LBL_OPTIONAL,
        LBL_NO_CONT,    1,  13,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumCurrent[12].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumCurrent[12].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumCurrent[12].Value)},

    {"PIXL_ILLUM_CURRENT",                 "INT",      LBL_OPTIONAL,
        LBL_NO_CONT,    1,  14,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumCurrent[13].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumCurrent[13].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumCurrent[13].Value)},

    {"PIXL_ILLUM_CURRENT",                 "INT",      LBL_OPTIONAL,
        LBL_NO_CONT,    1,  15,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumCurrent[14].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumCurrent[14].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumCurrent[14].Value)},

    {"PIXL_ILLUM_CURRENT",                 "INT",      LBL_OPTIONAL,
        LBL_NO_CONT,    1,  16,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumCurrent[15].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumCurrent[15].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumCurrent[15].Value)},

    {"PIXL_ILLUM_CURRENT",                 "INT",      LBL_OPTIONAL,
        LBL_NO_CONT,    1,  17,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumCurrent[16].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumCurrent[16].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumCurrent[16].Value)},

    {"PIXL_ILLUM_CURRENT__UNIT",            "STRING",      LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumCurrentUnit[0].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumCurrentUnit[0].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumCurrentUnit[0].Value)},

    {"PIXL_ILLUM_CURRENT__UNIT",            "STRING",      LBL_OPTIONAL,
        LBL_NO_CONT,    1,  2,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumCurrentUnit[1].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumCurrentUnit[1].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumCurrentUnit[1].Value)},

    {"PIXL_ILLUM_CURRENT__UNIT",            "STRING",      LBL_OPTIONAL,
        LBL_NO_CONT,    1,  3,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumCurrentUnit[2].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumCurrentUnit[2].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumCurrentUnit[2].Value)},

    {"PIXL_ILLUM_CURRENT__UNIT",            "STRING",      LBL_OPTIONAL,
        LBL_NO_CONT,    1,  4,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumCurrentUnit[3].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumCurrentUnit[3].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumCurrentUnit[3].Value)},

    {"PIXL_ILLUM_CURRENT__UNIT",            "STRING",      LBL_OPTIONAL,
        LBL_NO_CONT,    1,  5,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumCurrentUnit[4].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumCurrentUnit[4].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumCurrentUnit[4].Value)},

    {"PIXL_ILLUM_CURRENT__UNIT",            "STRING",      LBL_OPTIONAL,
        LBL_NO_CONT,    1,  6,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumCurrentUnit[5].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumCurrentUnit[5].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumCurrentUnit[5].Value)},

    {"PIXL_ILLUM_CURRENT__UNIT",            "STRING",      LBL_OPTIONAL,
        LBL_NO_CONT,    1,  7,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumCurrentUnit[6].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumCurrentUnit[6].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumCurrentUnit[6].Value)},

    {"PIXL_ILLUM_CURRENT__UNIT",            "STRING",      LBL_OPTIONAL,
        LBL_NO_CONT,    1,  8,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumCurrentUnit[7].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumCurrentUnit[7].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumCurrentUnit[7].Value)},

    {"PIXL_ILLUM_CURRENT__UNIT",            "STRING",      LBL_OPTIONAL,
        LBL_NO_CONT,    1,  9,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumCurrentUnit[8].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumCurrentUnit[8].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumCurrentUnit[8].Value)},

    {"PIXL_ILLUM_CURRENT__UNIT",            "STRING",      LBL_OPTIONAL,
        LBL_NO_CONT,    1,  10,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumCurrentUnit[9].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumCurrentUnit[9].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumCurrentUnit[9].Value)},

    {"PIXL_ILLUM_CURRENT__UNIT",            "STRING",      LBL_OPTIONAL,
        LBL_NO_CONT,    1,  11,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumCurrentUnit[10].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumCurrentUnit[10].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumCurrentUnit[10].Value)},

    {"PIXL_ILLUM_CURRENT__UNIT",            "STRING",      LBL_OPTIONAL,
        LBL_NO_CONT,    1,  12,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumCurrentUnit[11].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumCurrentUnit[11].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumCurrentUnit[11].Value)},

    {"PIXL_ILLUM_CURRENT__UNIT",            "STRING",      LBL_OPTIONAL,
        LBL_NO_CONT,    1,  13,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumCurrentUnit[12].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumCurrentUnit[12].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumCurrentUnit[12].Value)},

    {"PIXL_ILLUM_CURRENT__UNIT",            "STRING",      LBL_OPTIONAL,
        LBL_NO_CONT,    1,  14,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumCurrentUnit[13].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumCurrentUnit[13].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumCurrentUnit[13].Value)},

    {"PIXL_ILLUM_CURRENT__UNIT",            "STRING",      LBL_OPTIONAL,
        LBL_NO_CONT,    1,  15,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumCurrentUnit[14].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumCurrentUnit[14].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumCurrentUnit[14].Value)},

    {"PIXL_ILLUM_CURRENT__UNIT",            "STRING",      LBL_OPTIONAL,
        LBL_NO_CONT,    1,  16,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumCurrentUnit[15].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumCurrentUnit[15].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumCurrentUnit[15].Value)},

    {"PIXL_ILLUM_CURRENT__UNIT",            "STRING",      LBL_OPTIONAL,
        LBL_NO_CONT,    1,  17,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumCurrentUnit[16].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumCurrentUnit[16].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumCurrentUnit[16].Value)},

    {"PIXL_ILLUM_CURRENT_NAME",            "STRING",      LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumCurrentName[0].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumCurrentName[0].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumCurrentName[0].Value)},

    {"PIXL_ILLUM_CURRENT_NAME",            "STRING",      LBL_OPTIONAL,
        LBL_NO_CONT,    1,  2,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumCurrentName[1].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumCurrentName[1].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumCurrentName[1].Value)},

    {"PIXL_ILLUM_CURRENT_NAME",            "STRING",      LBL_OPTIONAL,
        LBL_NO_CONT,    1,  3,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumCurrentName[2].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumCurrentName[2].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumCurrentName[2].Value)},

    {"PIXL_ILLUM_CURRENT_NAME",            "STRING",      LBL_OPTIONAL,
        LBL_NO_CONT,    1,  4,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumCurrentName[3].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumCurrentName[3].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumCurrentName[3].Value)},

    {"PIXL_ILLUM_CURRENT_NAME",            "STRING",      LBL_OPTIONAL,
        LBL_NO_CONT,    1,  5,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumCurrentName[4].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumCurrentName[4].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumCurrentName[4].Value)},

    {"PIXL_ILLUM_CURRENT_NAME",            "STRING",      LBL_OPTIONAL,
        LBL_NO_CONT,    1,  6,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumCurrentName[5].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumCurrentName[5].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumCurrentName[5].Value)},

    {"PIXL_ILLUM_CURRENT_NAME",            "STRING",      LBL_OPTIONAL,
        LBL_NO_CONT,    1,  7,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumCurrentName[6].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumCurrentName[6].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumCurrentName[6].Value)},

    {"PIXL_ILLUM_CURRENT_NAME",            "STRING",      LBL_OPTIONAL,
        LBL_NO_CONT,    1,  8,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumCurrentName[7].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumCurrentName[7].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumCurrentName[7].Value)},

    {"PIXL_ILLUM_CURRENT_NAME",            "STRING",      LBL_OPTIONAL,
        LBL_NO_CONT,    1,  9,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumCurrentName[8].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumCurrentName[8].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumCurrentName[8].Value)},

    {"PIXL_ILLUM_CURRENT_NAME",            "STRING",      LBL_OPTIONAL,
        LBL_NO_CONT,    1,  10,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumCurrentName[9].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumCurrentName[9].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumCurrentName[9].Value)},

    {"PIXL_ILLUM_CURRENT_NAME",            "STRING",      LBL_OPTIONAL,
        LBL_NO_CONT,    1,  11,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumCurrentName[10].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumCurrentName[10].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumCurrentName[10].Value)},

    {"PIXL_ILLUM_CURRENT_NAME",            "STRING",      LBL_OPTIONAL,
        LBL_NO_CONT,    1,  12,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumCurrentName[11].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumCurrentName[11].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumCurrentName[11].Value)},

    {"PIXL_ILLUM_CURRENT_NAME",            "STRING",      LBL_OPTIONAL,
        LBL_NO_CONT,    1,  13,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumCurrentName[12].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumCurrentName[12].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumCurrentName[12].Value)},

    {"PIXL_ILLUM_CURRENT_NAME",            "STRING",      LBL_OPTIONAL,
        LBL_NO_CONT,    1,  14,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumCurrentName[13].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumCurrentName[13].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumCurrentName[13].Value)},

    {"PIXL_ILLUM_CURRENT_NAME",            "STRING",      LBL_OPTIONAL,
        LBL_NO_CONT,    1,  15,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumCurrentName[14].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumCurrentName[14].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumCurrentName[14].Value)},

    {"PIXL_ILLUM_CURRENT_NAME",            "STRING",      LBL_OPTIONAL,
        LBL_NO_CONT,    1,  16,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumCurrentName[15].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumCurrentName[15].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumCurrentName[15].Value)},

    {"PIXL_ILLUM_CURRENT_NAME",            "STRING",      LBL_OPTIONAL,
        LBL_NO_CONT,    1,  17,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumCurrentName[16].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumCurrentName[16].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumCurrentName[16].Value)},

    {"PIXL_ILLUM_FLASH_DUR",            "REAL",     LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumFlashDur[0].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumFlashDur[0].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumFlashDur[0].Value)},

    {"PIXL_ILLUM_FLASH_DUR",            "REAL",     LBL_OPTIONAL,
        LBL_NO_CONT,    1,  2,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumFlashDur[1].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumFlashDur[1].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumFlashDur[1].Value)},

    {"PIXL_ILLUM_FLASH_DUR",            "REAL",     LBL_OPTIONAL,
        LBL_NO_CONT,    1,  3,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumFlashDur[2].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumFlashDur[2].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumFlashDur[2].Value)},

    {"PIXL_ILLUM_FLASH_DUR",            "REAL",     LBL_OPTIONAL,
        LBL_NO_CONT,    1,  4,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumFlashDur[3].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumFlashDur[3].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumFlashDur[3].Value)},

    {"PIXL_ILLUM_FLASH_DUR",            "REAL",     LBL_OPTIONAL,
        LBL_NO_CONT,    1,  5,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumFlashDur[4].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumFlashDur[4].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumFlashDur[4].Value)},

    {"PIXL_ILLUM_FLASH_DUR",            "REAL",     LBL_OPTIONAL,
        LBL_NO_CONT,    1,  6,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumFlashDur[5].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumFlashDur[5].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumFlashDur[5].Value)},

    {"PIXL_ILLUM_FLASH_DUR",            "REAL",     LBL_OPTIONAL,
        LBL_NO_CONT,    1,  7,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumFlashDur[6].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumFlashDur[6].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumFlashDur[6].Value)},

    {"PIXL_ILLUM_FLASH_DUR",            "REAL",     LBL_OPTIONAL,
        LBL_NO_CONT,    1,  8,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumFlashDur[7].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumFlashDur[7].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumFlashDur[7].Value)},

    {"PIXL_ILLUM_FLASH_DUR",            "REAL",     LBL_OPTIONAL,
        LBL_NO_CONT,    1,  9,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumFlashDur[8].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumFlashDur[8].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumFlashDur[8].Value)},

    {"PIXL_ILLUM_FLASH_DUR__UNIT",       "STRING",     LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumFlashDurUnit[0].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumFlashDurUnit[0].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumFlashDurUnit[0].Value)},

    {"PIXL_ILLUM_FLASH_DUR__UNIT",       "STRING",     LBL_OPTIONAL,
        LBL_NO_CONT,    1,  2,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumFlashDurUnit[1].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumFlashDurUnit[1].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumFlashDurUnit[1].Value)},

    {"PIXL_ILLUM_FLASH_DUR__UNIT",       "STRING",     LBL_OPTIONAL,
        LBL_NO_CONT,    1,  3,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumFlashDurUnit[2].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumFlashDurUnit[2].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumFlashDurUnit[2].Value)},

    {"PIXL_ILLUM_FLASH_DUR__UNIT",       "STRING",     LBL_OPTIONAL,
        LBL_NO_CONT,    1,  4,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumFlashDurUnit[3].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumFlashDurUnit[3].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumFlashDurUnit[3].Value)},

    {"PIXL_ILLUM_FLASH_DUR__UNIT",       "STRING",     LBL_OPTIONAL,
        LBL_NO_CONT,    1,  5,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumFlashDurUnit[4].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumFlashDurUnit[4].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumFlashDurUnit[4].Value)},

    {"PIXL_ILLUM_FLASH_DUR__UNIT",       "STRING",     LBL_OPTIONAL,
        LBL_NO_CONT,    1,  6,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumFlashDurUnit[5].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumFlashDurUnit[5].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumFlashDurUnit[5].Value)},

    {"PIXL_ILLUM_FLASH_DUR__UNIT",       "STRING",     LBL_OPTIONAL,
        LBL_NO_CONT,    1,  7,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumFlashDurUnit[6].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumFlashDurUnit[6].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumFlashDurUnit[6].Value)},

    {"PIXL_ILLUM_FLASH_DUR__UNIT",       "STRING",     LBL_OPTIONAL,
        LBL_NO_CONT,    1,  8,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumFlashDurUnit[7].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumFlashDurUnit[7].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumFlashDurUnit[7].Value)},

    {"PIXL_ILLUM_FLASH_DUR__UNIT",       "STRING",     LBL_OPTIONAL,
        LBL_NO_CONT,    1,  9,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumFlashDurUnit[8].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumFlashDurUnit[8].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumFlashDurUnit[8].Value)},

    {"PIXL_ILLUM_FLASH_DUR_NAME",       "STRING",     LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumFlashDurName[0].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumFlashDurName[0].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumFlashDurName[0].Value)},

    {"PIXL_ILLUM_FLASH_DUR_NAME",       "STRING",     LBL_OPTIONAL,
        LBL_NO_CONT,    1,  2,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumFlashDurName[1].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumFlashDurName[1].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumFlashDurName[1].Value)},

    {"PIXL_ILLUM_FLASH_DUR_NAME",       "STRING",     LBL_OPTIONAL,
        LBL_NO_CONT,    1,  3,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumFlashDurName[2].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumFlashDurName[2].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumFlashDurName[2].Value)},

    {"PIXL_ILLUM_FLASH_DUR_NAME",       "STRING",     LBL_OPTIONAL,
        LBL_NO_CONT,    1,  4,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumFlashDurName[3].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumFlashDurName[3].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumFlashDurName[3].Value)},

    {"PIXL_ILLUM_FLASH_DUR_NAME",       "STRING",     LBL_OPTIONAL,
        LBL_NO_CONT,    1,  5,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumFlashDurName[4].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumFlashDurName[4].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumFlashDurName[4].Value)},

    {"PIXL_ILLUM_FLASH_DUR_NAME",       "STRING",     LBL_OPTIONAL,
        LBL_NO_CONT,    1,  6,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumFlashDurName[5].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumFlashDurName[5].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumFlashDurName[5].Value)},

    {"PIXL_ILLUM_FLASH_DUR_NAME",       "STRING",     LBL_OPTIONAL,
        LBL_NO_CONT,    1,  7,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumFlashDurName[6].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumFlashDurName[6].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumFlashDurName[6].Value)},

    {"PIXL_ILLUM_FLASH_DUR_NAME",       "STRING",     LBL_OPTIONAL,
        LBL_NO_CONT,    1,  8,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumFlashDurName[7].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumFlashDurName[7].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumFlashDurName[7].Value)},

    {"PIXL_ILLUM_FLASH_DUR_NAME",       "STRING",     LBL_OPTIONAL,
        LBL_NO_CONT,    1,  9,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumFlashDurName[8].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumFlashDurName[8].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumFlashDurName[8].Value)},

	{"PIXL_ILLUM_ADC_OFFSET",            "INT",      LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumAdcOffset[0].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumAdcOffset[0].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumAdcOffset[0].Value)},

    {"PIXL_ILLUM_ADC_OFFSET",            "INT",      LBL_OPTIONAL,
        LBL_NO_CONT,    1,  2,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumAdcOffset[1].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumAdcOffset[1].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumAdcOffset[1].Value)},

    {"PIXL_ILLUM_ADC_OFFSET_NAME",       "STRING",      LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumAdcOffsetName[0].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumAdcOffsetName[0].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumAdcOffsetName[0].Value)},

    {"PIXL_ILLUM_ADC_OFFSET_NAME",       "STRING",      LBL_OPTIONAL,
        LBL_NO_CONT,    1,  2,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumAdcOffsetName[1].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumAdcOffsetName[1].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumAdcOffsetName[1].Value)},

    {"PIXL_ILLUM_TEMPERATURE",               "REAL",     LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumTemperature[0].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumTemperature[0].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumTemperature[0].Value)},

    {"PIXL_ILLUM_TEMPERATURE",               "REAL",     LBL_OPTIONAL,
        LBL_NO_CONT,    1,  2,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumTemperature[1].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumTemperature[1].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumTemperature[1].Value)},

    {"PIXL_ILLUM_TEMPERATURE__UNIT",          "STRING",     LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumTemperatureUnit[0].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumTemperatureUnit[0].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumTemperatureUnit[0].Value)},

    {"PIXL_ILLUM_TEMPERATURE__UNIT",          "STRING",     LBL_OPTIONAL,
        LBL_NO_CONT,    1,  2,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumTemperatureUnit[1].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumTemperatureUnit[1].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumTemperatureUnit[1].Value)},

    {"PIXL_ILLUM_TEMPERATURE_NAME",           "STRING",     LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumTemperatureName[0].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumTemperatureName[0].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumTemperatureName[0].Value)},

    {"PIXL_ILLUM_TEMPERATURE_NAME",           "STRING",     LBL_OPTIONAL,
        LBL_NO_CONT,    1,  2,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumTemperatureName[1].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumTemperatureName[1].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumTemperatureName[1].Value)},

    {"PIXL_ILLUM_VOLTAGE",                   "REAL",     LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumVoltage[0].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumVoltage[0].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumVoltage[0].Value)},

    {"PIXL_ILLUM_VOLTAGE",                   "REAL",     LBL_OPTIONAL,
        LBL_NO_CONT,    1,  2,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumVoltage[1].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumVoltage[1].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumVoltage[1].Value)},

    {"PIXL_ILLUM_VOLTAGE",                   "REAL",     LBL_OPTIONAL,
        LBL_NO_CONT,    1,  3,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumVoltage[2].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumVoltage[2].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumVoltage[2].Value)},

    {"PIXL_ILLUM_VOLTAGE",                   "REAL",     LBL_OPTIONAL,
        LBL_NO_CONT,    1,  4,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumVoltage[3].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumVoltage[3].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumVoltage[3].Value)},

    {"PIXL_ILLUM_VOLTAGE",                   "REAL",     LBL_OPTIONAL,
        LBL_NO_CONT,    1,  5,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumVoltage[4].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumVoltage[4].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumVoltage[4].Value)},

    {"PIXL_ILLUM_VOLTAGE",                   "REAL",     LBL_OPTIONAL,
        LBL_NO_CONT,    1,  6,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumVoltage[5].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumVoltage[5].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumVoltage[5].Value)},

    {"PIXL_ILLUM_VOLTAGE",                   "REAL",     LBL_OPTIONAL,
        LBL_NO_CONT,    1,  7,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumVoltage[6].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumVoltage[6].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumVoltage[6].Value)},

    {"PIXL_ILLUM_VOLTAGE",                   "REAL",     LBL_OPTIONAL,
        LBL_NO_CONT,    1,  8,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumVoltage[7].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumVoltage[7].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumVoltage[7].Value)},

    {"PIXL_ILLUM_VOLTAGE",                   "REAL",     LBL_OPTIONAL,
        LBL_NO_CONT,    1,  9,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumVoltage[8].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumVoltage[8].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumVoltage[8].Value)},

    {"PIXL_ILLUM_VOLTAGE",                   "REAL",     LBL_OPTIONAL,
        LBL_NO_CONT,    1,  10,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumVoltage[9].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumVoltage[9].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumVoltage[9].Value)},

    {"PIXL_ILLUM_VOLTAGE",                   "REAL",     LBL_OPTIONAL,
        LBL_NO_CONT,    1,  11,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumVoltage[10].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumVoltage[10].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumVoltage[10].Value)},

    {"PIXL_ILLUM_VOLTAGE",                   "REAL",     LBL_OPTIONAL,
        LBL_NO_CONT,    1,  12,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumVoltage[11].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumVoltage[11].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumVoltage[11].Value)},

    {"PIXL_ILLUM_VOLTAGE",                   "REAL",     LBL_OPTIONAL,
        LBL_NO_CONT,    1,  13,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumVoltage[12].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumVoltage[12].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumVoltage[12].Value)},

    {"PIXL_ILLUM_VOLTAGE",                   "REAL",     LBL_OPTIONAL,
        LBL_NO_CONT,    1,  14,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumVoltage[13].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumVoltage[13].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumVoltage[13].Value)},

    {"PIXL_ILLUM_VOLTAGE",                   "REAL",     LBL_OPTIONAL,
        LBL_NO_CONT,    1,  15,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumVoltage[14].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumVoltage[14].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumVoltage[14].Value)},

    {"PIXL_ILLUM_VOLTAGE",                   "REAL",     LBL_OPTIONAL,
        LBL_NO_CONT,    1,  16,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumVoltage[15].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumVoltage[15].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumVoltage[15].Value)},

    {"PIXL_ILLUM_VOLTAGE__UNIT",              "STRING",     LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumVoltageUnit[0].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumVoltageUnit[0].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumVoltageUnit[0].Value)},

    {"PIXL_ILLUM_VOLTAGE__UNIT",              "STRING",     LBL_OPTIONAL,
        LBL_NO_CONT,    1,  2,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumVoltageUnit[1].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumVoltageUnit[1].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumVoltageUnit[1].Value)},

    {"PIXL_ILLUM_VOLTAGE__UNIT",              "STRING",     LBL_OPTIONAL,
        LBL_NO_CONT,    1,  3,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumVoltageUnit[2].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumVoltageUnit[2].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumVoltageUnit[2].Value)},

    {"PIXL_ILLUM_VOLTAGE__UNIT",              "STRING",     LBL_OPTIONAL,
        LBL_NO_CONT,    1,  4,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumVoltageUnit[3].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumVoltageUnit[3].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumVoltageUnit[3].Value)},

    {"PIXL_ILLUM_VOLTAGE__UNIT",              "STRING",     LBL_OPTIONAL,
        LBL_NO_CONT,    1,  5,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumVoltageUnit[4].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumVoltageUnit[4].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumVoltageUnit[4].Value)},

    {"PIXL_ILLUM_VOLTAGE__UNIT",              "STRING",     LBL_OPTIONAL,
        LBL_NO_CONT,    1,  6,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumVoltageUnit[5].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumVoltageUnit[5].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumVoltageUnit[5].Value)},

    {"PIXL_ILLUM_VOLTAGE__UNIT",              "STRING",     LBL_OPTIONAL,
        LBL_NO_CONT,    1,  7,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumVoltageUnit[6].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumVoltageUnit[6].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumVoltageUnit[6].Value)},

    {"PIXL_ILLUM_VOLTAGE__UNIT",              "STRING",     LBL_OPTIONAL,
        LBL_NO_CONT,    1,  8,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumVoltageUnit[7].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumVoltageUnit[7].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumVoltageUnit[7].Value)},

    {"PIXL_ILLUM_VOLTAGE__UNIT",              "STRING",     LBL_OPTIONAL,
        LBL_NO_CONT,    1,  9,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumVoltageUnit[8].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumVoltageUnit[8].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumVoltageUnit[8].Value)},

    {"PIXL_ILLUM_VOLTAGE__UNIT",              "STRING",     LBL_OPTIONAL,
        LBL_NO_CONT,    1,  10,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumVoltageUnit[9].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumVoltageUnit[9].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumVoltageUnit[9].Value)},

    {"PIXL_ILLUM_VOLTAGE__UNIT",              "STRING",     LBL_OPTIONAL,
        LBL_NO_CONT,    1,  11,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumVoltageUnit[10].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumVoltageUnit[10].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumVoltageUnit[10].Value)},

    {"PIXL_ILLUM_VOLTAGE__UNIT",              "STRING",     LBL_OPTIONAL,
        LBL_NO_CONT,    1,  12,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumVoltageUnit[11].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumVoltageUnit[11].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumVoltageUnit[11].Value)},

    {"PIXL_ILLUM_VOLTAGE__UNIT",              "STRING",     LBL_OPTIONAL,
        LBL_NO_CONT,    1,  13,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumVoltageUnit[12].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumVoltageUnit[12].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumVoltageUnit[12].Value)},

    {"PIXL_ILLUM_VOLTAGE__UNIT",              "STRING",     LBL_OPTIONAL,
        LBL_NO_CONT,    1,  14,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumVoltageUnit[13].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumVoltageUnit[13].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumVoltageUnit[13].Value)},

    {"PIXL_ILLUM_VOLTAGE__UNIT",              "STRING",     LBL_OPTIONAL,
        LBL_NO_CONT,    1,  15,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumVoltageUnit[14].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumVoltageUnit[14].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumVoltageUnit[14].Value)},

    {"PIXL_ILLUM_VOLTAGE__UNIT",              "STRING",     LBL_OPTIONAL,
        LBL_NO_CONT,    1,  16,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumVoltageUnit[15].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumVoltageUnit[15].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumVoltageUnit[15].Value)},

    {"PIXL_ILLUM_VOLTAGE_NAME",               "STRING",     LBL_OPTIONAL,
        LBL_NO_CONT,    1,  1,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumVoltageName[0].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumVoltageName[0].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumVoltageName[0].Value)},

    {"PIXL_ILLUM_VOLTAGE_NAME",               "STRING",     LBL_OPTIONAL,
        LBL_NO_CONT,    1,  2,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumVoltageName[1].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumVoltageName[1].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumVoltageName[1].Value)},

    {"PIXL_ILLUM_VOLTAGE_NAME",               "STRING",     LBL_OPTIONAL,
        LBL_NO_CONT,    1,  3,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumVoltageName[2].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumVoltageName[2].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumVoltageName[2].Value)},

    {"PIXL_ILLUM_VOLTAGE_NAME",               "STRING",     LBL_OPTIONAL,
        LBL_NO_CONT,    1,  4,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumVoltageName[3].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumVoltageName[3].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumVoltageName[3].Value)},

    {"PIXL_ILLUM_VOLTAGE_NAME",               "STRING",     LBL_OPTIONAL,
        LBL_NO_CONT,    1,  5,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumVoltageName[4].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumVoltageName[4].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumVoltageName[4].Value)},

    {"PIXL_ILLUM_VOLTAGE_NAME",               "STRING",     LBL_OPTIONAL,
        LBL_NO_CONT,    1,  6,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumVoltageName[5].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumVoltageName[5].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumVoltageName[5].Value)},

    {"PIXL_ILLUM_VOLTAGE_NAME",               "STRING",     LBL_OPTIONAL,
        LBL_NO_CONT,    1,  7,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumVoltageName[6].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumVoltageName[6].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumVoltageName[6].Value)},

    {"PIXL_ILLUM_VOLTAGE_NAME",               "STRING",     LBL_OPTIONAL,
        LBL_NO_CONT,    1,  8,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumVoltageName[7].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumVoltageName[7].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumVoltageName[7].Value)},

    {"PIXL_ILLUM_VOLTAGE_NAME",               "STRING",     LBL_OPTIONAL,
        LBL_NO_CONT,    1,  9,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumVoltageName[8].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumVoltageName[8].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumVoltageName[8].Value)},

    {"PIXL_ILLUM_VOLTAGE_NAME",               "STRING",     LBL_OPTIONAL,
        LBL_NO_CONT,    1,  10,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumVoltageName[9].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumVoltageName[9].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumVoltageName[9].Value)},

    {"PIXL_ILLUM_VOLTAGE_NAME",               "STRING",     LBL_OPTIONAL,
        LBL_NO_CONT,    1,  11,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumVoltageName[10].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumVoltageName[10].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumVoltageName[10].Value)},

    {"PIXL_ILLUM_VOLTAGE_NAME",               "STRING",     LBL_OPTIONAL,
        LBL_NO_CONT,    1,  12,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumVoltageName[11].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumVoltageName[11].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumVoltageName[11].Value)},

    {"PIXL_ILLUM_VOLTAGE_NAME",               "STRING",     LBL_OPTIONAL,
        LBL_NO_CONT,    1,  13,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumVoltageName[12].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumVoltageName[12].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumVoltageName[12].Value)},

    {"PIXL_ILLUM_VOLTAGE_NAME",               "STRING",     LBL_OPTIONAL,
        LBL_NO_CONT,    1,  14,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumVoltageName[13].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumVoltageName[13].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumVoltageName[13].Value)},

    {"PIXL_ILLUM_VOLTAGE_NAME",               "STRING",     LBL_OPTIONAL,
        LBL_NO_CONT,    1,  15,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumVoltageName[14].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumVoltageName[14].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumVoltageName[14].Value)},

    {"PIXL_ILLUM_VOLTAGE_NAME",               "STRING",     LBL_OPTIONAL,
        LBL_NO_CONT,    1,  16,  LBL_NULL,
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumVoltageName[15].Value),
        LBL_OFFSET(LblIlluminationParms_typ, PixlIllumVoltageName[15].Valid),
        LBL_NO_RETURN,  LBL_SIZE(PixlIllumVoltageName[15].Value)},

	{0, 0, 0, 0, 0, 0, 0, 0, 0, 0}};

static LblApiProcess_typ	Label = {
	LabelTbl,       "PROPERTY",     "PROPERTY",     "ILLUMINATION_PARMS",
	LBL_NULL };


/******************************************************************************
 *              LBL_SET_ILLUMINATION
 *
 *****************************************************************************/
void     LblSetIllumination(
  const char    *Name )
{
  Label.NameValue = Name;
  return;
}
/******************************************************************************
 *				LBL_ILLUMINATION_PARMS
 *
 *****************************************************************************/
int     LblIlluminationParms(
  int   Unit,
  int   Obtain,
  LblIlluminationParms_typ      *LabelItems,
  int	Instance)
{ int   RtnStatus;
  LblApiCntrl_typ	Cntrl;

  Label.Buffer = (void *)LabelItems;
  Label.BufferSize = sizeof(LblIlluminationParms_typ);

  memset(&Cntrl,0,sizeof(LblApiCntrl_typ));
  Cntrl.Instance = Instance;
  Cntrl.FileUnit = Unit;
  Cntrl.Obtain = Obtain;
  Cntrl.ProceedOnError = LBL_TRUE;

  RtnStatus = LblProcessor(&Cntrl, &Label);

  return (RtnStatus);
}
/******************************************************************************
 *              LBL_ILLUMINATION_PARMS
 *
 *****************************************************************************/
int     LblIlluminationApi(
  int   Unit,
  int   Obtain,
  LblIlluminationParms_typ      *LabelItems,
  int   Instance,
  const char* propertyName)
{ int   RtnStatus;
  LblApiCntrl_typ   Cntrl;
  LblSetIllumination(propertyName);
  Label.Buffer = (void *)LabelItems;
  Label.BufferSize = sizeof(LblIlluminationParms_typ);

  memset(&Cntrl,0,sizeof(LblApiCntrl_typ));
  Cntrl.Instance = Instance;
  Cntrl.FileUnit = Unit;
  Cntrl.Obtain = Obtain;
  Cntrl.ProceedOnError = LBL_TRUE;

  RtnStatus = LblProcessor(&Cntrl, &Label);

  return (RtnStatus);
}

/******************************************************************************
 *				LBL_PRINT_ILLUMINATION_PARMS
 *
 *****************************************************************************/
void	LblPrintIlluminationParms(
  LblIlluminationParms_typ	*LabelItems)
{
  Label.Buffer = (void *)LabelItems;

  PrintLabelElements( &Label );

  return;
}

/******************************************************************************
 *				LBL_TEST_PIXL_MCC_IMG_PARMS
 *
 *****************************************************************************/
void	LblTestIlluminationParms(
  LblIlluminationParms_typ	*LabelItems)
{
  Label.Buffer = (void *)LabelItems;

  TestLoadLabelElements( &Label );

  return;
}
