////////////////////////////////////////////////////////////////////////
// PigFileModelFIDO
//
// FIDO-specific File model.  This only needs to handle exceptions to the
// multimission label API.
////////////////////////////////////////////////////////////////////////

#include "PigFileModelFIDO.h"
#include "PigCSReference.h"

#include "zvproto.h"
#include "applic.h"

#include "return_status.h"

////////////////////////////////////////////////////////////////////////
// Constructor
////////////////////////////////////////////////////////////////////////

PigFileModelFIDO::PigFileModelFIDO(const char *filename, int unit,
							const char *mission)
		: PigFileModel(filename, unit, mission)
{
    _fidoLblWitsInfo = NULL;
}

PigFileModelFIDO::~PigFileModelFIDO()
{
    if (_fidoLblWitsInfo)
	delete _fidoLblWitsInfo;
}

////////////////////////////////////////////////////////////////////////
// These functions return complete Label API property structures.
////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////
// FidoWitsInfo
////////////////////////////////////////////////////////////////////////
const FidoLblWitsInfo_typ *PigFileModelFIDO::getFidoLblWitsInfo()
{
    if (_fidoLblWitsInfo != NULL)
	return _fidoLblWitsInfo;

    openFile();

    _fidoLblWitsInfo = new FidoLblWitsInfo_typ;
    if (_fidoLblWitsInfo == NULL) {
	printMsg("Memory error", "FidoWitsInfo", PigMsgError);
	return NULL;
    }

    int status=FidoLblWitsInfo(_unit, LBL_READ, _fidoLblWitsInfo,1);
    if (RTN_FAILURE(status)) {
	printMsg((char *)LblErrorMessage(), "FidoWitsInfo", PigMsgError);
	return NULL;
    }

    return _fidoLblWitsInfo;
}

////////////////////////////////////////////////////////////////////////
// These functions return basic information from the label.
////////////////////////////////////////////////////////////////////////

// Scalar numeric item
#define FILE_LABEL(Property,fName,Name,Type)					\
	Type PigFileModelFIDO::get##fName(Type def)			\
	{								\
	    if (_fidoLbl##Property == NULL)				\
		getFidoLbl##Property();					\
	    if (_fidoLbl##Property == NULL)				\
		return def;						\
	    if (!_fidoLbl##Property->Name.Valid)	       		\
		return def;						\
	    return _fidoLbl##Property->Name.Value;			\
	}

// Array item, converted to type "Type".  Type must have ctor for Value's type.
#define FILE_LABEL3(Property,fName,Name,Type)				\
	Type PigFileModelFIDO::get##fName(Type def)			\
	{								\
	    if (_fidoLbl##Property == NULL)				\
		getFidoLbl##Property();					\
	    if (_fidoLbl##Property == NULL)				\
		return def;						\
	    if (!_fidoLbl##Property->Name.Valid)	       		\
		return def;						\
	    return Type(_fidoLbl##Property->Name.Value);       		\
	}

FILE_LABEL   (WitsInfo, InstrumentMastJoint1Angle,
				MastJoint1Angle, float)
FILE_LABEL   (WitsInfo, InstrumentMastJoint2Angle,
				MastJoint2Angle, float)
FILE_LABEL   (WitsInfo, InstrumentMastJoint3Angle,
				MastJoint3Angle, float)
FILE_LABEL   (WitsInfo, InstrumentMastJoint4Angle,
				MastJoint4Angle, float)

// In only a few cases is the lack of value significant...
#define CHECK_FILE_LABEL(Property,fName,Name)				\
	int PigFileModelFIDO::check##fName()				\
	{								\
	    if (_fidoLbl##Property == NULL)				\
		getFidoLbl##Property();					\
	    if (_fidoLbl##Property == NULL)				\
		return FALSE;						\
	    return _fidoLbl##Property->Name.Valid;			\
	}

CHECK_FILE_LABEL(WitsInfo, InstrumentMastJoint1Angle,
				MastJoint1Angle)
CHECK_FILE_LABEL(WitsInfo, InstrumentMastJoint2Angle,
				MastJoint2Angle)
CHECK_FILE_LABEL(WitsInfo, InstrumentMastJoint3Angle,
				MastJoint3Angle)
CHECK_FILE_LABEL(WitsInfo, InstrumentMastJoint4Angle,
				MastJoint4Angle)

//////////////////////////////////////////////////////////////////////
// Caller is responsible for memory allocation of uid char array
// uid array should be big enough to hold at least 33 char
// 
//////////////////////////////////////////////////////////////////////
void PigFileModelFIDO::getUniqueId(char *uid)
{
    strcpy(uid, getProductId());
}

////////////////////////////////////////////////////////////////////////
// Get the Rover Motion Counter for the file.  The array must be supplied.
// num_index on input should contain the max size of the supplied array;
// on output it will contain the number of elements actually found (which
// could be bigger than the supplied max size, but only that many will
// actually be returned in the array).  PIG_MAX_SITE_INDEX (from
// PigRoverStateManager.h) can be useful for array dimensioning.
////////////////////////////////////////////////////////////////////////

void PigFileModelFIDO::getRoverMotionCounter(int indices[], int &num_indices)
{
    int max_indices = num_indices;
    num_indices = 0;

    //!!!!TEMPORARY Use RTL directly until these are added to AJR's routines!!!!

    openFile();				// just in case...

    int val, status;
    status = zlget(_unit, "property", "site_id", &val,
		"property", "identification", NULL);
    if (status != SUCCESS) {
	printWarning("SITE_ID not found in label!  0 assumed");
	val = 0;
    }
    if (num_indices < max_indices)
	indices[num_indices] = val;
    num_indices++;

    status = zlget(_unit, "property", "position_id", &val,
		"property", "identification", NULL);
    if (status != SUCCESS) {
	printWarning("POSITION_ID not found in label!  0 assumed");
	val = 0;
    }
    if (num_indices < max_indices)
	indices[num_indices] = val;
    num_indices++;

    return;
}

