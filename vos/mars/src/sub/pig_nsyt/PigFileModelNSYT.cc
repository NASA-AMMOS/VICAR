////////////////////////////////////////////////////////////////////////
// PigFileModelNSYT
//
// NSYT-specific File model.  This only needs to handle exceptions to the
// multimission label API.
////////////////////////////////////////////////////////////////////////

#include "PigFileModelNSYT.h"
#include "PigCSReference.h"

#include "zvproto.h"
#include "applic.h"

#include "return_status.h"

////////////////////////////////////////////////////////////////////////
// Constructor
////////////////////////////////////////////////////////////////////////

PigFileModelNSYT::PigFileModelNSYT(const char *filename, 
				 int unit,
				 const char *mission)
		        : PigFileModel(filename, unit, mission)
{
    // overwrites parent's offsets.  For NSYT offsets are always 0
    setupOffsets();
}

PigFileModelNSYT::~PigFileModelNSYT()
{
}

////////////////////////////////////////////////////////////////////////
// Initializes the x/y offsets, which are the offsets between the camera
// model L/S coordinates, and the "physical" coordinates in the image.
// Note that these physical coordinates are 0-based, e.g. you have to
// add 1 for VICAR files.
//
// For NSYT x and y offsets are always 0
////////////////////////////////////////////////////////////////////////

void PigFileModelNSYT::setupOffsets()
{
    _x_offset = 0;
    _y_offset = 0;
}

////////////////////////////////////////////////////////////////////////
// These functions return complete Label API property structures.
// OVERRIDES of base class to change the CS name for NSYT
////////////////////////////////////////////////////////////////////////

PIG_READ_LABEL_STRUCTURE_STR(LblCoordinate_typ, PigFileModelNSYT,
	RoverCoordSys, "RoverCoordSys", Coordinate, "LANDER_COODRINATE_SYSTEM")

PIG_READ_LABEL_STRUCTURE(LblInstrumentState_typ, PigFileModelNSYT,
        InstrumentState, "InstrumentState", InstrumentState, 
        "INSTRUMENT_STATE_PARMS")

////////////////////////////////////////////////////////////////////////
// These functions return basic information from the label.
////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////
// Determine instrument temperature using sensors data
//////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////
//!!!! FOR NOW JUST RETURN THE FIRST TEMP!!!!  NOT UPDATED FOR INSIGHT !!!!

float PigFileModelNSYT::getInstrumentTemperature(float def)
{						// InstrumentState
    if (_lblInstrumentState == NULL)
        getLblInstrumentState();
    if (_lblInstrumentState == NULL)
        return def;

    if (_lblInstrumentState->InstrumentTemperature[0].Valid)
	return _lblInstrumentState->InstrumentTemperature[0].Value;
    return def;
}


////////////////////////////////////////////////////////////////////////
// Get the Rover Motion Counter for the file.  The array must be supplied.
// num_index on input should contain the max size of the supplied array;
// on output it will contain the number of elements actually found (which
// could be bigger than the supplied max size, but only that many will
// actually be returned in the array).  PIG_MAX_SITE_INDEX (from
// PigRoverStateManager.h) can be useful for array dimensioning.
////////////////////////////////////////////////////////////////////////

void PigFileModelNSYT::getRoverMotionCounter(int indices[], int &num_indices)
{   
    int max_indices = num_indices;
    num_indices = 0;

    const LblIdentification_typ *lblIdentification = getLblIdentification();

    //getSite
    if(!lblIdentification->RoverMotionCounter[0].Valid) {
        printWarning("SITE not found in label!  1 assumed");
        indices[0] = 1;
    }
    else {
      indices[0] = lblIdentification->RoverMotionCounter[0].Value;
      num_indices++;
    }

    //getDrive
    if(!lblIdentification->RoverMotionCounter[1].Valid) {
        printWarning("DRIVE not found in label!  0 assumed");
        indices[1] = 0;
    }
    else {
      indices[1] = lblIdentification->RoverMotionCounter[1].Value;
      num_indices++;
    }

    return;
}

////////////////////////////////////////////////////////////////////////
// Returns the nominal number of RMC elements for this mission, or 0
// if none.  A given image may not have all elements specified; this
// call returns the max.
////////////////////////////////////////////////////////////////////////
int PigFileModelNSYT::getRoverMotionCounterCount()
{
    return 2;
}

////////////////////////////////////////////////////////////////////////
// Returns the string names of each RMC element.  String is statically
// allocated so user need not free.  Returns NULL if index out of range.
////////////////////////////////////////////////////////////////////////
char *PigFileModelNSYT::getRoverMotionCounterName(int index)
{
    static char *names[] = {
        "site", "drive"
    };
    if (index < 0 || index >= getRoverMotionCounterCount())
	return NULL;
    return names[index];
}

////////////////////////////////////////////////////////////////////////
// This is a set of routines used to get sets of mechanism angles or
// positions for a mission.  A set might be all the mobility angles
// (steering, bogie, differential), or the camera mast az/el, or arm
// joints.  The intent of these routines is to provide info for the
// PLACES database in a mission-independent manner.
////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////
// Get the number of sets for this mission
////////////////////////////////////////////////////////////////////////
int PigFileModelNSYT::getMechanismSets()
{
    return 1;
}

//!!!!!  THIS WHOLE SECTION NEEDS TO BE UPDATED FOR INSIGHT !!!!!!
//!!!!! PROBABLY NEED TO ADD SEIS, WTS, HP3 INFO, MAYBE MORE !!!!

////////////////////////////////////////////////////////////////////////
// Get info for a mechanism set.  The string should be assumed to
// point to a static area (i.e. caller does not have to allocate or free).
// Returns empty string and 0 if the set number is out of range.
////////////////////////////////////////////////////////////////////////
void PigFileModelNSYT::getMechanismSetInfo(int set, char *&name,
				int &num_elements)
{
    static char *sets[] = { "arm" };
    static int count[] = { 8 };
    name = "";
    num_elements = 0;
    if (set < 0 || set >= getMechanismSets())
	return;
    name = sets[set];
    num_elements = count[set];
    return;
}

////////////////////////////////////////////////////////////////////////
// Get info for a single element (angle, usually).  Again, the returned
// strings should be assumed to point to a static area, so the caller does
// not have to allocate or free.  Returns empty string and 0 if the inputs
// are out of range.  Unit may be an empty string if N/A.
// For this purpose, UNK/NULL/NA is treated as missing.
////////////////////////////////////////////////////////////////////////
void PigFileModelNSYT::getMechanismInfo(int set, int element,
                        char *&name, double &value, char *&unit)
{
    static char *arm_names[] = {
	"enc_azimuth", "enc_elevation", "enc_elbow", "enc_wrist",
	"res_azimuth", "res_elevation", "res_elbow", "res_wrist"
    };
    const LblArticulation_typ *arm_art = NULL;

    name = "";
    value = 0.0;
    unit = "";
    if (set < 0 || set >= getMechanismSets())
	return;
    switch (set) {

	case 0:				// Arm
	    arm_art = getLblArmArticulation();
	    if (arm_art == NULL)
		return;
	    if (element < 0 || element >= 8)
		return;
	    if (arm_art->ArticulationDeviceAngle[element].Valid != LBL_VALID)
		return;
	    value = arm_art->ArticulationDeviceAngle[element].Value;
	    name = arm_names[element];
	    unit = "radians";
	    return;

	default:
	    return;
    }
}

////////////////////////////////////////////////////////////////////////
// This is a bit of a hack.  We use the getArticulationDev() calls to
// be compatible with PHX... but we get the values themselves from the
// Arm Coordinate System group.
////////////////////////////////////////////////////////////////////////

PigPoint PigFileModelNSYT::getArticulationDevLocation(PigPoint def)
{
    const LblCoordinate_typ *cs_lbl = getLblArmCoordSys();

    if (cs_lbl == NULL)
	return def;

    if (!cs_lbl->OriginOffsetVector.Valid)
	return def;
    return PigPoint(cs_lbl->OriginOffsetVector.Value[0],
		    cs_lbl->OriginOffsetVector.Value[1],
		    cs_lbl->OriginOffsetVector.Value[2]);
}

PigQuaternion PigFileModelNSYT::getArticulationDevOrient(PigQuaternion def)
{
    const LblCoordinate_typ *cs_lbl = getLblArmCoordSys();

    if (cs_lbl == NULL)
	return def;
    if (!cs_lbl->OriginRotationQuaternion.Valid)
	return def;
    return PigQuaternion(cs_lbl->OriginRotationQuaternion.Value[0],
			 cs_lbl->OriginRotationQuaternion.Value[1],
			 cs_lbl->OriginRotationQuaternion.Value[2],
			 cs_lbl->OriginRotationQuaternion.Value[3]);
}

