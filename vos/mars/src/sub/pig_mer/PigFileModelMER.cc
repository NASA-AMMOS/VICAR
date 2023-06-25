////////////////////////////////////////////////////////////////////////
// PigFileModelMER
//
// MER-specific File model.  This only needs to handle exceptions to the
// multimission label API.
////////////////////////////////////////////////////////////////////////

#include "PigFileModelMER.h"
#include "PigCSReference.h"

#include "zvproto.h"
#include "applic.h"

#include "return_status.h"

////////////////////////////////////////////////////////////////////////
// Constructor
////////////////////////////////////////////////////////////////////////

PigFileModelMER::PigFileModelMER(const char *filename, 
				 int unit,
				 const char *mission)
		        : PigFileModel(filename, unit, mission)
{
    // overwrites parent's offsets.  For MER offsets are always 0
    setupOffsets();
}

PigFileModelMER::~PigFileModelMER()
{
}

////////////////////////////////////////////////////////////////////////
// Initializes the x/y offsets, which are the offsets between the camera
// model L/S coordinates, and the "physical" coordinates in the image.
// Note that these physical coordinates are 0-based, e.g. you have to
// add 1 for VICAR files.
//
// For MER x and y offsets are always 0
////////////////////////////////////////////////////////////////////////

void PigFileModelMER::setupOffsets()
{
    _x_offset = 0; 
    _y_offset = 0;		
}

////////////////////////////////////////////////////////////////////////
// These functions return complete Label API property structures.
// OVERRIDES of base class to change the names for MER
////////////////////////////////////////////////////////////////////////

PIG_READ_LABEL_STRUCTURE_STR(LblArticulation_typ, PigFileModelMER,
	RsmArticulation, "RsmArticulation", Articulation,
						"PMA_ARTICULATION_STATE")

PIG_READ_LABEL_STRUCTURE_STR(LblArticulation_typ, PigFileModelMER,
	ArmArticulation, "ArmArticulation", Articulation,
						"IDD_ARTICULATION_STATE")

PIG_READ_LABEL_STRUCTURE(LblInstrumentState_typ, PigFileModelMER,
        InstrumentState, "InstrumentState", InstrumentState, 
        "INSTRUMENT_STATE_PARMS")


////////////////////////////////////////////////////////////////////////
// These functions return basic information from the label.
////////////////////////////////////////////////////////////////////////

// Scalar numeric item
#define FILE_LABEL(Property,fName,Name,Type)					\
	Type PigFileModelMER::get##fName(Type def)			\
	{								\
	    if (_lbl##Property == NULL)				\
		getLbl##Property();					\
	    if (_lbl##Property == NULL)				\
		return def;						\
	    if (!_lbl##Property->Name.Valid)	       		\
		return def;						\
	    return _lbl##Property->Name.Value;			\
	}

// Scalar numeric item from base class
#define FILE_LABEL2(Property,fName,Name,Type)					\
	Type PigFileModelMER::get##fName(Type def)			\
	{								\
	    if (_lbl##Property == NULL)				\
		getLbl##Property();					\
	    if (_lbl##Property == NULL)				\
		return def;						\
	    if (!_lbl##Property->Name.Valid)	       		\
		return def;						\
	    return _lbl##Property->Name.Value;			\
	}
// Array item, converted to type "Type".  Type must have ctor for Value's type.
#define FILE_LABEL3(Property,fName,Name,Type)				\
	Type PigFileModelMER::get##fName(Type def)			\
	{								\
	    if (_lbl##Property == NULL)				\
		getLbl##Property();					\
	    if (_lbl##Property == NULL)				\
		return def;						\
	    if (!_merLbl##Property->Name.Valid)	       		\
		return def;						\
	    return Type(_merLbl##Property->Name.Value);       		\
	}

FILE_LABEL   (RsmArticulation, Azimuth, 
			  ArticulationDeviceAngle[0], float)
FILE_LABEL   (RsmArticulation, Elevation, 
			  ArticulationDeviceAngle[1], float)

FILE_LABEL   (ArmArticulation, IddJoint1Angle, 
			  ArticulationDeviceAngle[0], float)
FILE_LABEL   (ArmArticulation, IddJoint2Angle, 
			  ArticulationDeviceAngle[1], float)
FILE_LABEL   (ArmArticulation, IddJoint3Angle, 
			  ArticulationDeviceAngle[2], float)
FILE_LABEL   (ArmArticulation, IddJoint4Angle, 
			  ArticulationDeviceAngle[3], float)
FILE_LABEL   (ArmArticulation, IddJoint5Angle, 
			  ArticulationDeviceAngle[4], float)

// In only a few cases is the lack of value significant...
#define CHECK_FILE_LABEL(Property,fName,Name)				\
	int PigFileModelMER::check##fName()				\
	{								\
	    if (_lbl##Property == NULL)				\
		getLbl##Property();					\
	    if (_lbl##Property == NULL)				\
		return FALSE;						\
	    return _lbl##Property->Name.Valid;			\
	}

CHECK_FILE_LABEL(RsmArticulation, Azimuth, ArticulationDeviceAngle[0])
CHECK_FILE_LABEL(RsmArticulation, Elevation, ArticulationDeviceAngle[1])

CHECK_FILE_LABEL(ArmArticulation, IddJoint1Angle, ArticulationDeviceAngle[0])
CHECK_FILE_LABEL(ArmArticulation, IddJoint2Angle, ArticulationDeviceAngle[1])
CHECK_FILE_LABEL(ArmArticulation, IddJoint3Angle, ArticulationDeviceAngle[2])
CHECK_FILE_LABEL(ArmArticulation, IddJoint4Angle, ArticulationDeviceAngle[3])
CHECK_FILE_LABEL(ArmArticulation, IddJoint5Angle, ArticulationDeviceAngle[4])

////////////////////////////////////////////////////////////////////////////////
// MER has 9 temperature sensors.  We use the following rules to determine
// the "best" temperature to use:
// 
// 1. Use the CCD temp of said camera, if it exists.
// Else
// 2. Use the CCD temp of neighboring camera (left/right partner), 
//    if it is available.
// Else
// 3. Use the CCD temp of "similar" camera (i.e., Navcam/Pancam)
// Else
// 4. Use CCD temperature from any camera.
// Else
// 5. Use the electronics temperature of said camera.
// Else
// 6. Use the electronics temperature of similar camera.
// Else
// 7. Use default.
//
// #5-7 are a last resort in view of the fact that MER will be operating
// warmup heaters inside the electronics (during nighttime and early morning)
// that will raise camera electronics temperatures above CCD temperatures.
// Thus any CCD temperature is at higher priority than any electronics 
// temperature measurement.  The most significant consequence of this is
// that MI CCD is the best available proxy for all four Hazcam CCDs.
//
// This rules are according to Keith Novak from the thermal team.
// 
// The available sensors are :
// 0 FRONT HAZ ELECTRONICS
// 1 REAR HAZ ELECTRONICS
// 2 LEFT PAN ELECTRONICS
// 3 LEFT PAN CCD
// 4 RIGHT PAN CCD
// 5 LEFT NAV CCD
// 6 MI CCD
// 7 MI ELECTRONICS
// 8 EDL CCD
//
// The number in front of sensor's name is the position in 
// INSTRUMENT_TEMPERATURE array.  The values in that array are floats
// and represent degC.  The value 0.0, is treated as no-reading from 
// the sensor.
// 
// If the sensor breaks, hardware people usually see it as an infinite 
// resistance and thus it reads a full scale DN value.  We check against
// large DN value (50 C) as the max criterion for ignoring one sensor's 
// data and moving on to the next one.
// 
//////////////////////////////////////////////////////////////////////////////
float PigFileModelMER::getInstrumentTemperature(float def) {// InstrumentState 

    // Priority array for each supported camera.  The order is 
    // determined by the rules described above.  The values
    // represent position in INSTRUMENT_TEMPERATURE array. 
    short int pancam_left[6] = {3, 4, 5, 6, 2, 0};
    short int pancam_right[6] = {4, 3, 5, 6, 2, 0};

    short int navcam_left[6] = {5, 3, 4, 6, 2, 0};
    short int navcam_right[6] = {5, 4, 3, 6, 2, 0};

    short int front_hazcam_left[6] = {6, 3, 5, 4, 0, 1};
    short int front_hazcam_right[6] = {6, 4, 3, 5, 0, 1};
    short int rear_hazcam_left[6] = {6, 3, 5, 4, 1, 0};
    short int rear_hazcam_right[6] = {6, 4, 3, 5, 1, 0};

    short int mi[6] = {6, 3, 4, 5, 7, 0};

    short int edl[6] = {8, 8, 8, 8, 8, 8};

    short int *temp;

    if (_lblInstrumentState == NULL)
        getLblInstrumentState();
    if (_lblInstrumentState == NULL)
        return def;

    char inst_id[32];
    strcpy(inst_id, "\0");
    strcpy(inst_id, getInstrumentId());

    // choose the correct priority array.
    if (!strcmp(inst_id, "PANCAM_LEFT")) {
        temp = pancam_left;
    }
    else if (!strcmp(inst_id, "PANCAM_RIGHT")) {
        temp = pancam_right;
    }
    else if (!strcmp(inst_id, "NAVCAM_LEFT")) {
        temp = navcam_left;
    }
    else if (!strcmp(inst_id, "NAVCAM_RIGHT")) {
        temp = navcam_right;
    }
    else if (!strcmp(inst_id, "FRONT_HAZCAM_LEFT")) {
        temp = front_hazcam_left;
    }
    else if (!strcmp(inst_id, "FRONT_HAZCAM_RIGHT")) {
        temp = front_hazcam_right;
    }
    else if (!strcmp(inst_id, "REAR_HAZCAM_LEFT")) {
        temp = rear_hazcam_left;
    }
    else if (!strcmp(inst_id, "REAR_HAZCAM_RIGHT")) {
        temp = rear_hazcam_right;
    }
    else if (!strcmp(inst_id, "MI")) {
        temp = mi;
    }
    else if (!strcmp(inst_id, "EDL")) {
        temp = edl;
    }
    else {  //camera has not been recognized, assign default
        temp = navcam_left;
    }

    // Now go by priority list, 0.0 is treated as non-reading from
    // the temperature sensor.
    for (int cnt = 0; cnt < 6; cnt++) {
        if (_lblInstrumentState->InstrumentTemperature[temp[cnt]].Valid && 
       _lblInstrumentState->InstrumentTemperature[temp[cnt]].Value != 0.0 &&
       _lblInstrumentState->InstrumentTemperature[temp[cnt]].Value < 50.0) {
	    return _lblInstrumentState->InstrumentTemperature[temp[cnt]].Value;
	}
    }
    
    // if we are here, no valid entry has been found, return the default
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

void PigFileModelMER::getRoverMotionCounter(int indices[], int &num_indices)
{    
    int max_indices = num_indices;
    num_indices = 0;

    const LblIdentification_typ *lblIdentification = getLblIdentification();

    //getSite
    if(!lblIdentification->RoverMotionCounter[0].Valid) {
        printWarning("SITE not found in label!  0 assumed");
        indices[0] = 0;
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

    //getIDD
    if(!lblIdentification->RoverMotionCounter[2].Valid) {
        printWarning("IDD not found in label!  0 assumed");
        indices[2] = 0;
    }
    else {
      indices[2] = lblIdentification->RoverMotionCounter[2].Value;
      num_indices++;
    }

    //getPMA
    if(!lblIdentification->RoverMotionCounter[3].Valid) {
        printWarning("PMA not found in label!  0 assumed");
        indices[3] = 0;
    }
    else {
      indices[3] = lblIdentification->RoverMotionCounter[3].Value;
      num_indices++;
    }

    //getHGA
    if(!lblIdentification->RoverMotionCounter[4].Valid) {
        printWarning("HGA not found in label!  0 assumed");
        indices[4] = 0;
    }
    else {
      indices[4] = lblIdentification->RoverMotionCounter[4].Value;
      num_indices++;
    }

    return;
}

////////////////////////////////////////////////////////////////////////
// Returns the nominal number of RMC elements for this mission, or 0
// if none.  A given image may not have all elements specified; this
// call returns the max.
////////////////////////////////////////////////////////////////////////
int PigFileModelMER::getRoverMotionCounterCount()
{
    return 5;
}

////////////////////////////////////////////////////////////////////////
// Returns the string names of each RMC element.  String is statically
// allocated so user need not free.  Returns NULL if index out of range.
////////////////////////////////////////////////////////////////////////
char *PigFileModelMER::getRoverMotionCounterName(int index)
{
    static char *names[] = {
        "site", "drive", "idd", "pma", "hga"
    };
    if (index < 0 || index >= 5)
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
int PigFileModelMER::getMechanismSets()
{
    return 5;
}

////////////////////////////////////////////////////////////////////////
// Get info for a mechanism set.  The string should be assumed to
// point to a static area (i.e. caller does not have to allocate or free).
// Returns empty string and 0 if the set number is out of range.
////////////////////////////////////////////////////////////////////////
void PigFileModelMER::getMechanismSetInfo(int set, char *&name,
				int &num_elements)
{
    static char *sets[] = { "pma", "idd", "drive", "hga", "suspension" };
    static int count[] = { 2, 10, 4, 2, 3 };
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
void PigFileModelMER::getMechanismInfo(int set, int element,
                        char *&name, double &value, char *&unit)
{
    static char *pma_names[] = {
	"azimuth", "elevation"
    };
    static char *idd_names[] = {
	"enc_azimuth", "enc_elevation", "enc_elbow", "enc_wrist", "enc_turret",
	"pot_azimuth", "pot_elevation", "pot_elbow", "pot_wrist", "pot_turret"
    };
    static char *drive_names[] = {
	"steer_LF", "steer_RF", "steer_LR", "steer_RR",
    };
    static char *hga_names[] = {
	"azimuth", "elevation"
    };
    static char *suspension_names[] = {
	"bogie_left", "bogie_right", "diff"
    };
    const LblArticulation_typ *pma_art = NULL;
    const LblArticulation_typ *idd_art = NULL;
    const LblArticulation_typ *drive_art = NULL;
    const LblArticulation_typ *hga_art = NULL;

    name = "";
    value = 0.0;
    unit = "";
    if (set < 0 || set >= getMechanismSets())
	return;
    switch (set) {
	case 0:				// PMA
	    pma_art = getLblRsmArticulation();
	    if (pma_art == NULL)
		return;
	    switch (element) {
		case 0:			// elevation
		    if (pma_art->ArticulationDeviceAngle[0].Valid != LBL_VALID)
			return;
		    value = pma_art->ArticulationDeviceAngle[0].Value;
		    break;
		case 1:			// enc_elevation
		    if (pma_art->ArticulationDeviceAngle[1].Valid != LBL_VALID)
			return;
		    value = pma_art->ArticulationDeviceAngle[1].Value;
		    break;
		default:
		    return;
	    }
	    name = pma_names[element];
	    unit = "radians";
	    return;

	case 1:				// IDD
	    idd_art = getLblArmArticulation();
	    if (idd_art == NULL)
		return;
	    if (element < 0 || element >= 10)
		return;
	    if (idd_art->ArticulationDeviceAngle[element].Valid != LBL_VALID)
		return;
	    value = idd_art->ArticulationDeviceAngle[element].Value;
	    name = idd_names[element];
	    unit = "radians";
	    return;

	case 2:				// Drive
	    drive_art = getLblChassisArticulation();
	    if (drive_art == NULL)
		return;
	    if (element < 0 || element >= 4)
		return;
	    if (drive_art->ArticulationDeviceAngle[element].Valid != LBL_VALID)
		return;
	    value = drive_art->ArticulationDeviceAngle[element].Value;
	    name = drive_names[element];
	    unit = "radians";
	    return;

	case 3:				// HGA
	    hga_art = getLblHgaArticulation();
	    if (hga_art == NULL)
		return;
	    if (element < 0 || element >= 2)
		return;
	    if (hga_art->ArticulationDeviceAngle[element].Valid != LBL_VALID)
		return;
	    value = hga_art->ArticulationDeviceAngle[element].Value;
	    name = hga_names[element];
	    unit = "radians";
	    return;

	// This is a bit of a different case because the suspension angles
	// are in the drive group in the label.  So we split them out here.
	// Note the +4 offset when accessing the label items.
	case 4:				// Suspension
	    drive_art = getLblChassisArticulation();
	    if (drive_art == NULL)
		return;
	    if (element < 0 || element >= 4)
		return;
	    if (drive_art->ArticulationDeviceAngle[element+4].Valid !=LBL_VALID)
		return;
	    value = drive_art->ArticulationDeviceAngle[element+4].Value;
	    name = suspension_names[element];
	    unit = "radians";
	    return;

	default:
	    return;
    }
}

