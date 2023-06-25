////////////////////////////////////////////////////////////////////////
// PigFileModelM20
//
// M20-specific File model.  This only needs to handle exceptions to the
// multimission label API.
////////////////////////////////////////////////////////////////////////

#include "PigFileModelM20.h"
#include "PigCSReference.h"

#include "zvproto.h"
#include "applic.h"

#include "return_status.h"

////////////////////////////////////////////////////////////////////////
// Constructor
////////////////////////////////////////////////////////////////////////

PigFileModelM20::PigFileModelM20(const char *filename, 
				 int unit,
				 const char *mission)
		        : PigFileModel(filename, unit, mission)
{
    // overwrites parent's offsets.  For MSL offsets are always 0
    setupOffsets();

    // Get the flag for turret pointing

    _lblTurretCoordSys = NULL;
    _use_sherloc_turret = FALSE;
    char point_method[256], *value;
    int count;
    getStaticParam("POINT_METHOD", point_method, &count, 1, 0);
    if (count != 0) {
	value = parseParamString(point_method, "SHERLOC_POINT");
	if (value != NULL && strcasecmp(value, "TURRET") == 0) {
	    _use_sherloc_turret = TRUE;
	}
    }
}

PigFileModelM20::~PigFileModelM20()
{
}

////////////////////////////////////////////////////////////////////////
// Initializes the x/y offsets, which are the offsets between the camera
// model L/S coordinates, and the "physical" coordinates in the image.
// Note that these physical coordinates are 0-based, e.g. you have to
// add 1 for VICAR files.
//
// For M20 x and y offsets are always 0
////////////////////////////////////////////////////////////////////////

void PigFileModelM20::setupOffsets()
{
    _x_offset = 0;
    _y_offset = 0;
}

////////////////////////////////////////////////////////////////////////
// These functions return complete Label API property structures.
// OVERRIDES of base class to change the names
////////////////////////////////////////////////////////////////////////

PIG_READ_LABEL_STRUCTURE(LblInstrumentState_typ, PigFileModelM20,
	InstrumentState, "InstrumentState", InstrumentState,
	"INSTRUMENT_STATE_PARMS")

////////////////////////////////////////////////////////////////////////
// These functions return basic information from the label.
////////////////////////////////////////////////////////////////////////


////////////////////////////////////////////////////////////////////////
// Get azimuth and elevation.  This is complicated because if the first
// pair (resolvers) is not available, we want to fall back to the 4th
// pair (encoders).  Not available is signaled by a value of 1e30.  So
// we check for < 1e29 to provide a margin for floating-point comparison.
////////////////////////////////////////////////////////////////////////

int PigFileModelM20::checkAzimuth()
{
    if (_lblRsmArticulation == NULL)
	getLblRsmArticulation();
    if (_lblRsmArticulation == NULL)
	return FALSE;
    if (_lblRsmArticulation->ArticulationDeviceAngle[0].Valid &&
	_lblRsmArticulation->ArticulationDeviceAngle[0].Value < 1e29)
	return TRUE;
    if (_lblRsmArticulation->ArticulationDeviceAngle[6].Valid &&
	_lblRsmArticulation->ArticulationDeviceAngle[6].Value < 1e29)
	return TRUE;
    return FALSE;
}

int PigFileModelM20::checkElevation()
{
    if (_lblRsmArticulation == NULL)
	getLblRsmArticulation();
    if (_lblRsmArticulation == NULL)
	return FALSE;
    if (_lblRsmArticulation->ArticulationDeviceAngle[1].Valid &&
	_lblRsmArticulation->ArticulationDeviceAngle[1].Value < 1e29)
	return TRUE;
    if (_lblRsmArticulation->ArticulationDeviceAngle[7].Valid &&
	_lblRsmArticulation->ArticulationDeviceAngle[7].Value < 1e29)
	return TRUE;
    return FALSE;
}

float PigFileModelM20::getAzimuth(float def)
{
    if (_lblRsmArticulation == NULL)
	getLblRsmArticulation();
    if (_lblRsmArticulation == NULL)
	return def;
    if (_lblRsmArticulation->ArticulationDeviceAngle[0].Valid &&
	_lblRsmArticulation->ArticulationDeviceAngle[0].Value < 1e29)
	return _lblRsmArticulation->ArticulationDeviceAngle[0].Value;
    if (_lblRsmArticulation->ArticulationDeviceAngle[6].Valid &&
	_lblRsmArticulation->ArticulationDeviceAngle[6].Value < 1e29)
	return _lblRsmArticulation->ArticulationDeviceAngle[6].Value;
    return def;
}

float PigFileModelM20::getElevation(float def)
{
    if (_lblRsmArticulation == NULL)
	getLblRsmArticulation();
    if (_lblRsmArticulation == NULL)
	return def;
    if (_lblRsmArticulation->ArticulationDeviceAngle[1].Valid &&
	_lblRsmArticulation->ArticulationDeviceAngle[1].Value < 1e29)
	return _lblRsmArticulation->ArticulationDeviceAngle[1].Value;
    if (_lblRsmArticulation->ArticulationDeviceAngle[7].Valid &&
	_lblRsmArticulation->ArticulationDeviceAngle[7].Value < 1e29)
	return _lblRsmArticulation->ArticulationDeviceAngle[7].Value;
    return def;
}

////////////////////////////////////////////////////////////////////////
// Determine instrument temperature using sensors data
//////////////////////////////////////////////////////////////////////
// Cameras have a variety of temperature sensors.  The following are the generic
// rules for the "best" one to use.
//
// 1. Use the CCD temp of said camera.
// 2. Use the CCD temp of left/right partner.
// 3. Use the CCD temp of alternate A/B side side camera.
// 4. Use the CCD temp of alternate A/B side left/right partner.
// 5. Use the CCD temp of "similar" camera (i.e., mast, arm, haz)
// 6. Use CCD temperature from any camera.
// 7. Use the electronics temperature of said camera.
// 8. Use the electronics temperature of partner cameras, in the order of
//    2-4 above.
// 9. Use the electronics temperature of similar camera, in the order of
// 1-4 above.
// 10.Use default.
//
// #7-10 are a last resort in view of the fact that M20 will be operating
// warmup heaters inside the electronics (during nighttime and early morning)
// that will raise camera electronics temperatures above CCD temperatures.
// Thus any CCD temperature is at higher priority than any electronics
// temperature measurement.
//
// These rules are copied straight from the MSL rules, which are in turn
// a modification of the MER rules, which came from Keith Novak from the MER
// thermal team.
//
// The sensors, in order, for the engineering cameras (including cachecam):
// 0 A_FRONT_LEFT_HAZ_1
// 1 A_FRONT_LEFT_HAZ_2
// 2 A_FRONT_RIGHT_HAZ_1
// 3 A_FRONT_RIGHT_HAZ_2
// 4 B_FRONT_LEFT_HAZ_1
// 5 B_FRONT_LEFT_HAZ_2
// 6 B_FRONT_RIGHT_HAZ_1
// 7 B_FRONT_RIGHT_HAZ_2
// 8 REAR_LEFT_HAZ_1
// 9 REAR_LEFT_HAZ_2
// 10 REAR_RIGHT_HAZ_1
// 11 REAR_RIGHT_HAZ_2
// 12 NAVCAM_LEFT_1
// 13 NAVCAM_LEFT_2
// 14 NAVCAM_RIGHT_1
// 15 NAVCAM_RIGHT_2
// 16 CACHE_1
// 17 CACHE_2
// 18 FRONT_HAZ_LEFT_CAL
// 19 FRONT_HAZ_RIGHT_CAL
// 20 NAVCAM_LEFT_CAL
// 21 NAVCAM_RIGHT_CAL
//
// Unfortunately none of those are on the actual detector(!!).  So we use
// _CAL first, which is on the camera mounting plate, and then _1 and _2
// which are in the electronics.
//
// For ZCAM, Watson, ACI:
// 0 DEA
// 1 HEAD_FPA
// 2 HEAD_HTR_1
// 3 HEAD_HTR_2
//
// For RMI:
// 0 MU_OBOX_TEMP
// 1 MU_LASER_IF_TEMP
// 2 MU_EBOX_TEMP
// 3 MU_SEC_MIRROR_TEMP
// 4 RAMP_TEMP
//
// For MCC:
// None listed... (!)
//
// For SkyCam:
// 0 MEDA_CCD
// 1 MEDA_PCB
//
// For Heli, LVS, EDLcams:
// None listed
//
// If a sensor breaks, hardware people usually see it as an infinite
// resistance and thus it reads a full scale DN value.  We check against
// large DN value (50 C) as the max criterion for ignoring one sensor's
// data and moving on to the next one.
//
// Most users should call the first version.  The second is a special
// version used ONLY for camera model interpolation.  Here's what it does:
// * If INTERPOLATION_METHOD is ONBOARD and INTERPOLATION_VALUE exists:
//   + If the value is equal to either PRT
//     - return the appropriate PRT (this eye or partner eye based on
//       use_partner)
//   + else return the interp value, regardless of use_partner
// * else return the appropriate PRT (based on use_partner)
//
// This is based on: the FSW sets interp_value to the appropriate PRT
// temperature for interpolation.  Unless there's only one model loaded,
// in which case it's the temperature of that model.
//
// Use cases:
// 1) Nominal, both cameras interp'd
//    -> Use PRT values for both eyes (interp_value == prt for this case)
// 2) Only one model loaded in both cameras (no interp)
//    -> Use interp_value for both eyes.  If the one loaded model is one
//       of the ones we have for interp, then this will return that model
//       (it's interpolated but effectively returned  without change
//       because the interp value is the same as the cmod's temperature).
// 3) As in #2, but value happens to equal one of the PRT's
//    -> Use PRT values.  Wrong, but not by much since the actual temp
//       is very close to the model temp.  The results are consistent
//       whether looking at the L or R image.
// 4) One eye has interp models, the other has a constant model
//    -> Eye with interp uses PRT, eye with constant uses that models' temp.
//       This is very wrong, but is a totally off nominal case, and there's
//       no way to do better without flags from the FSW.
//
// Note that the above interp stuff is MSL heritage and may or may not
// apply to M20...
//////////////////////////////////////////////////////////////////////

float PigFileModelM20::getInstrumentTemperature(float def)
{						// InstrumentState
    if (_lblInstrumentState == NULL)
        getLblInstrumentState();
    if (_lblInstrumentState == NULL)
        return def;

    char inst_id[32];
    strcpy(inst_id, "\0");
    strcpy(inst_id, getInstrumentId());
    return getInstrumentTemperatureFromName(def, inst_id);
}

////

float PigFileModelM20::getInstrumentTemperature(float def, int use_partner)
{
    if (_lblInstrumentState == NULL)
        getLblInstrumentState();
    if (_lblInstrumentState == NULL)
        return def;

    char inst_id[32];
    char partner_inst_id[32];
    strcpy(inst_id, "\0");
    strcpy(inst_id, getInstrumentId());

    if (!strcmp(inst_id, "NAVCAM_LEFT"))
	strcpy(partner_inst_id, "NAVCAM_RIGHT");
    else if (!strcmp(inst_id, "NAVCAM_RIGHT"))
	strcpy(partner_inst_id, "NAVCAM_LEFT");
    else if (!strcmp(inst_id, "FRONT_HAZCAM_LEFT_A"))
	strcpy(partner_inst_id, "FRONT_HAZCAM_RIGHT_A");
    else if (!strcmp(inst_id, "FRONT_HAZCAM_RIGHT_A"))
	strcpy(partner_inst_id, "FRONT_HAZCAM_LEFT_A");
    else if (!strcmp(inst_id, "FRONT_HAZCAM_LEFT_B"))
	strcpy(partner_inst_id, "FRONT_HAZCAM_RIGHT_B");
    else if (!strcmp(inst_id, "FRONT_HAZCAM_RIGHT_B"))
	strcpy(partner_inst_id, "FRONT_HAZCAM_LEFT_B");
    else if (!strcmp(inst_id, "REAR_HAZCAM_LEFT"))
	strcpy(partner_inst_id, "REAR_HAZCAM_RIGHT");
    else if (!strcmp(inst_id, "REAR_HAZCAM_RIGHT"))
	strcpy(partner_inst_id, "REAR_HAZCAM_LEFT");
    else if (!strcmp(inst_id, "MCZ_LEFT"))
	strcpy(partner_inst_id, "MCZ_RIGHT");
    else if (!strcmp(inst_id, "MCZ_RIGHT"))
	strcpy(partner_inst_id, "MCZ_LEFT");
    else					// no partners
	strcpy(partner_inst_id, inst_id);

    // Look at the interpolation keywords, use them if they exist.
    // If not, return the standard temperature.

    int have_interp_kwds = FALSE;
    float interp_value = 0.0;

    if (_lblCameraModel == NULL)
        getLblCameraModel();
    if (_lblCameraModel != NULL) {
	if (_lblCameraModel->InterpolationMethod.Valid) {
	    if (strcmp(_lblCameraModel->InterpolationMethod.Value, "ONBOARD")
									== 0) {
	        if (_lblCameraModel->InterpolationValue.Valid) {
		    interp_value = _lblCameraModel->InterpolationValue.Value;
		    have_interp_kwds = TRUE;
		}
	    }
	}
    }

    // Sanity check... same as in m20edrgen

    if (interp_value > 50 || interp_value < -128)
	have_interp_kwds = FALSE;

    if (!have_interp_kwds) {		// Keywords not there
	if (use_partner)
	    return getInstrumentTemperatureFromName(def, partner_inst_id);
	else
	    return getInstrumentTemperatureFromName(def, inst_id);
    }

    float my_temp = getInstrumentTemperatureFromName(def, inst_id);
    float partner_temp = getInstrumentTemperatureFromName(def, partner_inst_id);

#define TEMPERATURE_EPSILON 1e-3
    if ((fabs(interp_value - my_temp) < TEMPERATURE_EPSILON) ||
	(fabs(interp_value - partner_temp) < TEMPERATURE_EPSILON)) {
	if (use_partner)
	    return getInstrumentTemperatureFromName(def, partner_inst_id);
	else
	    return getInstrumentTemperatureFromName(def, inst_id);
    }

    return interp_value;
}


// PRIVATE temperature routine used only by the above

float PigFileModelM20::getInstrumentTemperatureFromName(
				float def, const char *inst_id)
{

//!!!!!!!! NOT UPDATED FOR MSL !!!!
//!!!!!!!! Names are but numbers definitely ARE NOT!!
    short int num_sensors = 24;		// for eng cam

    // Priority array for each supported camera.  The order is
    // determined by the rules described above.  The values
    // represent position in INSTRUMENT_TEMPERATURE array.
    // !!!! Currently only Engineering cameras are supported.

    short int navcam_left[] =  {20, 21, 18, 19,
	12, 13, 14, 15, 0, 1, 4, 5, 2, 3, 6, 7, 8, 9, 10, 11, 16, 17};
    short int navcam_right[] = {21, 20, 19, 18,
	14, 15, 12, 13, 2, 3, 6, 7, 0, 1, 4, 5, 10, 11, 8, 9, 16, 17};
    short int front_hazcam_left_a[] = {18, 19, 20, 21,
	0, 1, 4, 5, 2, 3, 6, 7, 12, 13, 14, 15, 8, 9, 10, 11, 16, 17};
    short int front_hazcam_right_a[] ={19, 18, 21, 20,
	2, 3, 6, 7, 0, 1, 4, 5, 14, 15, 12, 13, 10, 11, 8, 9, 16, 17};
    short int front_hazcam_left_b[] = {18, 19, 20, 21,
	5, 6, 0, 1, 6, 7, 2, 3, 12, 13, 14, 15, 8, 9, 10, 11, 16, 17};
    short int front_hazcam_right_b[] ={19, 18, 21, 20,
	6, 7, 2, 3, 4, 5, 0, 1, 14, 15, 12, 13, 10, 11, 8, 9, 16, 17};
    short int rear_hazcam_left[] =  {8, 9, 10, 11,
	18, 19, 20, 21, 0, 1, 4, 5, 2, 3, 6, 7, 12, 13, 14, 15, 16, 17};
    short int rear_hazcam_right[] = {10, 11, 8, 9,
	19, 18, 21, 20, 2, 3, 6, 7, 0, 1, 4, 5, 14, 15, 12, 13, 16, 17};
    short int cachecam[] = {16, 17,
	18, 19, 20, 21, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15};
    short int mcz[] = {1, 2, 3, 0};
    short int rmi[] = {0, 1, 3, 2, 4};
    short int skycam[] = {0, 1};
    short int default_list[] = {0};

    short int *temp = NULL;

    if (_lblInstrumentState == NULL)
        getLblInstrumentState();
    if (_lblInstrumentState == NULL)
        return def;

    // choose the correct priority array.
    if (!strcmp(inst_id, "NAVCAM_LEFT"))
        temp = navcam_left;
    else if (!strcmp(inst_id, "NAVCAM_RIGHT"))
        temp = navcam_right;
    else if (!strcmp(inst_id, "FRONT_HAZCAM_LEFT_A"))
        temp = front_hazcam_left_a;
    else if (!strcmp(inst_id, "FRONT_HAZCAM_RIGHT_A"))
        temp = front_hazcam_right_a;
    else if (!strcmp(inst_id, "FRONT_HAZCAM_LEFT_B"))
        temp = front_hazcam_left_b;
    else if (!strcmp(inst_id, "FRONT_HAZCAM_RIGHT_B"))
        temp = front_hazcam_right_b;
    else if (!strcmp(inst_id, "REAR_HAZCAM_LEFT"))
        temp = rear_hazcam_left;
    else if (!strcmp(inst_id, "REAR_HAZCAM_RIGHT"))
        temp = rear_hazcam_right;
    else if (!strcmp(inst_id, "CACHECAM"))
	temp = cachecam;
    else if (strcmp(inst_id, "MCZ_LEFT") == 0 ||
	     strcmp(inst_id, "MCZ_RIGHT") == 0 ||
	     strcmp(inst_id, "SHERLOC_WATSON") == 0 ||
	     strcmp(inst_id, "SHERLOC_ACI")) {
	temp = mcz;
	num_sensors = 4;
    }
    else if (strcmp(inst_id, "SUPERCAM_RMI") == 0) {
	temp = rmi;
	num_sensors = 5;
    }
    else if (strcmp(inst_id, "SKYCAM") == 0) {
	temp = skycam;
	num_sensors = 2;
    }
    else {  //camera has not been recognized, assign default
        temp = default_list;
	num_sensors = 1;
    }

    // Now go by priority list, 0.0 is treated as non-reading from
    // the temperature sensor.
    // Note that currently we are not tracking which sensor's value
    // ends up being used.  We might decide to issue warning if, for 
    // example, none of the CCD sensors readings were valid.
    for (int cnt = 0; cnt < num_sensors; cnt++) {
        if (_lblInstrumentState->InstrumentTemperature[temp[cnt]].Valid &&
       _lblInstrumentState->InstrumentTemperature[temp[cnt]].Value != 0.0 &&
       _lblInstrumentState->InstrumentTemperature[temp[cnt]].Value < 50.0) {
            return _lblInstrumentState->InstrumentTemperature[temp[cnt]].Value;
            }
    }

    // if we are here, no valid entry has been found, return the default
    //printMsg("WARNING: Unable to find any valid Temperature readings, will use the default value", 
    //         PigMsgWarning);
    return def;
}

//////////////////////////////////////////////////////////////////////
// Special electronics temperature for MEDA SkyCam
//////////////////////////////////////////////////////////////////////

float PigFileModelM20::getInstrumentElectronicsTemperature(float def)
{
    if (_lblInstrumentState == NULL)
        getLblInstrumentState();
    if (_lblInstrumentState == NULL)
        return def;
    const char *inst = getInstrumentId();

    // If not skycam, just get normal temperature.  Note that other cameras
    // do have electronics temps, they're just not implemented here since
    // they're not needed.  They could be, though.

    if (strcasecmp(inst, "SKYCAM") != 0) {
	return getInstrumentTemperature(def);
    }

    // PCB is the second temperature

    if (_lblInstrumentState->InstrumentTemperature[1].Valid) {
        return _lblInstrumentState->InstrumentTemperature[1].Value;
    }

    return def;
}

////////////////////////////////////////////////////////////////////////
// Get the Rover Motion Counter for the file.  The array must be supplied.
// num_index on input should contain the max size of the supplied array;
// on output it will contain the number of elements actually found (which
// could be bigger than the supplied max size, but only that many will
// actually be returned in the array).  PIG_MAX_RMC_INDEX (from
// PigRoverStateManager.h) can be useful for array dimensioning.
////////////////////////////////////////////////////////////////////////

void PigFileModelM20::getRoverMotionCounter(int indices[], int &num_indices)
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

    // See if the rest apply

    const char *inst_id = getInstrumentId();
    if (strcmp(inst_id, "HELI_NAV") == 0 ||
        strcmp(inst_id, "HELI_RTE") == 0 ||
	strcmp(inst_id, "LCAM") == 0) {

	return;			// HELI and RTE have only 2 entries
    }

    //getPose
    if(!lblIdentification->RoverMotionCounter[2].Valid) {
        printWarning("Pose not found in label!  0 assumed");
        indices[2] = 0;
    }
    else {
      indices[2] = lblIdentification->RoverMotionCounter[2].Value;
      num_indices++;
    }

    //getArm
    if(!lblIdentification->RoverMotionCounter[3].Valid) {
        printWarning("Arm not found in label!  0 assumed");
        indices[3] = 0;
    }
    else {
      indices[3] = lblIdentification->RoverMotionCounter[3].Value;
      num_indices++;
    }

    //getSHA
    if(!lblIdentification->RoverMotionCounter[4].Valid) {
        printWarning("SHA not found in label!  0 assumed");
        indices[4] = 0;
    }
    else {
        indices[4]=lblIdentification->RoverMotionCounter[4].Value;
        num_indices++;
    }

    //getDrill
    if(!lblIdentification->RoverMotionCounter[5].Valid) {
        printWarning("Drill not found in label!  0 assumed");
        indices[5] = 0;
    }
    else {
        indices[5]=lblIdentification->RoverMotionCounter[5].Value;
        num_indices++;
    }

    //getRSM
    if(!lblIdentification->RoverMotionCounter[6].Valid) {
        printWarning("RSM not found in label!  0 assumed");
        indices[6] = 0;
    }
    else {
        indices[6]=lblIdentification->RoverMotionCounter[6].Value;
        num_indices++;
    }

    //getHGA
    if(!lblIdentification->RoverMotionCounter[7].Valid) {
        printWarning("HGA not found in label!  0 assumed");
        indices[7] = 0;
    }
    else {
        indices[7]=lblIdentification->RoverMotionCounter[7].Value;
        num_indices++;
    }

    //getBITCAR
    if(!lblIdentification->RoverMotionCounter[8].Valid) {
        printWarning("BITCAR not found in label!  0 assumed");
        indices[8] = 0;
    }
    else {
        indices[8]=lblIdentification->RoverMotionCounter[8].Value;
        num_indices++;
    }

    //getSEAL
    if(!lblIdentification->RoverMotionCounter[9].Valid) {
        printWarning("SEAL not found in label!  0 assumed");
        indices[9] = 0;
    }
    else {
        indices[9]=lblIdentification->RoverMotionCounter[9].Value;
        num_indices++;
    }

    //getRTT
    if(!lblIdentification->RoverMotionCounter[10].Valid) {
	// Don't warn for missing RTT unless it's PIXL
	if (strcmp(inst_id, "PIXL_MCC") == 0)
            printWarning("RTT not found in label!  0 assumed");
        indices[10] = 0;
    }
    else {
        indices[10]=lblIdentification->RoverMotionCounter[10].Value;
        num_indices++;
    }

    //getPMC
    if(!lblIdentification->RoverMotionCounter[11].Valid) {
	// Don't warn for missing PMC unless it's PIXL
	if (strcmp(inst_id, "PIXL_MCC") == 0)
            printWarning("PMC not found in label!  0 assumed");
        indices[11] = 0;
    }
    else {
        indices[11]=lblIdentification->RoverMotionCounter[11].Value;
        num_indices++;
    }

    return;
}

////////////////////////////////////////////////////////////////////////
// Returns the nominal number of RMC elements for this mission, or 0
// if none.  A given image may not have all elements specified; this
// call returns the max.
////////////////////////////////////////////////////////////////////////
int PigFileModelM20::getRoverMotionCounterCount()
{
    return 12;
}

////////////////////////////////////////////////////////////////////////
// Returns the string names of each RMC element.  String is statically
// allocated so user need not free.  Returns NULL if index out of range.
////////////////////////////////////////////////////////////////////////
char *PigFileModelM20::getRoverMotionCounterName(int index)
{
    static char *names[] = {
    "site", "drive", "pose", "arm", "sha", "drill", "rsm", "hga", "bitcar",
		"seal", "rtt", "pmc"
    };

    static char *heli_names[] = { "flight", "pos" };
    static char *lvs_names[] = { "set", "instance" };

    if (index < 0 || index >= getRoverMotionCounterCount())
	return NULL;

    const char *inst_id = getInstrumentId();
    if ((strcmp(inst_id, "HELI_NAV") == 0 ||
         strcmp(inst_id, "HELI_RTE") == 0) && index < 2) {
	return heli_names[index];
    }

    if ((strcmp(inst_id, "LCAM") == 0) && index < 2) {
	return lvs_names[index];
    }

    return names[index];
}

////////////////////////////////////////////////////////////////////////
// This is a set of routines used to get sets of mechanism angles or
// positions for a mission.  A set might be all the mobility angles
// (steering, bogie, differential), or the camera mast az/el, or arm
// joints.  The intent of these routines is to provide info for the
// PLACES database in a mission-independent manner.
////////////////////////////////////////////////////////////////////////

//!!!!!!!! THE MECHANISM SETS need to be checked for 2020... are they the
//!!!!!!!! same, or what's different.....

////////////////////////////////////////////////////////////////////////
// Get the number of sets for this mission
////////////////////////////////////////////////////////////////////////
int PigFileModelM20::getMechanismSets()
{
    return 5;
}

////////////////////////////////////////////////////////////////////////
// Get info for a mechanism set.  The string should be assumed to
// point to a static area (i.e. caller does not have to allocate or free).
// Returns empty string and 0 if the set number is out of range.
////////////////////////////////////////////////////////////////////////
void PigFileModelM20::getMechanismSetInfo(int set, char *&name,
				int &num_elements)
{
//!!!!!!!! CHECK FOR 2020
    static char *sets[] = { "rsm", "arm", "drive", "hga", "suspension" };
    static int count[] = { 4, 10, 4, 2, 4 };
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
void PigFileModelM20::getMechanismInfo(int set, int element,
                        char *&name, double &value, char *&unit)
{
//!!!!!!!! CHECK FOR 2020
    static char *rsm_names[] = {
	"enc_azimuth", "enc_elevation", "res_azimuth", "res_elevation"
    };
    static char *arm_names[] = {
	"enc_azimuth", "enc_elevation", "enc_elbow", "enc_wrist", "enc_turret",
	"res_azimuth", "res_elevation", "res_elbow", "res_wrist", "res_turret"
    };
    static char *drive_names[] = {
	"steer_LF", "steer_RF", "steer_LR", "steer_RR"
    };
    static char *hga_names[] = {
	"azimuth", "elevation"
    };
    static char *suspension_names[] = {
	"bogie_left", "bogie_right", "diff_left", "diff_right"
    };
    const LblArticulation_typ *rsm_art = NULL;
    const LblArticulation_typ *arm_art = NULL;
    const LblArticulation_typ *drive_art = NULL;
    const LblArticulation_typ *hga_art = NULL;

    name = "";
    value = 0.0;
    unit = "";
    if (set < 0 || set >= getMechanismSets())
	return;
    switch (set) {
	case 0:				// RSM
	    rsm_art = getLblRsmArticulation();
	    if (rsm_art == NULL)
		return;
	    switch (element) {
		case 0:			// enc_azimuth
		    if (rsm_art->ArticulationDeviceAngle[6].Valid != LBL_VALID)
			return;
		    value = rsm_art->ArticulationDeviceAngle[6].Value;
		    break;
		case 1:			// enc_elevation
		    if (rsm_art->ArticulationDeviceAngle[7].Valid != LBL_VALID)
			return;
		    value = rsm_art->ArticulationDeviceAngle[7].Value;
		    break;
		case 2:			// res_azimuth
		    if (rsm_art->ArticulationDeviceAngle[0].Valid != LBL_VALID)
			return;
		    value = rsm_art->ArticulationDeviceAngle[0].Value;
		    break;
		case 3:			// res_elevation
		    if (rsm_art->ArticulationDeviceAngle[1].Valid != LBL_VALID)
			return;
		    value = rsm_art->ArticulationDeviceAngle[1].Value;
		    break;
		default:
		    return;
	    }
	    name = rsm_names[element];
	    unit = "radians";
	    return;

	case 1:				// Arm
	    arm_art = getLblArmArticulation();
	    if (arm_art == NULL)
		return;
	    if (element < 0 || element >= 10)
		return;
	    if (arm_art->ArticulationDeviceAngle[element].Valid != LBL_VALID)
		return;
	    value = arm_art->ArticulationDeviceAngle[element].Value;
	    name = arm_names[element];
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

////////////////////////////////////////////////////////////////////////
// This is a bit of a hack.  We use the getArticulationDev() calls to
// be compatible with PHX... but we get the values themselves from the
// Camera Model group.
// But if _use_sherloc_turret is set, we get the values from the TURRET
// CS instead.
// If the cmod labels aren't present, use TURRET mode anyway (so we have
// *something*...)
////////////////////////////////////////////////////////////////////////

PigPoint PigFileModelM20::getArticulationDevLocation(PigPoint def)
{
    // Normal mode

    if (!_use_sherloc_turret) {
        const LblCameraModel_typ *cm_lbl = getLblCameraModel();

        if (cm_lbl != NULL && cm_lbl->ModelTransformVector.Valid) {
            return PigPoint(cm_lbl->ModelTransformVector.Value[0],
		    	    cm_lbl->ModelTransformVector.Value[1],
		    	    cm_lbl->ModelTransformVector.Value[2]);
	}
    }

    // Didn't find normal mode (or we're in turret mode), look for turret

    const LblCoordinate_typ *tcs = getLblTurretCoordSys();
    if (tcs == NULL)
	return def;
    if (!tcs->OriginOffsetVector.Valid)
	return def;
    return PigPoint(tcs->OriginOffsetVector.Value[0],
		    tcs->OriginOffsetVector.Value[1],
		    tcs->OriginOffsetVector.Value[2]);
}

PigQuaternion PigFileModelM20::getArticulationDevOrient(PigQuaternion def)
{
    if (!_use_sherloc_turret) {
        const LblCameraModel_typ *cm_lbl = getLblCameraModel();

        if (cm_lbl != NULL && cm_lbl->ModelTransformQuaternion.Valid) {
	    return PigQuaternion(cm_lbl->ModelTransformQuaternion.Value[0],
			 	 cm_lbl->ModelTransformQuaternion.Value[1],
			 	 cm_lbl->ModelTransformQuaternion.Value[2],
			 	 cm_lbl->ModelTransformQuaternion.Value[3]);
	}
    }

    const LblCoordinate_typ *tcs = getLblTurretCoordSys();
    if (tcs == NULL)
	 return def;
    if (!tcs->OriginRotationQuaternion.Valid)
	return def;
    return PigQuaternion(tcs->OriginRotationQuaternion.Value[0],
			tcs->OriginRotationQuaternion.Value[1],
			tcs->OriginRotationQuaternion.Value[2],
			tcs->OriginRotationQuaternion.Value[3]);
}

PIG_READ_LABEL_STRUCTURE_STR(LblCoordinate_typ, PigFileModelM20,
	TurretCoordSys, "TurretCoordSys", Coordinate, "TURRET_COORDINATE_SYSTEM")

