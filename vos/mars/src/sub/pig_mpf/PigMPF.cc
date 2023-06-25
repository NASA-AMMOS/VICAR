////////////////////////////////////////////////////////////////////////
// PigMPF
//
// Contains all mission-specific code for creating mission-specific subclasses
// of various Pig objects.  See PigMission.
////////////////////////////////////////////////////////////////////////

#include "PigMPF.h"
#include "PigFileModel.h"
#include "PigCAHVOR.h"
#include "PigPointMpfImp.h"
#include "RadiometryMpfImp.h"
#include "PigFileModelMpf.h"
#include "PigPointMpfRover.h"
#include "PigRoverStateManagerMPF.h"
#include "mat3.h"

#include <string.h>
#include <stdlib.h>
#include <iostream>
using namespace std;
#include <stdio.h>

////////////////////////////////////////////////////////////////////////
// Mars Pathfinder Camera Models.  These should really go in files
// somewhere, but this is the way the original was done...
// These are all in Lander coordinates.
////////////////////////////////////////////////////////////////////////

/* lander engineering model camera left filter 5 (calibrated 2-27-97) */
 static double C11[] ={  -0.269827,     -0.083518,     -0.623922};
 static double A11[] ={   0.999912,      0.012396,      0.004767};
 static double H11[] ={ 119.330862,   1003.515581,     -1.531832};
 static double V11[] ={ 124.472316,      4.258231,   1002.643744};
 static double O11[] ={   0.999887,      0.013374,      0.006695};
 static double R11[] ={   0.000004,      0.022257,     -0.095217};
 
/* lander engineering model camera right filter 5 (calibrated 2-27-97) */
 static double C12[] ={  -0.267944,      0.068547,     -0.623687};
 static double A12[] ={   0.999895,     -0.013326,     -0.005780};
 static double H12[] ={ 140.186034,   1001.991798,      3.680542};
 static double V12[] ={ 126.226375,     -5.643751,   1003.531670};
 static double O12[] ={   0.999886,     -0.013595,     -0.006583};
 static double R12[] ={  -0.000030,      0.007779,     -0.554352};
 
/* flight lander camera left filter 0 */
 static double C7[] ={     -0.108104,     -0.037518,     -0.624781};
 static double A7[] ={     -0.361885,      0.932214,     -0.004003};
 static double H7[] ={   -993.351685,   -253.969894,     -7.135695};
 static double V7[] ={    -48.434330,    107.111832,   1016.950012};
 static double O7[] ={     -0.362351,      0.932037,     -0.003054};
 static double R7[] ={     -0.000002,      0.032242,     -0.181680};
 
/* flight lander camera right filter 0 */
 static double C8[] ={     -0.250134,     -0.091110,     -0.624520};
 static double A8[] ={     -0.337255,      0.941370,      0.009011};
 static double H8[] ={   -998.308899,   -215.706863,     -2.351819};
 static double V8[] ={    -44.249931,    111.091103,   1013.696289};
 static double O8[] ={     -0.334371,      0.942413,      0.007365};
 static double R8[] ={      0.000012,      0.049300,     -0.050680};
 
/* flight lander camera left filter 5 */
 static double C2[] ={     -0.109595,     -0.033987,     -0.624660};
 static double A2[] ={     -0.361886,      0.932211,     -0.004559};
 static double H2[] ={   -992.206177,   -253.815872,     -6.994257};
 static double V2[] ={    -48.516823,    107.315269,   1015.932922};
 static double O2[] ={     -0.361433,      0.932387,     -0.004661};
 static double R2[] ={      0.000000,     -0.013529,     -0.052813};
 
/* flight lander camera right filter 5 */
 static double C3[] ={     -0.252486,     -0.084549,     -0.624226};
 static double A3[] ={     -0.333411,      0.942745,      0.008285};
 static double H3[] ={   -996.403809,   -214.824554,     -2.672969};
 static double V3[] ={    -43.346863,    111.057312,   1011.621826};
 static double O3[] ={     -0.330888,      0.943619,      0.009790};
 static double R3[] ={     -0.000030,     -0.003814,      0.627156};
 
/* flight lander camera left filter11 */
 static double C9[] ={     -0.107809,     -0.038414,     -0.624445};
 static double A9[] ={     -0.360739,      0.932655,     -0.004747};
 static double H9[] ={   -993.254761,   -253.764221,     -6.939358};
 static double V9[] ={    -48.598488,    107.475128,   1017.553406};
 static double O9[] ={     -0.361037,      0.932541,     -0.004418};
 static double R9[] ={     -0.000013,      0.002740,      0.360291};

/* flight lander camera right filter 11 */
 static double C10[] ={     -0.249636,     -0.092126,     -0.624229};
 static double A10[] ={     -0.336255,      0.941664,      0.014192};
 static double H10[] ={   -998.699524,   -215.911972,     -1.325855};
 static double V10[] ={    -43.739658,    111.065987,   1015.114319};
 static double O10[] ={     -0.335013,      0.942119,      0.013351};
 static double R10[] ={     -0.000001,      0.040033,     -0.182060};
 
/* rover left front camera */
 static double C0[] ={      0.213882,     -0.062790,     -0.086008};
 static double A0[] ={      0.925883,     -0.005100,      0.377777};
 static double H0[] ={    357.939117,    381.352264,    148.233246};
 static double V0[] ={    112.964831,     -3.230871,    399.078613};
 static double O0[] ={      0.927527,     -0.006309,      0.373704};
 static double R0[] ={      0.000009,     -0.336686,      0.105484};

/* rover right front camera */
 static double C1[] ={      0.214828,      0.062790,     -0.087117};
 static double A1[] ={      0.926768,     -0.004059,      0.375613};
 static double H1[] ={    350.559479,    381.763733,    138.739563};
 static double V1[] ={     97.513293,      1.418743,    391.958560};
 static double O1[] ={      0.930370,     -0.011777,      0.366432};
 static double R1[] ={      0.000144,     -0.335626,      0.103943};

/* rover rear (color) camera */
 static double C4[] ={     -0.214522,      0.105816,     -0.096641};
 static double A4[] ={     -0.775782,     -0.016059,      0.630797};
 static double H4[] ={   -503.961761,      1.325661,    -16.445467};
 static double V4[] ={   -195.940821,   -286.257606,    151.438735};
 static double O4[] ={     -0.774756,     -0.017774,      0.632011};
 static double R4[] ={     -0.000010,     -0.369097,      0.142449};
 
PigRoverStateManager *PigMPF::_mpf_rsm = NULL;

PigCoordSystem *PigMPF::_mpf_cs_db[PIG_MAX_CS];
int PigMPF::_mpf_cs_db_count = 0;
PigCoordSystem *PigMPF::_mpf_fixed_cs = NULL;


////////////////////////////////////////////////////////////////////////
// Create an MPF camera model given an image file.
////////////////////////////////////////////////////////////////////////

PigCameraModel *PigMPF::createCameraModel(PigFileModel *file,
					  const char *special)
{
    char filter[100];

    const char *inst_id = file->getInstrumentId();
    PigCoordSystem *cs = getCoordSystem(file, NULL);

    if (inst_id && strcasecmp(inst_id, "IMP") == 0) {

	strcpy(filter, file->getFilterNumber());

//!!!! "flight" should not be hardcoded for version!!!!

	const char *image_id = file->getImageId();
	// Even image id == left camera (!)
	if (image_id && (image_id[strlen(image_id)-1] % 2) == 0)
	    return createCameraModel("IMP Left", "flight", filter, 
				     special, NULL, NULL, cs);
	else
	    return createCameraModel("IMP Right", "flight", filter, 
				     special,NULL, NULL, cs);
    }
    else {			// Must be rover
	return createCameraModel(file->getInstrumentName(), "flight", "",
				 special, NULL, NULL, cs);
    }

    return NULL;		// never reached
}

////////////////////////////////////////////////////////////////////////
// Create an MPF camera model given strings.  Valid strings are:
// instrument:  "IMP Left", "IMP Right", "ROVER CAMERA LEFT",
//		"ROVER CAMERA RIGHT", "ROVER CAMERA REAR"
// version:  "flight", "engineering"
// subtype:  string indicating the filter number.
// special:  standard string (across all subclasses).  "stereo" will return
//	     the "stereo partner" of the model instead.
//
// Version applies only to IMP, there are no numbers for non-flight rover
// cameras.  The Filter also applies only to IMP Flight cameras (engineering
// models don't distinguish the filter).
//
// All strings are case-insensitive.
//
// If the PigCoordSystem is NULL, the "natural" frame for the given
// mission/instrument, at the default site, are used.
////////////////////////////////////////////////////////////////////////

PigCameraModel *PigMPF::createCameraModel(const char *instrument,
					  const char *version, 
					  const char *subtype, 
					  const char *special,
					  const char *construction,
					  const char *calibration,
					  PigCoordSystem *cs)
{
    PigCAHVOR *model;
    int filter;
    int stereo;
    char msg[256];

    if (instrument == NULL)
	return NULL;

    if (cs == NULL)			// use default, natural frame
	cs = getCoordSystem(getNaturalFrame(instrument));
    else
	cs = getCoordSystem(cs, getNaturalFrame(instrument));


    // Check the "special" string

    stereo = 0;
    if (special != NULL && strlen(special) > 0) {
	if (strcasecmp(special, "stereo") == 0)
	    stereo = 1;
	else
	    return NULL;		// unknown special string
    }

    if (stereo) {		// swap instrument names
	if (strcasecmp(instrument, "IMP Left") == 0)
	    instrument = "IMP Right";
	else if (strcasecmp(instrument, "IMP Right") == 0)
	    instrument = "IMP Left";
	else if (strcasecmp(instrument, "ROVER CAMERA LEFT") == 0)
	    instrument = "ROVER CAMERA RIGHT";
	else if (strcasecmp(instrument, "ROVER CAMERA RIGHT") == 0)
	    instrument = "ROVER CAMERA LEFT";
	else
	    return NULL;		// no stereo for ROVER REAR or unknown
    }

    // All Pathfinder cameras use CAHVOR, so we can create the subclass
    // now then fill in the parameters.

    model = new PigCAHVOR("MPF", instrument, version, 
			  subtype, construction, calibration);
    model->setInitialCoordSystem(cs);

    if (subtype != NULL && strlen(subtype) > 0)
        filter = atoi(subtype);
    else
        filter = 5;			// default

    if (strcasecmp(instrument, "IMP Left") == 0) {
	if (version != NULL && strcasecmp(version, "engineering") == 0) {
	    model->setInitialCAHVOR(C11, A11, H11, V11, O11, R11, cs);
	}
	else {
	    if (filter != 0 && filter != 5 && filter != 11) {
		sprintf(msg,"No MPF camera model for %s %s filter %d.  Filter 5 assumed",
					instrument, version, filter);
		printWarning(msg);
	    }
	    if (filter == 0)
		model->setInitialCAHVOR(C7, A7, H7, V7, O7, R7, cs);
	    else if (filter == 11)
		model->setInitialCAHVOR(C9, A9, H9, V9, O9, R9, cs);
	    else					// filter 5
		model->setInitialCAHVOR(C2, A2, H2, V2, O2, R2, cs);
	}
    }
    else if (strcasecmp(instrument, "IMP Right") == 0) {
	if (version != NULL && strcasecmp(version, "engineering") == 0) {
	    model->setInitialCAHVOR(C12, A12, H12, V12, O12, R12, cs);
	}
	else {
	    if (filter != 0 && filter != 5 && filter != 11) {
		sprintf(msg,"No MPF camera model for %s %s filter %d.  Filter 5 assumed",
					instrument, version, filter);
		printWarning(msg);
	    }
	    if (filter == 0)
		model->setInitialCAHVOR(C8, A8, H8, V8, O8, R8, cs);
	    else if (filter == 11)
		model->setInitialCAHVOR(C10, A10, H10, V10, O10, R10, cs);
	    else					// filter 5
		model->setInitialCAHVOR(C3, A3, H3, V3, O3, R3, cs);
	}
    }
    else if (strcasecmp(instrument, "ROVER CAMERA LEFT") == 0)
	model->setInitialCAHVOR(C0, A0, H0, V0, O0, R0, cs);
    else if (strcasecmp(instrument, "ROVER CAMERA RIGHT") == 0)
	model->setInitialCAHVOR(C1, A1, H1, V1, O1, R1, cs);
    else if (strcasecmp(instrument, "ROVER CAMERA REAR") == 0)
	model->setInitialCAHVOR(C4, A4, H4, V4, O4, R4, cs);
    else {
	sprintf(msg, "Unknown MPF instrument %s.  IMP Left 5 assumed.",
				instrument);
	printWarning(msg);
	model->setInitialCAHVOR(C2, A2, H2, V2, O2, R2, cs);
//!!!! should errors return NULL????!!!!
    }

    return model;
}

////////////////////////////////////////////////////////////////////////
// Create an MPF pointing model given an image file.
////////////////////////////////////////////////////////////////////////

PigPointingModel *PigMPF::createPointingModel(PigCameraModel *cm,
					      PigFileModel *file,
					      const char *type,
					      bool allow_type_override)
{
    const char *inst_id = file->getInstrumentId();
    if (inst_id && strcasecmp(inst_id, "IMP") == 0) {

	const char *image_id = file->getImageId();
	// Even image id == left camera (!)
	if (image_id && (image_id[strlen(image_id)-1] % 2) == 0)
	    return createPointingModel(cm, "IMP Left", NULL, true);
	else
	    return createPointingModel(cm, "IMP Right", NULL, true);
    }
    else {			// Must be rover
      	return createPointingModel(cm, file->getInstrumentName(), NULL, true);
    }

    return NULL;		// never reached
}

////////////////////////////////////////////////////////////////////////
// Create an MPF pointing model given strings.  Valid strings are:
// instrument:  "IMP Left", "IMP Right", "ROVER CAMERA LEFT",
//		"ROVER CAMERA RIGHT", "ROVER CAMERA REAR"
//
// All strings are case-insensitive.
////////////////////////////////////////////////////////////////////////

PigPointingModel *PigMPF::createPointingModel(PigCameraModel *cm,
					      const char *instrument,
					      const char *type,
					      bool allow_type_override)
{
    PigPointingModel *model;

    if (instrument == NULL)
	return NULL;

    // IMP and Rover are pointed differently...

    if (strncasecmp(instrument, "ROVER", 5) == 0) {
        model = new PigPointMpfRover(cm, this, instrument); 
    }
    else {
        //must be the rover
	model = new PigPointMpfImp(cm, this, instrument);
    }

    return model;
}

////////////////////////////////////////////////////////////////////////
// Create an MPF radiometry model given an image file.  There is no non-file
// version (!) so most of the work is deferred to the subclass itself.
////////////////////////////////////////////////////////////////////////

RadiometryModel *PigMPF::createRadiometryModel(PigFileModel *file)
{
    // check for other instruments here... if there were any...

    return RadiometryMpfImp::create(file);
}

////////////////////////////////////////////////////////////////////////
// Create an MPF FileModel.  Since MPF does not use the standard label
// API, we need a special FileModel.
////////////////////////////////////////////////////////////////////////

PigFileModel *PigMPF::createFileModel(const char *filename, int unit) 
{
    return new PigFileModelMpf(filename, unit, getMissionName());
}

////////////////////////////////////////////////////////////////////////
// Return the borders appropriate for the instrument type.
// This should only be called by PigFileModel!!
////////////////////////////////////////////////////////////////////////

int PigMPF::getBorders(PigFileModel *file, int &sl, int &ss,
					   int &el, int &es)
{
    //check if it is Rear Rover'r Camera
    if (strcasecmp(file->getInstrumentName(), "ROVER CAMERA REAR") == 0) {
        sl = 0;
	ss = 5;
	el = 767;
	es = 491;
	return TRUE;
    }
    // Check if it is Left or Right Rover's camera
    if (strncasecmp(file->getInstrumentName(), "ROVER", 5) == 0) {
        sl = 5;
	ss = 0;
	el = 491;
	es = 767;
	return TRUE;
    }
    const char *image_id = file->getImageId();
    // Even image id == left camera (!)
    int left = (image_id && (image_id[strlen(image_id)-1] % 2) == 0);

    //!!!! This test is really bogus because flight vs. engineering is not
    //!!!! in the MPF label.  So there's no way to tell.

    const char *vers_id = file->getInstrumentVersionId();
    if (vers_id && (strcasecmp(vers_id, "EM") == 0 ||
		   strcasecmp(vers_id, "engineering") == 0)) {
	if (left) {
	    sl = 0;
	    ss = 17;
	    el = 244;
	    es = 254;
	    return TRUE;
	}
	else {
	    sl = 0;
	    ss = 1;
	    el = 242;
	    es = 243;
	    return TRUE;
	}
    }
    else {				// Flight camera
	if (left) {
	    sl = 0;
	    ss = 4;
	    el = 245;
	    es = 254;
	    return TRUE;
	}
	else {
	    sl = 0;
	    ss = 1;
	    el = 245;
	    es = 254;
	    return TRUE;
	}
    }

    return FALSE;
}

////////////////////////////////////////////////////////////////////////
// Canonicalize a frame name and get the # of indices it should have.
// The returned strings are pointers to internal strings and MUST NOT
// be modified!
// Strings that must be supported for multimission compatibility:
//     FIXED - fixed frame
//     INSTRUMENT - the "natural" frame for the instrument
//     SITE
//     ROVER
//     LOCAL_LEVEL
// inst_id is used only if the frame is INSTRUMENT and can be NULL
// (if it's NULL and frame is INSTRUMENT, just get the "most common")
// Return 1 for success, 0 for frame not recognized (in which case it's
// as if FIXED was given)
// mask_indices (output) will contain 1 for relevant indices and -1 for
// those that should be wildcarded.  Valid pointer must be provided, or
// NULL for dont-care.  Mask will be filled up to max_indices.
////////////////////////////////////////////////////////////////////////

int PigMPF::canonicalizeFrameName(const char *frame, const char *inst_id,
                char *&canon_frame, char *&short_frame,
                int &max_indices, int *mask_indices)
{
    if (frame == NULL) {
        printError("NULL frame name given, ignored");
        canon_frame = "SITE";     // same as FIXED
        short_frame = "SITE";
        max_indices = 0;
        if (mask_indices)
            mask_indices[0] = 1;
        return 0;
    }

    // Special handling of INSTRUMENT

    if (strcasecmp(frame, "INSTRUMENT") == 0)
        frame = getNaturalFrame(inst_id);

    // FIXED is special

    if ((strcasecmp(frame, "FIXED") == 0) ||
        (strcasecmp(frame, "MFX") == 0) ||              // alias
        (strcasecmp(frame, "SFX") == 0) ||
        (strcasecmp(frame, "Mars Surface Fixed") == 0) ||
        (strcasecmp(frame, "Surface Fixed") == 0)) {    // alias

        canon_frame = "FIXED_FRAME";
        short_frame = "FIXED";
        max_indices = 0;
        if (mask_indices)
            mask_indices[0] = 1;
        return 1;
    }

    if ((strcasecmp(frame, "Site") == 0) ||
        (strcasecmp(frame, "SITE_FRAME") == 0) ||
        (strcasecmp(frame, "Major Site") == 0)) {

        canon_frame = "SITE";
        short_frame = "SITE";
        max_indices = 1;
        if (mask_indices)
            mask_indices[0] = 1;
        return 1;
    }

    if ((strcasecmp(frame, "MLL") == 0) ||              // alias
        (strcasecmp(frame, "LL") == 0) ||
        (strcasecmp(frame, "Mars Local Level") == 0) ||
        (strcasecmp(frame, "Local Level") == 0) ||
        (strcasecmp(frame, "LOCAL_LEVEL_FRAME") == 0) ||
        (strcasecmp(frame, "Local_Level") == 0)) {      // alias

        canon_frame = "MARS LOCAL LEVEL";
        short_frame = "MLL";
        max_indices = 2;
        if (mask_indices) {
            mask_indices[0] = 1;
            mask_indices[1] = 1;
        }
        return 1;
    }

    if ((strcasecmp(frame, "Rover") == 0) ||
	(strcasecmp(frame, "ROVER_FRAME") == 0)) {

        canon_frame = "ROVER";
        short_frame = "ROVER";
        max_indices = 2;
        if (mask_indices) {
            mask_indices[0] = 1;
            mask_indices[1] = 1;
        }
        return 1;
    }

    if ((strcasecmp(frame, "Lander") == 0) ||
	(strcasecmp(frame, "LANDER_FRAME") == 0)) {

        canon_frame = "LANDER";
        short_frame = "LANDER";
        max_indices = 2;
        if (mask_indices) {
            mask_indices[0] = 1;
            mask_indices[1] = 1;
        }
        return 1;
    }
    char msg[256];
    sprintf(msg, "Unknown MPF coordinate system frame: '%s', Ignored", frame);
    printError(msg);

    canon_frame = "SITE"; // same as FIXED
    short_frame = "SITE";
    max_indices = 1;
    if (mask_indices)
        mask_indices[0] = 1;

    return 0;
}

////////////////////////////////////////////////////////////////////////
// Return the RMC indices for this mission.
////////////////////////////////////////////////////////////////////////

const char *PigMPF::getRmcIndexName(int i)
{
    if (i == 0)  return "SITE";
    if (i == 1)  return "DRIVE";

    return "UNKNOWN";   // oops!
}

////////////////////////////////////////////////////////////////////////
// This function converts Euler angles to an equivalent unit quaternion
// which represents rotation.
////////////////////////////////////////////////////////////////////////
PigQuaternion PigMPF::eulerAnglesToQuaternion(double heading, 
					      double pitch, 
					      double roll   ) {
    double rotMatrix[3][3];
    double quaternion[4];
    
    double cosPitch   = cos(pitch);
    double sinPitch   = sin(pitch);
    double cosHeading = cos(heading);
    double sinHeading = sin(heading);
    double cosRoll    = cos(roll);
    double sinRoll    = sin(roll);

    //initialize rotation matrix
    rotMatrix[0][0] =  cosPitch * cosHeading;
    rotMatrix[0][1] = -cosPitch * sinHeading;
    rotMatrix[0][2] =  sinPitch;
    rotMatrix[1][0] =  cosRoll * sinHeading + sinRoll * sinPitch * cosHeading;
    rotMatrix[1][1] =  cosRoll * cosHeading - sinRoll * sinPitch * sinHeading;
    rotMatrix[1][2] = -sinRoll * cosPitch;
    rotMatrix[2][0] =  sinRoll * sinHeading - cosRoll * sinPitch * cosHeading;
    rotMatrix[2][1] =  sinRoll * cosHeading + cosRoll * sinPitch * sinHeading;
    rotMatrix[2][2] =  cosRoll * cosPitch; 

    //compute quaternion
    quatr(rotMatrix, quaternion);

    return PigQuaternion(quaternion[0], 
			 quaternion[1],
			 quaternion[2],
			 quaternion[3]);
}

////////////////////////////////////////////////////////////////////////
// Get the PigRoverStateManager to use for this mission.  It is a singleton
// per mission.
////////////////////////////////////////////////////////////////////////

PigRoverStateManager *PigMPF::getRoverStateManager()
{
    if (_mpf_rsm == NULL) {
        _mpf_rsm = new PigRoverStateManager(this);
        // Add "telemetry" as the lowest priority...
        _mpf_rsm->addPriority("telemetry");
    }
    return _mpf_rsm;
}

