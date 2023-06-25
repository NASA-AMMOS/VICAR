////////////////////////////////////////////////////////////////////////
// PigCameraModel
//
// Base class for Camera models.  Responsible for maintaining calibration
// information for a camera, applying pointing when requested, and doing
// the actual ray projection calculations.
//
// Calibration parameters might consist of initial CAHVOR parameters for that
// model, or things like focal length, boresight offset, and radial distiortion
// for an orbiter model.
//
// The CameraModel maintains an initial, and a current, coordinate system.
// The caller who sets the camera parameters (via subclass functions) must
// set the initial coordinate system as well.  This may be changed via
// setCoordSystem(), but is restored by resetCameraLocation().
////////////////////////////////////////////////////////////////////////

#include "PigCameraModel.h"

#include "PigFileModel.h"
#include "PigPointingModel.h"
#include "PigMission.h"
#include "PigCoordSystem.h"

#include "PigCAHV.h"
#include "PigCAHVOR.h"
#include "PigCAHVORE.h"
#include "PigPSPH.h"

#include <string.h>
#include <stdlib.h>

////////////////////////////////////////////////////////////////////////
// Constructor
////////////////////////////////////////////////////////////////////////

PigCameraModel::PigCameraModel(const char *mission, 
			       const char *instrument,
			       const char *version, 
			       const char *subtype,
			       const char *construction, 
			       const char *calibration)
{
    _mission = _instrument = _version = NULL;
    _subtype = _construction = _calibration = NULL;

    if (mission)
	_mission = strdup(mission);
    if (instrument)
	_instrument = strdup(instrument);
    if (version)
	_version = strdup(version);
    if (subtype)
	_subtype = strdup(subtype);
    if (construction)
	_construction = strdup(construction);
    if (calibration)
	_calibration = strdup(calibration);

    _pointing_model = NULL;

    // default
    _initial_cs = PigMission::getMissionObject(_mission)->getFixedCS();
    _current_cs = _initial_cs;

    _transformVectorValid = FALSE;
    _transformQuatValid = FALSE;
    _interpMethodValid = FALSE;
    _interpValueValid = FALSE;
    strcpy(_interpMethod, "");
}

PigCameraModel::PigCameraModel()
{
    _mission = _instrument = _version = NULL;
    _subtype = _construction = _calibration = NULL;

    _pointing_model = NULL;

    _transformVectorValid = FALSE;
    _transformQuatValid = FALSE;
    _interpMethodValid = FALSE;
    _interpValueValid = FALSE;
    strcpy(_interpMethod, "");
}

////////////////////////////////////////////////////////////////////////
// Destructor
////////////////////////////////////////////////////////////////////////

PigCameraModel::~PigCameraModel()
{
    if (_mission)
	delete _mission;
    if (_instrument)
	delete _instrument;
    if (_version)
	delete _version;
    if (_subtype)
	delete _subtype;
    if (_construction)
	delete _construction;
    if (_calibration)
	delete _calibration;

    if (_pointing_model)
	_pointing_model->setCameraModel(NULL);	// clear pointer to us
}

////////////////////////////////////////////////////////////////////////
// These factory methods create and return an instance of the
// proper subclass for the given camera.  Cameras are specified
// by the file itself (look at the label to figure it out), or by
// the mission name/camera name/subtype (often a filter name).
// The "special" string, if present, allows for standard variations
// recognized by most subclasses (return NULL if the string is unknown):
//    "stereo" - create the "stereo partner" for the specified camera,
//                  if it is stereo.  If not, return NULL.
// If the PigCoordSystem is NULL in the string version, the "natural"
// frame for the given mission/instrument, at the default site, are used.
////////////////////////////////////////////////////////////////////////

PigCameraModel *PigCameraModel::create(const char *filename,const char *special)
{
    PigFileModel *file = PigFileModel::create(filename);
    if (file == NULL)
	return NULL;
    PigCameraModel *camera = create(file, special);
    delete file;
    return camera;
}

PigCameraModel *PigCameraModel::create(PigFileModel *file, const char *special)
{
    PigMission *m = PigMission::getMissionObject(file);
    return m->createCameraModel(file, special);
}

PigCameraModel *PigCameraModel::create(const char *mission,
				       const char *instrument, 
				       const char *version,
				       const char *subtype,
				       const char *special,
				       const char *construction,
				       const char *calibration,
				       PigCoordSystem *cs)
{
    PigMission *m = PigMission::getMissionObject(mission);
    return m->createCameraModel(instrument, version, subtype, special, 
				construction, calibration, cs);
}

////////////////////////////////////////////////////////////////////////
// These factory methods create and return a camera model from a label
// or file.  They differ from readFromFile et al because those are
// subclassed and expect a specific type of CM.  These factories look to
// see what kind of CM to create first, then create the appropriate type,
// and call the readFromFile et al routines to fill it in.
////////////////////////////////////////////////////////////////////////

PigCameraModel *PigCameraModel::createFromLabel(PigFileModel *file,
                                const char *name,
                                const char *mission,
                                const char *instrument,
                                const char *version,
                                const char *subtype,
                                const char *construction,
                                const char *calibration,
                                PigCoordSystem *cs)
{
    const LblCameraModel_typ *cameraModel;

    // Find the instance corresponding to this camera model name

    int instance = 1;
    while ((cameraModel = file->getLblCameraModel(instance)) != NULL) {
	if (cameraModel->ModelName.Valid &&
	    strcasecmp(cameraModel->ModelName.Value, name) == 0) {

	    break;		// found the right one
	}
    }

    if (cameraModel == NULL) {		// no match
	char msg[256];
	sprintf(msg, "Unable to find camera model label matching name '%s'.",
				name);
	printStaticMsg(msg, PigMsgError);
	return NULL;
    }

    return createFromLabel(file, instance, mission, instrument, version,
			subtype, construction, calibration, cs);
}

////////////////////////////////////////////////////////////////////////

PigCameraModel *PigCameraModel::createFromLabel(PigFileModel *file,
                                int instance,
                                const char *mission,
                                const char *instrument,
                                const char *version,
                                const char *subtype,
                                const char *construction,
                                const char *calibration,
                                PigCoordSystem *cs)
{
    const LblCameraModel_typ *cameraModel;
    PigCameraModel *cm = NULL;
    char msg[256];
    int status;

    cameraModel = file->getLblCameraModel(instance);
    if (cameraModel == NULL) {
	sprintf(msg, "Unable to find camera model label instance %d", instance);
	printStaticMsg(msg, PigMsgError);
	return NULL;
    }

    if (!cameraModel->ModelType.Valid) {
	printStaticMsg("Missing ModelType in camera model label", PigMsgError);
	return NULL;
    }

    if (strcasecmp(cameraModel->ModelType.Value, "CAHV") == 0)
	cm = new PigCAHV(mission, instrument, version,
				subtype, construction, calibration);
    else if (strcasecmp(cameraModel->ModelType.Value, "CAHVOR") == 0)
	cm = new PigCAHVOR(mission, instrument, version,
				subtype, construction, calibration);
    else if (strcasecmp(cameraModel->ModelType.Value, "CAHVORE") == 0)
	cm = new PigCAHVORE(mission, instrument, version,
				subtype, construction, calibration);
    else if (strcasecmp(cameraModel->ModelType.Value, "PSPH") == 0)
	cm = new PigPSPH(mission, instrument, version,
				subtype, construction, calibration);
    else {
	sprintf(msg, "Unrecognized camera model type: '%s'",
				cameraModel->ModelType.Value);
	printStaticMsg(msg, PigMsgError);
	return NULL;
    }

    cm->setInitialCoordSystem(cs);

    status = cm->readFromLabel(file, instance);

    if (status == 0)		// success!
	return cm;

    delete cm;
    return NULL;
}

////////////////////////////////////////////////////////////////////////

PigCameraModel *PigCameraModel::createFromFile(const char *filename,
                                const char *mission,
                                const char *instrument,
                                const char *version,
                                const char *subtype,
                                const char *construction,
                                const char *calibration,
                                PigCoordSystem *cs)
{
    FILE *f;
    char fn[PIG_MAX_FILENAME_SIZE];
    PigCameraModel *cm = NULL;
    int status;

    // Add each extension and check for the file being present

    strcpy(fn, filename);
    char *dot = strrchr(fn, '.');		// look for extension
    if (!dot)
	dot = fn + strlen(fn);			// no extension, append to end

    // Check for CAHV

    strcpy(dot, ".cahv");
    if ((f = fopen(fn, "r")) != NULL) {
	fclose(f);				// It's CAHV
	cm = new PigCAHV(mission, instrument, version,
				subtype, construction, calibration);
    }

    // Check for CAHVOR

    if (cm == NULL) {

        strcpy(dot, ".cahvor");
        if ((f = fopen(fn, "r")) != NULL) {
	    fclose(f);				// It's CAHVOR
	    cm = new PigCAHVOR(mission, instrument, version,
				subtype, construction, calibration);
	}
    }

    // Check for CAHVORE

    if (cm == NULL) {

        strcpy(dot, ".cahvore");
        if ((f = fopen(fn, "r")) != NULL) {
	    fclose(f);				// It's CAHVORE
	    cm = new PigCAHVORE(mission, instrument, version,
				subtype, construction, calibration);
	}
    }

    // Check for PSPH

    if (cm == NULL) {

        strcpy(dot, ".psph");
        if ((f = fopen(fn, "r")) != NULL) {
	    fclose(f);				// It's PSPH
	    cm = new PigPSPH(mission, instrument, version,
				subtype, construction, calibration);
	}
    }

    if (cm == NULL) {
	char msg[256];
	sprintf(msg, "Unable to find camera model file for '%s'", filename);
	printStaticMsg(msg, PigMsgError);
	return NULL;
    }

    cm->setInitialCoordSystem(cs);

    status = cm->readFromFile(fn, cs);

    if (status == 0)			// success!
	return cm;

    delete cm;
    return NULL;
}

////////////////////////////////////////////////////////////////////////
// Set the pointing model.  Usually done by the PM itself.
////////////////////////////////////////////////////////////////////////

void PigCameraModel::setPointingModel(PigPointingModel *pm)
{
    if (pm == _pointing_model)
	return;					// already set, avoid recursion
    if (_pointing_model != NULL) {
	PigPointingModel *pm = _pointing_model;
	_pointing_model = NULL;			// avoid recursion
	pm->setCameraModel(NULL);
    }
    _pointing_model = pm;
    if (_pointing_model != NULL)
	_pointing_model->setCameraModel(this);
}

////////////////////////////////////////////////////////////////////////
// This is an UNSAFE function.  It must be called only by the PigMission
// object that created the CM because it's the only one that knows if it
// is safe to change the Construction or not.
////////////////////////////////////////////////////////////////////////

void PigCameraModel::setConstruction(const char *construction)
{
    if (_construction)
	delete _construction;
    if (construction != NULL)
	_construction = strdup(construction);
    else
	_construction = NULL;
}

/////////////////////////////////////////////////////////////////
// Accessors for the additional fields.  These are not part of
// the model per se but say something about how it is constructed.
/////////////////////////////////////////////////////////////////

void PigCameraModel::setTransformVector(PigVector v)
{
    _transformVector = v;
    _transformVectorValid = TRUE;
}

void PigCameraModel::setTransformQuaternion(PigQuaternion q)
{
    _transformQuat = q;
    _transformQuatValid = TRUE;
}

int PigCameraModel::getTransformVector(PigVector &v)
{
    v = _transformVector;
    return _transformVectorValid;
}

int PigCameraModel::getTransformQuaternion(PigQuaternion &q)
{
    q = _transformQuat;
    return _transformQuatValid;
}

void PigCameraModel::setInterpType(const char *type)
{
    if (type != NULL && strlen(type) != 0) {
	if (strlen(_interpMethod) != 0)
	    strcat(_interpMethod, ",");
	strcat(_interpMethod, type);
	_interpMethodValid = TRUE;
    }
    else {
	strcpy(_interpMethod, "");
	_interpMethodValid = FALSE;
    }
}

void PigCameraModel::setInterpValue(double val)
{
    _interpValue = val;
    _interpValueValid = TRUE;
}

int PigCameraModel::getInterpType(char *type)
{
    strcpy(type, _interpMethod);
    return _interpMethodValid;
}

int PigCameraModel::getInterpValue(double &val)
{
    val = _interpValue;
    return _interpValueValid;
}

/////////////////////////////////////////////////////////////////
// Transfer the additional fields (above) to another model... but
// only if they don't already exists in To.
/////////////////////////////////////////////////////////////////

void PigCameraModel::transferMetadata(PigCameraModel *to)
{
    if (!to->_transformVectorValid) {
        to->_transformVector = _transformVector;
        to->_transformVectorValid = _transformVectorValid;
    }
    if (!to->_transformQuatValid) {
        to->_transformQuat = _transformQuat;
        to->_transformQuatValid = _transformQuatValid;
    }

    // Copy these as a unit... only if neither are already set
    if (!to->_interpMethodValid && !to->_interpValueValid) {
        strcpy(to->_interpMethod, _interpMethod);
        to->_interpMethodValid = _interpMethodValid;
        to->_interpValue = _interpValue;
        to->_interpValueValid = _interpValueValid;
    }
}

/////////////////////////////////////////////////////////////////
// Get the algorithm to use for epipolar alignment.  There is a default
// per mission (in PigMission) but this can be overridden via CMOD_WARP=n
// in POINT_METHOD.  Currently the values are:
// 1 = old model, used by MER and MSL since 2012
// 2 = new model (2017), which works better for non-traditional stereo
// 3 or PSPH = aligns to (and returns) PSPH model
/////////////////////////////////////////////////////////////////

PigCmodWarpAlgorithm PigCameraModel::getCmodWarpAlgorithm()
{
    char point_method[1024], *value;
    int count;
    PigCmodWarpAlgorithm algo = CMOD_WARP_NONE;

    getParam("POINT_METHOD", point_method, &count, 1, 0);

    // See if the user overrode

    if (count != 0) {
	value = parseParamString(point_method, "CMOD_WARP");
	if (value != NULL) {
	    if (strncmp(value, "1", 1) == 0)
		return CMOD_WARP_1;
	    else if (strncmp(value, "2", 1) == 0)
		return CMOD_WARP_2;
	    else if ((strncmp(value, "3", 1)==0) ||
		     (strncasecmp(value, "PSPH", 4)==0))
		return CMOD_WARP_PSPH;
	    else {
	        char msg[1024];
	        sprintf(msg, "Invalid CMOD_WARP value: %s ignored", value);
	        printWarning(msg);
	    }
	}
    }

    // If not, get the mission default

    PigMission *m = PigMission::getMissionObject(_mission);
    if (m != NULL)
	return m->getDefaultCmodWarpAlgorithm(_instrument);

    return CMOD_WARP_1;		// ultimate default
}

