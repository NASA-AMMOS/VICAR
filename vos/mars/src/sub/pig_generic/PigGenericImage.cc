////////////////////////////////////////////////////////////////////////
// PigGenericImage
//
// Contains all "mission-specific" code for dealing with generic images
// (those that don't have a recognized mission).  See PigMission.
////////////////////////////////////////////////////////////////////////

#include "PigGenericImage.h"
#include "PigFileModelGeneric.h"
#include "PigCAHVORE.h"
#include "PigPointGenericCamera_0DOF.h"
#include "PigPointGenericCamera_AzEl.h"
#include "PigPointGenericCamera_6dof.h"
#include "PigPointGenericCamera_7dof.h"
#include "PigRoverStateManagerGeneric.h"

#include <string.h>
#include <stdlib.h>
#include <iostream>
#include <fstream>
using namespace std;
#include <stdio.h>

#define PIG_GENERIC_NATURAL_FRAME "CAMERA"

PigRoverStateManager *PigGenericImage::_generic_rsm = NULL;

PigCoordSystem *PigGenericImage::_generic_cs_db[PIG_MAX_CS];
int PigGenericImage::_generic_cs_db_count = 0;
PigCoordSystem *PigGenericImage::_generic_fixed_cs = NULL;

////////////////////////////////////////////////////////////////////////
// Create a generic camera model given an image file.  We first look for
// CAHVORE then CAHVOR then CAHV labels.  If those aren't present, 
// then we obtain the CM by stripping the extension off the filename 
// and adding ".cahvore" or ".cahvor" or ".cahv" and looking for those files. 
// We let the appropriate type of camera model try to load each.  
// Other types of camera models (with their own extensions) can also be added.
//
// This is made complicated because we store the string-ified initial CM
// parameters in the "construction" string of the CM.  This allows things like
// marsmos and marsmcauley to get synthetic camera models (the "string"
// versions of the create) that match what's given.  This must be set after
// the CM is created.
//
// This function looks at the value of the POINT_METHOD parameter.
// It looks for a keyword "CM_NAME" which indicates which instance of the
// camera model labels to load (this is ignored if we go to files).  The
// given instance must match the value of the CAMERA_MODEL_NAME item in
// the label.  If the parameter is not specified, the first camera model
// label instance is used.
////////////////////////////////////////////////////////////////////////

PigCameraModel *PigGenericImage::createCameraModel(PigFileModel *file,
						   const char *special)
{
    int status;
    PigCameraModel *model = NULL;
    char point_method[256];
    int count;
    char *cm_name = NULL;
    char construction[350];

    PigCoordSystem *cs = getCoordSystem(file, NULL);

    // Get the camera model name to use

    getParam("POINT_METHOD", point_method, &count, 1, 0);

    if (count == 1) {
	cm_name = parseParamString(point_method, "CM_NAME");
    }

    // Look for labels

    file->openFile();				// just to make sure

    // named labels

    if (model == NULL) {
	if (cm_name != NULL)
	    model = PigCameraModel::createFromLabel(file,       // file model
					    cm_name,            // instance
					    "Generic",          // mission
					    "prepointed camera",// instrument
					    NULL,               // version
					    NULL,               // subtype
					    NULL,               // construction
					    NULL,               // calibration
					    cs);                // coord_sys
    }

    // instance 1

    if (model == NULL) {
	model = PigCameraModel::createFromLabel(file, 1,
					"Generic", "prepointed camera",
					NULL, NULL, NULL, NULL, cs);
    }

    // Independent file

    if (model == NULL) {
	model = PigCameraModel::createFromFile(file->getFilename(),
				       "Generic", "prepointed camera", 
				       NULL, NULL, NULL, NULL, cs);
    }

    // Done.  Now clean up by setting the "construction" string.

    char msg[256];
    if (model != NULL) {
      sprintf(msg,"Successfully created camera model for file %s", 
	      file->getFilename());
      PigModelBase::printInfo(msg);
	status = model->writeToString(construction, sizeof(construction));
	model->setConstruction(construction);
	return model;
    }

    // Failure.  Print an error and return.
    sprintf(msg,"Unable to find camera model for file %s", 
	    file->getFilename());
    PigModelBase::printStaticMsg(msg, PigMsgFatal);

    return NULL;
}

////////////////////////////////////////////////////////////////////////
// Create a generic camera model given strings.  Problem is, we don't
// *know* any generic camera models just given strings!!  So as a complete
// and total hack, what we do is to encode all of the CM's parameters into
// its "construction" string.  If that version is passed back into this 
// routine, the CM is re-created.  This assumes one is starting with a 
// valid CM and looking for things like the stereo partner.
//
// Because this is a generic camera, instrument, subtype, and special are
// ignored.  Note especially that special=="stereo" will return the exact
// same camera model as without stereo.  This is because there is absolutely
// no way to tell, on a generic image, what the partner might be, or even if
// there is one.
//
// If the PigCoordSystem is NULL, the "natural" frame for the given
// mission/instrument, at the default site, are used.
////////////////////////////////////////////////////////////////////////

PigCameraModel *PigGenericImage::createCameraModel(const char *instrument,
						   const char *version, 
						   const char *subtype, 
						   const char *special,
						   const char *construction,
						   const char *calibration,
						   PigCoordSystem *cs)
{
    PigCameraModel *model;
    int status;
    char msg[256];

    // If there's nothing in the construction string, we can just give up now.

    if (construction == NULL || strlen(construction) == 0) {
	sprintf(msg, "Unable to retrieve generic camera model");
	PigModelBase::printStaticMsg(msg, PigMsgFatal);
	return NULL;
    }

    if (cs == NULL)				// use default, natural frame
	cs = getCoordSystem(getNaturalFrame(instrument));
    else
	cs = getCoordSystem(cs, getNaturalFrame(instrument));

   // Check for CAHVORE

    if (strncmp(construction, "CAHVORE:", 8) == 0) {
	model = new PigCAHVORE("Generic", instrument, version, 
			       subtype, construction, calibration);
	model->setInitialCoordSystem(cs);

	status = model->readFromString(construction, cs);
	if (status != 0) {
	    delete model;
	    PigModelBase::printStaticMsg(
		"Unable to restore generic CAHVORE camera model from string",
		PigMsgFatal);
	    return NULL;
	}

	return model;
    }

    // Check for CAHVOR
    if (strncmp(construction, "CAHVOR:", 7) == 0) {
	model = new PigCAHVOR("Generic", instrument, version, 
			      subtype, construction, calibration);
	model->setInitialCoordSystem(cs);

	status = model->readFromString(construction, cs);
	if (status != 0) {
	    delete model;
	    PigModelBase::printStaticMsg(
		"Unable to restore generic CAHVOR camera model from string",
		PigMsgFatal);
	    return NULL;
	}

	return model;
    }
    // Check for CAHV
    if (strncmp(construction, "CAHV:", 5) == 0) {
	model = new PigCAHV("Generic", instrument, version, 
			    subtype, construction, calibration);
	model->setInitialCoordSystem(cs);

	status = model->readFromString(construction, cs);
	if (status != 0) {
	    delete model;
	    PigModelBase::printStaticMsg(
		"Unable to restore generic CAHV camera model from string",
		PigMsgFatal);
	    return NULL;
	}

	return model;
    }

    PigModelBase::printStaticMsg(
	"Unable to restore generic camera model from string: unknown type",
	PigMsgFatal);

    return NULL;
}

////////////////////////////////////////////////////////////////////////
// Create a Generic pointing model given strings.
//
// This function looks at the value of the POINT_METHOD parameter.
// It looks for a keyword "GENERIC" or "PM" with values of "AzEl" (the default)
// for az/el pointing or "0DOF" for 0-degree-of-freedom (no pointing
// params) pointing model or "6DOF" for 6-DOF (XYZ plus euler angles) or
// "7DOF" for 7-DOF (XYZ plus quaternion).
//
// Note:  "AzEl" is not specifically checked for; if the value is not one of
// the others or the keyword is not present, then AzEl is assumed.
////////////////////////////////////////////////////////////////////////

PigPointingModel *PigGenericImage::createPointingModel(PigCameraModel *cm,
						       const char *instrument,
						       const char *type,
						       bool allow_type_override)
{
    char point_method[256];
    int count;
    char *pointMethodValue = NULL;
    char model_type[30];

    if (type != NULL)
        strcpy(model_type, type);
    else
        strcpy(model_type, "");


    getParam("POINT_METHOD", point_method, &count, 1, 0);

    if (count == 1) {
	pointMethodValue = parseParamString(point_method, "GENERIC");
        if (pointMethodValue == NULL)
	    pointMethodValue = parseParamString(point_method, "PM");
    }

    if (allow_type_override && pointMethodValue != NULL) {
        if (strncasecmp(pointMethodValue, "0DOF", 4) == 0)
            strcpy(model_type, "GenericCamera_0DOF");
        if (strncasecmp(pointMethodValue, "6DOF", 4) == 0)
            strcpy(model_type, "GenericCamera_6DOF");
        if (strncasecmp(pointMethodValue, "7DOF", 4) == 0)
            strcpy(model_type, "GenericCamera_7DOF");
    }

    if (strcasecmp(model_type, "GenericCamera_0DOF") == 0)
        return new PigPointGenericCamera_0DOF(cm, this, NULL);
    if (strcasecmp(model_type, "GenericCamera_6DOF") == 0)
        return new PigPointGenericCamera_6dof(cm, this, NULL);
    if (strcasecmp(model_type, "GenericCamera_7DOF") == 0)
        return new PigPointGenericCamera_7dof(cm, this, NULL);
    return new PigPointGenericCamera_AzEl(cm, this, NULL);
}

////////////////////////////////////////////////////////////////////////
// Create a Generic radiometry model given an image file.  Since we have
// no radiometry for a generic image, simply return NULL.
////////////////////////////////////////////////////////////////////////

RadiometryModel *PigGenericImage::createRadiometryModel(PigFileModel *file)
{
    return NULL;
}

////////////////////////////////////////////////////////////////////////
// Create an Generic FileModel.
////////////////////////////////////////////////////////////////////////

PigFileModel *PigGenericImage::createFileModel(const char *filename, int unit)
{
    return new PigFileModelGeneric(filename, unit, getMissionName());
}

////////////////////////////////////////////////////////////////////////
// Return the borders appropriate for the instrument type.
// This should only be called by PigFileModel!!
////////////////////////////////////////////////////////////////////////

int PigGenericImage::getBorders(PigFileModel *file, int &sl, int &ss,
					   int &el, int &es)
{
    sl = 0;
    ss = 0;
    el = file->getNL()-1;
    es = file->getNS()-1;
    return TRUE;
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

int PigGenericImage::canonicalizeFrameName(const char *frame,
		const char *inst_id,
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
        (strcasecmp(frame, "SFX") == 0) ||
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

    if ((strcasecmp(frame, "Camera") == 0) ||
        (strcasecmp(frame, "Natural") == 0) ||
        (strcasecmp(frame, "Spacecraft") == 0)) {

        canon_frame = "Generic Camera";
        short_frame = "Camera";
        max_indices = 2;
        if (mask_indices) {
            mask_indices[0] = 1;
            mask_indices[1] = 1;
        }
        return 1;
    }

    char msg[256];
    sprintf(msg,"Unknown Generic coordinate system frame: '%s', Ignored",frame);
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

const char *PigGenericImage::getRmcIndexName(int i)
{
    if (i == 0)  return "SITE";
    if (i == 1)  return "DRIVE";

    return "UNKNOWN";   // oops!
}

////////////////////////////////////////////////////////////////////////
// Get the PigRoverStateManager to use for this mission.  It is a singleton
// per mission.
////////////////////////////////////////////////////////////////////////

PigRoverStateManager *PigGenericImage::getRoverStateManager()
{
    if (_generic_rsm == NULL) {
        _generic_rsm = new PigRoverStateManagerGeneric(this);
        // Add "telemetry" as the lowest priority...
        _generic_rsm->addPriority("telemetry");
    }
    return _generic_rsm;
}

