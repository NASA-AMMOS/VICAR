////////////////////////////////////////////////////////////////////////
// PigM01
//
// Contains all mission-specific code for dealing with the Mars '01 testbed.
// This is not a real mission (any more); it is mainly a testbed for the
// Pancam camera.
////////////////////////////////////////////////////////////////////////

#include "PigM01.h"
#include "PigFileModel.h"
#include "PigCAHVOR.h"
#include "PigPointM01Pancam.h"

#include <string.h>
#include <stdlib.h>
#include <iostream>
#include <fstream>
using namespace std;
#include <stdio.h>

PigRoverStateManager *PigM01::_m01_rsm = NULL;

PigCoordSystem *PigM01::_m01_cs_db[PIG_MAX_CS];
int PigM01::_m01_cs_db_count = 0;
PigCoordSystem *PigM01::_m01_fixed_cs = NULL;

////////////////////////////////////////////////////////////////////////
// Create an M01 camera model given an image file.
////////////////////////////////////////////////////////////////////////

PigCameraModel *PigM01::createCameraModel(PigFileModel *file,
					  const char *special)
{
    PigCoordSystem *cs = getCoordSystem(file, NULL);

    // Get Flight vs. engineering	!!!!TBD

    // Get Filter number		!!!!TBD

    // Get Instrument ID

    //!!!! should be InstrumentId here !!!!
    const char *inst_id = file->getInstrumentName();
    if (strcasecmp(inst_id, "COLOR-PANCAM") != 0) {
        PigModelBase::printStaticMsg(
                   "Unrecognized InstrumentId, Pancam assumed", PigMsgWarning);
    }

    // Determine left vs. right

    const char *frame = file->getFrameId();
    if (frame != NULL && frame[0] == 'R') {
	return createCameraModel("Pancam Right", NULL, NULL, 
				 special, NULL, NULL, cs);
    }
    else {
	if (frame == NULL || frame[0] != 'L')
            PigModelBase::printStaticMsg(
                   "Unrecognized FrameId, Left eye assumed", PigMsgWarning);

	return createCameraModel("Pancam Left", NULL, NULL, 
				 special, NULL, NULL, cs);
    }

    return NULL;		// never reached

}

////////////////////////////////////////////////////////////////////////
// Create an M01 camera model given strings.  Valid strings are:
// instrument:  "Pancam Left", "Pancam Right"
// version: currently ignored
// subtype: currently ignored
// special: standard string (across all subclasses).  "stereo" will return
//	    the "stereo partner" of the model instead.
//
// All strings are case-insensitive.
//
// If the PigCoordSystem is NULL, the "natural" frame for the given
// mission/instrument, at the default site, are used.
////////////////////////////////////////////////////////////////////////

PigCameraModel *PigM01::createCameraModel(const char *instrument,
					  const char *version, 
					  const char *subtype, 
					  const char *special,
					  const char *construction,
					  const char *calibration,
					  PigCoordSystem *cs)
{
    PigCAHVOR *model;
    int stereo;
    int status;
    char msg[150];
    char cmod_path[500];
    char *cmod_file;

    if (cs == NULL)				// use default, natural frame
	cs = getCoordSystem(getNaturalFrame(instrument));
    else
	cs = getCoordSystem(cs, getNaturalFrame(instrument));

    // Check the "special" string

    stereo = 0;
    if (special != NULL && strlen(special) > 0) {
	if (strcasecmp(special, "stereo") == 0)
	    stereo = 1;
	else
	    return NULL;			// unknown special string
    }

    if (stereo) {			// swap instrument names
	if (strcasecmp(instrument, "Pancam Left") == 0)
	    instrument = "Pancam Right";
	else if (strcasecmp(instrument, "Pancam Right") == 0)
	    instrument = "Pancam Left";
	else
	    return NULL;		// no stereo or unknown
    }

    // All M01 cameras use CAHVOR, so we can create the subclass
    // now then fill in the parameters.

    model = new PigCAHVOR("M01", instrument, version, 
			  subtype, construction, calibration);
    model->setInitialCoordSystem(cs);

    // Check filter (subtype)		!!!!TBD

    if (strcasecmp(instrument, "Pancam Left") == 0) {
	// Check flight vs. engineering		!!!!TBD
	cmod_file = "camera_models/M01_Pancam_L.cahvor";
    }
    else if (strcasecmp(instrument, "Pancam Right") == 0) {
	// Check flight vs. engineering		!!!!TBD
	cmod_file = "camera_models/M01_Pancam_R.cahvor";
    }
    else {
	sprintf(msg, "Unknown M01 instrument %s", instrument);
	PigModelBase::printStaticMsg(msg, PigMsgFatal);
	return NULL;
    }

    status = 1;
    FILE *f = PigModelBase::openConfigFile(cmod_file, cmod_path);
    if (f != NULL) {
	fclose(f);
	status = model->readFromFile(cmod_path, cs);
    }
    if (f == NULL || status != 0) {		// file not present
	sprintf(msg,
	    "Unable to find calibration camera model file: %s (path=%s)",
	    cmod_file, cmod_path);
	PigModelBase::printStaticMsg(msg, PigMsgFatal);
	return NULL;
    }

    return model;

}

////////////////////////////////////////////////////////////////////////
// Create a M01 pointing model given an image file.
////////////////////////////////////////////////////////////////////////

PigPointingModel *PigM01::createPointingModel(PigCameraModel *cm,
					      PigFileModel *file,
					      const char *type,
					      bool allow_type_override)
{
    // Check instrument type	!!!!TBD

    const char *frame = file->getFrameId();
    if (frame != NULL && frame[0] == 'R') {
	return createPointingModel(cm, "Pancam Right", NULL, true);
    }
    else {
	if (frame == NULL || frame[0] != 'L')
            PigModelBase::printStaticMsg(
                   "Unrecognized FrameId, Left eye assumed", PigMsgWarning);

	return createPointingModel(cm, "Pancam Left", NULL, true);
    }

    return NULL;		// never reached
}

////////////////////////////////////////////////////////////////////////
// Create an M01 pointing model given strings.  Valid strings are:
// instrument:  "Pancam Left", "Pancam Right"
//
// All strings are case-insensitive.
////////////////////////////////////////////////////////////////////////

PigPointingModel *PigM01::createPointingModel(PigCameraModel *cm,
					      const char *instrument,
					      const char *type,
					      bool allow_type_override)
{
    // Check instrument		!!!!TBD

    return new PigPointM01Pancam(cm, this, instrument);
}

////////////////////////////////////////////////////////////////////////
// Create an M01 radiometry model given an image file.  Since we have
// no radiometry for M01, simply return NULL.
// !!!!TBD add radiometry model
////////////////////////////////////////////////////////////////////////

RadiometryModel *PigM01::createRadiometryModel(PigFileModel *file)
{
    return NULL;
}

////////////////////////////////////////////////////////////////////////
// Create an M01 FileModel.
////////////////////////////////////////////////////////////////////////

PigFileModel *PigM01::createFileModel(const char *filename, int unit)
{
    return new PigFileModel(filename, unit, getMissionName());
}

////////////////////////////////////////////////////////////////////////
// Return the borders appropriate for the instrument type.
// This should only be called by PigFileModel!!
////////////////////////////////////////////////////////////////////////

int PigM01::getBorders(PigFileModel *file, int &sl, int &ss,
					   int &el, int &es)
{
    // !!!!TBD set borders properly.  Probably depends on instrument.

    // Arbitrarily assign a 1-pixel unused border around the image (2 pixels
    // on top/bottom).  This was derived by visual inspection of the images.
    // We only need 1 pixel on the bottom, but since the images are rotated,
    // the 2-pixel border needs to apply on both ends.
    sl = 2;
    ss = 1;
    el = file->getNL() - 3;
    es = file->getNS() - 2;
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

int PigM01::canonicalizeFrameName(const char *frame, const char *inst_id,
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

    if ((strcasecmp(frame, "MLL") == 0) ||              // alias
        (strcasecmp(frame, "LL") == 0) ||
        (strcasecmp(frame, "Mars Local Level") == 0) ||
        (strcasecmp(frame, "Local Level") == 0) ||
        (strcasecmp(frame, "LOCAL_LEVEL_FRAME") == 0) ||
        (strcasecmp(frame, "Local_Level") == 0)) {      // alias

        canon_frame = "LOCAL LEVEL";
        short_frame = "LL";
        max_indices = 2;
        if (mask_indices) {
            mask_indices[0] = 1;
            mask_indices[1] = 1;
        }
        return 1;
    }

    if ((strcasecmp(frame, "Rover") == 0) ||
        (strcasecmp(frame, "Camera") == 0) ||
        (strcasecmp(frame, "Spacecraft") == 0)) {

        canon_frame = "M01 Camera";
        short_frame = "Camera";
        max_indices = 2;
        if (mask_indices) {
            mask_indices[0] = 1;
            mask_indices[1] = 1;
        }
        return 1;
    }

    char msg[256];
    sprintf(msg, "Unknown M01 coordinate system frame: '%s', Ignored", frame);
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

const char *PigM01::getRmcIndexName(int i)
{
    if (i == 0)  return "SITE";
    if (i == 1)  return "DRIVE";

    return "UNKNOWN";   // oops!
}

////////////////////////////////////////////////////////////////////////
// Get the PigRoverStateManager to use for this mission.  It is a singleton
// per mission.
////////////////////////////////////////////////////////////////////////

PigRoverStateManager *PigM01::getRoverStateManager()
{
    if (_m01_rsm == NULL) {
        _m01_rsm = new PigRoverStateManager(this);
        // Add "telemetry" as the lowest priority...
        _m01_rsm->addPriority("telemetry");
    }
    return _m01_rsm;
}


