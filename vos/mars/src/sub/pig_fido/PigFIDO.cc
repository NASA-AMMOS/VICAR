////////////////////////////////////////////////////////////////////////
// PigFIDO
//
// Contains all mission-specific code for dealing 
// with the Mars FIDO testbed.
// This is not a real mission ; it is only a testbed.
// 
////////////////////////////////////////////////////////////////////////

#include "PigFIDO.h"
#include "PigFileModel.h"
#include "PigFileModelFIDO.h"
#include "PigCAHVOR.h"
#include "PigPointFIDOMastArmCamera.h"
#include "PigPointFIDOMastArmCamera2DOF.h"
#include "PigPointFIDOBodyFixedCamera.h"
#include "RadiometryFIDO.h"
#include "PigRoverStateManagerFIDO.h"

#include <string.h>
#include <stdlib.h>
#include <iostream>
#include <fstream>
using namespace std;
#include <stdio.h>

PigRoverStateManager *PigFIDO::_fido_rsm = NULL;

PigCoordSystem *PigFIDO::_fido_cs_db[PIG_MAX_CS];
int PigFIDO::_fido_cs_db_count = 0;
PigCoordSystem *PigFIDO::_fido_fixed_cs = NULL;

////////////////////////////////////////////////////////////////////////
// Create a FIDO camera model given an image file.
////////////////////////////////////////////////////////////////////////

PigCameraModel *PigFIDO::createCameraModel(PigFileModel *file,
					   const char *special)
{
    PigCoordSystem *cs = getCoordSystem(file, NULL);

    // Get Instrument ID

    //!!!! should be InstrumentId here !!!!
    const char *inst_id = file->getInstrumentId();

    if (strcasecmp(inst_id, "PAN") == 0) {
      // Determine left vs. right
      const char *frame = file->getFrameId();
      if (frame != NULL && frame[0] == 'R') {
	return createCameraModel("Pancam Right", // instrument
				 NULL,           // version
				 NULL,           // subtype
				 special,        // special
				 NULL,           // construction
				 NULL,           // calibration
				 cs);            // coord_sys
      }
      else {
	if (frame == NULL || frame[0] != 'L')
	  PigModelBase::printStaticMsg(
                   "Unrecognized FrameId, Left eye assumed", PigMsgWarning);

	return createCameraModel("Pancam Left", NULL, NULL, 
				 special, NULL, NULL, cs);
      }

    } 
    else if(strcasecmp(inst_id, "NAV") == 0) {
      // Determine left vs. right
      const char *frame = file->getFrameId();
      if (frame != NULL && frame[0] == 'R') {
	return createCameraModel("Navcam Right", NULL, NULL, 
				 special, NULL, NULL, cs);
      }
      else {
	if (frame == NULL || frame[0] != 'L')
	  PigModelBase::printStaticMsg(
                   "Unrecognized FrameId, Left eye assumed", PigMsgWarning);

	return createCameraModel("Navcam Left", NULL, NULL, 
				 special, NULL, NULL, cs);
      }
    }

    else if(strcasecmp(inst_id, "FHZ") == 0) {
      // Determine left vs. right
      const char *frame = file->getFrameId();
      if (frame != NULL && frame[0] == 'R') {
	return createCameraModel("Hazcam Front Right", NULL, NULL, 
				 special, NULL, NULL, cs);
      }
      else {
	if (frame == NULL || frame[0] != 'L')
	  PigModelBase::printStaticMsg(
                   "Unrecognized FrameId, Left eye assumed", PigMsgWarning);

	return createCameraModel("Hazcam Front Left", 
				 NULL, NULL, special, NULL, NULL, cs);
      }
    }

    else if(strcasecmp(inst_id, "RHZ") == 0) {
      // Determine left vs. right
      const char *frame = file->getFrameId();
      if (frame != NULL && frame[0] == 'R') {
	return createCameraModel("Hazcam Rear Right", 
				 NULL, NULL, special, NULL, NULL, cs);
      }
      else {
	if (frame == NULL || frame[0] != 'L')
	  PigModelBase::printStaticMsg(
                   "Unrecognized FrameId, Left eye assumed", PigMsgWarning);

	return createCameraModel("Hazcam Rear Left", 
				 NULL, NULL, special, NULL, NULL, cs);
      }
    }

    else if(strcasecmp(inst_id, "BCM") == 0) {
      // Determine left vs. right
      const char *frame = file->getFrameId();
      if (frame != NULL && frame[0] == 'R') {
	return createCameraModel("Bellycam Right", 
				 NULL, NULL, special, NULL, NULL, cs);
      }
      else {
	if (frame == NULL || frame[0] != 'L')
	  PigModelBase::printStaticMsg(
                   "Unrecognized FrameId, Left eye assumed", PigMsgWarning);

	return createCameraModel("Bellycam Left", 
				 NULL, NULL, special, NULL, NULL, cs);
      }
    }
    else {
      PigModelBase::printStaticMsg(
                   "Unrecognized InstrumentId, Pancam assumed", PigMsgWarning);
	return createCameraModel("Pancam Left", NULL, NULL, 
				 special, NULL, NULL, cs);
    }
      
    return NULL;		// never reached

}

////////////////////////////////////////////////////////////////////////
// Create a FIDO camera model given strings.  Valid strings are:
// instrument:  "Pancam Left", "Pancam Right", Navcam Left, Navcam Right
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

PigCameraModel *PigFIDO::createCameraModel(const char *instrument,
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

    if (cs == NULL)		               // use default, natural frame
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
	if (strcasecmp(instrument, "Pancam Left") == 0)
	    instrument = "Pancam Right";
	else if (strcasecmp(instrument, "Pancam Right") == 0)
	    instrument = "Pancam Left";

	else if (strcasecmp(instrument, "Navcam Left") == 0)
	    instrument = "Navcam Right";
	else if (strcasecmp(instrument, "Navcam Right") == 0)
	    instrument = "Navcam Left";

	else if (strcasecmp(instrument, "Hazcam Front Left") == 0)
	    instrument = "Hazcam Front Right";
	else if (strcasecmp(instrument, "Hazcam Front Right") == 0)
	    instrument = "Hazcam Front Left";

	else if (strcasecmp(instrument, "Hazcam Rear Left") == 0)
	    instrument = "Hazcam Rear Right";
	else if (strcasecmp(instrument, "Hazcam Rear Right") == 0)
	    instrument = "Hazcam Rear Left";

	else if (strcasecmp(instrument, "Bellycam Left") == 0)
	    instrument = "Bellycam Right";
	else if (strcasecmp(instrument, "Bellycam Right") == 0)
	    instrument = "Bellycam Left";

	else
	    return NULL;		// no stereo or unknown
    }

    // All FIDO cameras use CAHVOR, so we can create the subclass
    // now then fill in the parameters.

    model = new PigCAHVOR(getMissionName(), instrument, version, 
			  subtype, construction, calibration);
    model->setInitialCoordSystem(cs);


    if (strcasecmp(instrument, "Pancam Left") == 0) {
	cmod_file = "camera_models/FIDO_leftSciHi.cahvor";
    }
    else if (strcasecmp(instrument, "Pancam Right") == 0) {
	cmod_file = "camera_models/FIDO_rightSciHi.cahvor";
    }
    else if (strcasecmp(instrument, "Navcam Right") == 0) {
	cmod_file = "camera_models/FIDO_rightNavHi.cahvor";
    }
    else if (strcasecmp(instrument, "Navcam Left") == 0) {
	cmod_file = "camera_models/FIDO_leftNavHi.cahvor";
    }
    else if(strcasecmp(instrument, "Hazcam Front Left") == 0) {
      cmod_file = "camera_models/FIDO_leftFrontHaz.cahvor";
    }
    else if(strcasecmp(instrument, "Hazcam Front Right") == 0) {
      cmod_file = "camera_models/FIDO_rightFrontHaz.cahvor";
    }
    else if(strcasecmp(instrument, "Hazcam Rear Left") == 0) {
      cmod_file = "camera_models/FIDO_leftRearHaz.cahvor";
    }
    else if(strcasecmp(instrument, "Hazcam Rear Right") == 0) {
      cmod_file = "camera_models/FIDO_rightRearHaz.cahvor";
    }
    else if(strcasecmp(instrument, "Bellycam Left") == 0) {
      cmod_file = "camera_models/FIDO_leftBellyCam.cahvor";
    }
    else if(strcasecmp(instrument, "Bellycam Right") == 0) {
      cmod_file = "camera_models/FIDO_rightBellyCam.cahvor";
    }
    else {
	sprintf(msg, "Unknown FIDO instrument %s", instrument);
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
// Create a FIDO pointing model given an image file.
////////////////////////////////////////////////////////////////////////

PigPointingModel *PigFIDO::createPointingModel(PigCameraModel *cm,
					       PigFileModel *file,
					       const char *type,
					       bool allow_type_override)
{
   //!!!! should be InstrumentId here !!!!
    const char *inst_id = file->getInstrumentId();
    const char *frame = file->getFrameId();

    // Check instrument type
    if (strcasecmp(inst_id, "PAN") == 0) {
      if (frame != NULL && frame[0] == 'R') {
	return createPointingModel(cm, "Pancam Right", NULL, true);
      }
      else {
	if (frame == NULL || frame[0] != 'L')
	  PigModelBase::printStaticMsg(
                   "Unrecognized FrameId, Left eye assumed", PigMsgWarning);

	return createPointingModel(cm, "Pancam Left", NULL, true);
      }
    }
    else if (strcasecmp(inst_id, "NAV") == 0) {
      if (frame != NULL && frame[0] == 'R') {
	return createPointingModel(cm, "Navcam Right", NULL, true);
      }
      else {
	if (frame == NULL || frame[0] != 'L')
	  PigModelBase::printStaticMsg(
                   "Unrecognized FrameId, Left eye assumed", PigMsgWarning);

	return createPointingModel(cm, "Navcam Left", NULL, true);
      }
    }
    if (strcasecmp(inst_id, "FHZ") == 0) {
      if (frame != NULL && frame[0] == 'R') {
	return createPointingModel(cm, "Hazcam Front Right", NULL, true);
      }
      else {
	if (frame == NULL || frame[0] != 'L')
	  PigModelBase::printStaticMsg(
                   "Unrecognized FrameId, Left eye assumed", PigMsgWarning);

	return createPointingModel(cm, "Hazcam Front Left", NULL, true);
      }
    }
    else if (strcasecmp(inst_id, "RHZ") == 0) {
      if (frame != NULL && frame[0] == 'R') {
	return createPointingModel(cm, "Hazcam Rear Right", NULL, true);
      }
      else {
	if (frame == NULL || frame[0] != 'L')
	  PigModelBase::printStaticMsg(
                   "Unrecognized FrameId, Left eye assumed", PigMsgWarning);

	return createPointingModel(cm, "Hazcam Rear Left", NULL, true);
      }
    }
    else if (strcasecmp(inst_id, "BCM") == 0) {
      if (frame != NULL && frame[0] == 'R') {
	return createPointingModel(cm, "Bellycam Right", NULL, true);
      }
      else {
	if (frame == NULL || frame[0] != 'L')
	  PigModelBase::printStaticMsg(
                   "Unrecognized FrameId, Left eye assumed", PigMsgWarning);

	return createPointingModel(cm, "Bellycam Left", NULL, true);
      }
    }
    else {
      PigModelBase::printStaticMsg(
                   "Unrecognized InstrumentId, Pancam Left assumed", 
		   PigMsgWarning);
	return createPointingModel(cm, "Pancam Left", NULL, true);
    }

    return NULL;		// never reached
}

////////////////////////////////////////////////////////////////////////
// Create an FIDO pointing model given strings.  Valid strings are:
// instrument:  "Pancam Left", "Pancam Right", "Navcam Left", 
// "Navcam Right", "Hazcam Front Left", "Hazcam Front Right", 
// "Hazcam Rear Left", "Hazcam Rear Right", "Bellycam Left", 
//  "Bellycam Right"
//
// All strings are case-insensitive.
////////////////////////////////////////////////////////////////////////

PigPointingModel *PigFIDO::createPointingModel(PigCameraModel *cm,
					       const char *instrument,
					       const char *type,
					       bool allow_type_override)
{
     /////////////////
    PigPointingModel *model;
    char msg[150];

    if (instrument == NULL)
	return NULL;

    if (strncasecmp(instrument, "Nav", 3) == 0 ||
	strncasecmp(instrument, "Pan", 3) == 0) {

      //determine which pointing model to use
      char point_method[256];
      int count;

      getParam("POINT_METHOD", point_method, &count, 1, 0);

      if (count == 1) {
	char *value = parseParamString(point_method, "FIDO");
	if (value != NULL && 
	    ((strncasecmp(value, "4DOF", 4) == 0) ||
	     (strncasecmp(value, "Joints", 6) == 0)) )
	  model = new PigPointFIDOMastArmCamera(cm, this, instrument);
	else
	  model = new PigPointFIDOMastArmCamera2DOF(cm, this, instrument);
      }
      else
	model = new PigPointFIDOMastArmCamera2DOF(cm, this, instrument);
	
    }
    else if ((strncasecmp(instrument, "Haz", 3) == 0) ||
	     strncasecmp(instrument, "Belly", 5) == 0) {
	model = new PigPointFIDOBodyFixedCamera(cm, this, instrument);
    }
    
    else {
	sprintf(msg, "Unknown FIDO instrument %s", instrument);
	PigModelBase::printStaticMsg(msg, PigMsgFatal);
	return NULL;
    }

    return model;
}

////////////////////////////////////////////////////////////////////////
// Create a FIDO radiometry model given an image file.
////////////////////////////////////////////////////////////////////////

RadiometryModel *PigFIDO::createRadiometryModel(PigFileModel *file)
{
  return RadiometryFIDO::create(file);
}

////////////////////////////////////////////////////////////////////////
// Create a FIDO FileModel.
////////////////////////////////////////////////////////////////////////

PigFileModel *PigFIDO::createFileModel(const char *filename, int unit)
{
    return new PigFileModelFIDO(filename, unit, getMissionName());
}

////////////////////////////////////////////////////////////////////////
// Return the borders appropriate for the instrument type.
// This should only be called by PigFileModel!!
////////////////////////////////////////////////////////////////////////

int PigFIDO::getBorders(PigFileModel *file, int &sl, int &ss,
					   int &el, int &es)
{
    // By visual inspection of the FIDO images, it appears 
    // borders are pretty constant for all cameras.
    sl = 1;
    ss = 12;
    el = file->getNL() - 1;
    es = file->getNS() - 9;
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

int PigFIDO::canonicalizeFrameName(const char *frame, const char *inst_id,
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
        (strcasecmp(frame, "Fixed_Frame") == 0) ||
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
        (strcasecmp(frame, "Rover_Frame") == 0) ||
        (strcasecmp(frame, "Rover Mechanical") == 0)) {

        canon_frame = "ROVER";
        short_frame = "ROVER";
        max_indices = 2;
        if (mask_indices) {
            mask_indices[0] = 1;
            mask_indices[1] = 1;
        }
        return 1;
    }

    char msg[256];
    sprintf(msg, "Unknown FIDO coordinate system frame: '%s', Ignored", frame);
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

const char *PigFIDO::getRmcIndexName(int i)
{
    if (i == 0)  return "SITE";
    if (i == 1)  return "DRIVE";

    return "UNKNOWN";   // oops!
}

////////////////////////////////////////////////////////////////////////
// Get the PigRoverStateManager to use for this mission.  It is a singleton
// per mission.
////////////////////////////////////////////////////////////////////////

PigRoverStateManager *PigFIDO::getRoverStateManager()
{
    if (_fido_rsm == NULL) {
	_fido_rsm = new PigRoverStateManagerFIDO(this);
	// Add "telemetry" as the lowest priority...
	_fido_rsm->addPriority("telemetry");
    }
    return _fido_rsm;
}

