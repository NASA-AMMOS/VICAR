//////////////////////////////////////////////////////////////////////////
// PigMSL
//
// Contains all mission-specific code for dealing 
// with the Mars MSL mission.
// 
////////////////////////////////////////////////////////////////////////

#include <stdlib.h>

#include "PigMSL.h"
#include "PigLabelModelMSL.h"
#include "PigFileModelMSL.h"
#include "PigCAHVOR.h"
#include "PigCAHVORE.h"
#include "PigPointGenericCamera_0DOF.h"
#include "PigPointGenericCamera_AzEl.h"
#include "PigPointGenericCamera_7dof.h"
#include "PigPointMSLmastCamera.h"
#include "PigPointMSLmastCamera3dof.h"
#include "PigPointMSLmastCameraScale.h"
#include "PigPointMSLBodyFixedCamera.h"
#include "PigPointMSLmahliCamera.h"
#include "PigPointMSLmahliCamera6dof.h"
#include "PigRoverStateManagerMSL.h"
#include "RadiometryMSL.h"
#include "PigCSReference.h"
#include "PigCSDefinition.h"

#include "PigXerces.h"

PigRoverStateManager *PigMSL::_msl_rsm = NULL;

PigCoordSystem *PigMSL::_msl_cs_db[PIG_MAX_CS];
int PigMSL::_msl_cs_db_count = 0;
PigCoordSystem *PigMSL::_msl_fixed_cs = NULL;

// Constant coordinate frame offsets

// Keep in sync with msl_mast.point config file
#define MSL_RMECH_TO_RNAV_OFFSET_X 0.09002
#define MSL_RMECH_TO_RNAV_OFFSET_Y 0.0
#define MSL_RMECH_TO_RNAV_OFFSET_Z -1.1205

////////////////////////////////////////////////////////////////////////
// The INSTRUMENT_HOST_ID label contains "MSL" or "SIM".  Because "SIM"
// is not very unique, we internally change the host ID to "MSLSIM".
// This value is what is used to find config files.  Just in case the
// host is already MSLSIM, we prepend MSL only if the value does not already
// start with MSL.  We save the unadorned value for use in comparing against
// the list of existing PigMSL objects in PigMission::getMissionObject().
// That should be the ONLY user of the "orig" host ID.
////////////////////////////////////////////////////////////////////////
PigMSL::PigMSL(const char *mission_name, const char *host_id)
{
    strcpy(_mission_name, mission_name);
    if (strncmp(host_id, "MSL", 3) == 0) {
        strcpy(_orig_host_id, host_id);
        strcpy(_host_id, host_id);
    } else {
	strcpy(_orig_host_id, host_id);
	strcpy(_host_id, "MSL");
	strcat(_host_id, host_id);
    }
}
////////////////////////////////////////////////////////////////////////
// Create a MSL camera model given an image file.
////////////////////////////////////////////////////////////////////////

PigCameraModel *PigMSL::createCameraModel(PigFileModel *file,
                                          const char *special)
{
    int count;
    char *cm_name = NULL;
    char point_method[256];
    char geom_proj_type[11];
    PigCoordSystem *cs = getCoordSystem(file, NULL);
    PigCameraModel *model = NULL;

    // If there's no geometry projection type then it's not a normally-labeled
    // file... so there's no camera model to create.
    // TBD: create a mosaic camera model if that's what it is... !!!!

    const char *p = file->getGeometryProjectionType();
    if (p == NULL)
        return NULL;
    strcpy(geom_proj_type, p);

    int dx = file->getFirstLineSample(1) - 1;
    int dy = file->getFirstLine(1) - 1;
    int hscale = (int)(file->getDownsampleXFactor(1.0));
    int vscale = (int)(file->getDownsampleYFactor(1.0));


    // Get Instrument ID
    const char *inst_id = file->getInstrumentId();

    // make sure that frame id is valid given inst_id name
    const char *frame = file->getFrameId();
    bool match = TRUE;

    if(strstr(inst_id, "RIGHT") != NULL) {
        if(frame != NULL && frame[0] != 'R')
            match = FALSE;
    }
    else if(strstr(inst_id, "LEFT") != NULL) {
        if(frame != NULL && frame[0] != 'L')
                        match = FALSE;
    }
        
    if(!match)
        PigModelBase::printStaticMsg(
        "WARNING: FrameId does not match InstrumentId name or Unrecognized FrameId!", PigMsgWarning);

    // Put subframe and downsampling info into the 
    // construction string
    char sub_down_str[150];
    sprintf(sub_down_str, "kinematics type=%s subframe=(%d,%d) downsampling=(%d,%d)", 
	    geom_proj_type, dx, dy, hscale, vscale);

    // Combine filter, focus, and temperature for the subtype string
    char subtype[255];
    const char *filt = file->getFilterNumber();
    double our_temp = file->getInstrumentTemperature(0);
    double partner_temp = file->getInstrumentTemperature(0);
    if (strcmp(file->getModelName(), "FileModelMSL") == 0) {
	our_temp =
		((PigFileModelMSL *)file)->getInstrumentTemperature(0, false);
	partner_temp =
		((PigFileModelMSL *)file)->getInstrumentTemperature(0, true);
    }

    sprintf(subtype, "filter=%s focus=%d zoom=%d temp=%f partner_temp=%f\n",
			(filt == NULL) ? "NULL" : filt,
			file->getInstrumentFocusPosition(0),
			0,		// no zoom
			our_temp,
			partner_temp);

    // Get the camera model name to use
    getParam("POINT_METHOD", point_method, &count, 1, 0);

    char *pointMethodValue = NULL;
    if (count == 1) {   
        pointMethodValue = parseParamString(point_method, "cm");
        if(pointMethodValue != NULL && 
           (strncasecmp(pointMethodValue, "label",5) == 0)) {
            
            cm_name = parseParamString(point_method, "CM_NAME");
            
            // Look for labels
            
            file->openFile();                       // just to make sure
                        
            // Named labels
                        
            if (model == NULL) {
                if (cm_name != NULL)
                    model = PigCameraModel::createFromLabel(file, // file model
                                 cm_name,                       // instance
                                 getMissionName(),              // mission 
                                 inst_id,                       // instrument 
                                 NULL,                          // version
                                 subtype,		// filter+focus+temp
                                 "label",                       // construction
                                 file->getCalibrationSourceId(),// calibration
                                 cs);                           // coord_sys 
            }
                        
            // Instance 1
                        
            if (model == NULL) {
                model = PigCameraModel::createFromLabel(file,   // file model
                                1,                              // instance
                                getMissionName(),               // mission 
                                inst_id,                        // instrument 
                                NULL,                           // version
                                subtype,		// filter+focus+temp
                                "label",                        // construction
			        file->getCalibrationSourceId(), // calibration
                                cs);                            // coord_sys 
            }

            if (model != NULL)
                return model;
        }
        else if(pointMethodValue != NULL && 
                (strncasecmp(pointMethodValue, "kinematics", 10) == 0)) {
            model =  createCameraModel(inst_id,                 // instrument
                                NULL,                           // version
                                subtype,		// filter+focus+temp
                                special,                        // special
                                sub_down_str,                   // construction
				file->getCalibrationSourceId(), // calibration
				cs);                            // coord_sys
        }
    }
    
    //If point_method's cm  parameter is not specified, or the value is not 
    //recognized as a valid input, assume "kinematics" subtype as a default.
    if (!model)
        model = createCameraModel(inst_id,                      // instrument
                                NULL,                           // version
                                subtype,		// filter+focus+temp
                                special,                        // special
                                sub_down_str,                   // construction
				file->getCalibrationSourceId(), // calibration
				cs);                            // coord_sys
        
    return model;
}

////////////////////////////////////////////////////////////////////////
// Given strings constructs full-path filename
// Note that calibration currently is not used to construct filename.
// Returns cmod_file char string.
//
// If the camera model needs to be interpolated for this type of camera
// (does_interp is true), then instead of returning the actual camera model
// filename, this returns the filename of the interpolation file, which lists
// all the actual camera models.
//
// use_filter can be used to turn off the filter without affecting the
// interpolation (i.e. interpolate a non-filtered model).  use_filter=true
// does not mean the camera has a filter, it just means to check the filter
// field in subtype for one.
////////////////////////////////////////////////////////////////////////
void PigMSL::constructCameraModelFileName(const char *calibration,
					  const char *serial,
					  const char *subtype,
					  const char *type,
					  int interp_focus,
					  int interp_zoom,
					  int interp_temp,
					  int interp_partner_temp,
					  int use_filt,
					  char *cmod_file)
// interp_zoom is unused
{    
    strcpy(cmod_file, "");
    strcat(cmod_file, "camera_models/MSL");
    
    if (serial) {
        strcat(cmod_file, "_SN_");
        strcat(cmod_file, serial);
    }

    char filt[100];
    char *filter = NULL;
    double focus = 0;	// read as float for maximum flexibility
    double zoom = 0;	// not used
    double temperature = 0;
    double partner_temp = 0;
    if (subtype) {
        sscanf(subtype,"filter=%s focus=%lf zoom=%lf temp=%lf partner_temp=%lf",
			filt, &focus, &zoom, &temperature, &partner_temp);
	if (strcasecmp(filt, "NULL") != 0)
	    filter = filt;
    }

    if (filter && use_filt) {
        strcat(cmod_file, "_F_");
        strcat(cmod_file, filter);
    }

    if (interp_focus || interp_temp) {
	strcat(cmod_file, ".interp");
	if (interp_focus)
	    strcat(cmod_file, "f");
	if (interp_temp)
	    strcat(cmod_file, "t");
    }
    else if (type) {
        strcat(cmod_file, ".");
        strcat(cmod_file, type);
    }
}


//////////////////////////////////////////////////////////////////////
// Creates camera model given PigCameraMapEntry and strings 
// the value of which can't be derived from PigCameraMapEntry instance.
// This version does NOT interpolate!
//////////////////////////////////////////////////////////////////////
PigCameraModel *PigMSL::createCameraModel(PigCameraMapEntry *entry,
					  const char *subtype,
					  const char *construction,
					  const char *calibration)
{
    // Mastcam, Navcam and MAHLI MSL cameras use CAHVOR by default, 
    // while 4 Hazcams use CAHVORE camera model. 
    // We create an instance now then fill in the parameters.

    char msg[1024];
    char type[7];

    if (!entry)
      return NULL;

    strcpy(type, entry->getType());

    if(strncasecmp(type, "CAHVORE", 7) == 0)
      return new PigCAHVORE(getMissionName(), 
			     entry->getID(), entry->getSerialNumber(), 
			     subtype, construction, calibration);
    else if(strncasecmp(type, "CAHVOR", 6) == 0)
      return new PigCAHVOR(getMissionName(), 
			     entry->getID(), entry->getSerialNumber(), 
			     subtype, construction, calibration);
    else if(strncasecmp(type, "CAHV", 4) == 0)
      return new PigCAHV(getMissionName(), 
			     entry->getID(), entry->getSerialNumber(), 
			     subtype, construction, calibration);
    else {
        PigModelBase::printStaticMsg("Camera Model does not match any known type, revert to default type for known instruments", 
				     PigMsgWarning);

//!!!! CHEMCAM!!!!
	// Get the default type for known instruments
	if((strncasecmp(entry->getShortID(), "ML", 2) == 0) ||
	   (strncasecmp(entry->getShortID(), "MR", 2) == 0) ||
	   (strncasecmp(entry->getShortID(), "MH", 2) == 0) ||
	   (strncasecmp(entry->getShortID(), "NL", 2) == 0) ||
	   (strncasecmp(entry->getShortID(), "NR", 2) == 0)) {
	    return new PigCAHVOR(getMissionName(), 
				 entry->getID(), entry->getSerialNumber(), 
				 subtype, construction, calibration);
	}
	else if((strncasecmp(entry->getShortID(), "FL", 1) == 0) ||
	        (strncasecmp(entry->getShortID(), "FR", 1) == 0) ||
	        (strncasecmp(entry->getShortID(), "RL", 1) == 0) ||
	        (strncasecmp(entry->getShortID(), "RR", 1) == 0)) {
	    return new PigCAHVORE(getMissionName(), 
				  entry->getID(), entry->getSerialNumber(), 
				  subtype, construction, calibration);
	}
	else {
	  sprintf(msg, "Unknown MSL instrument %s", entry->getShortID());
	  PigModelBase::printStaticMsg(msg, PigMsgFatal);
	  return NULL;
	}
    }
}

////////////////////////////////////////////////////////////////////////
// Create an MSL pointing model given strings.  Valid strings are:
// instrument:  "Mastcam Left/Right", "Navcam Left/Right", "Hazcam Front/Rear 
// Left/Right", "MAHLI" (Microscopic imager), "MARDI" (descent), "CHEMCAM_RMI".
// All strings are case-insensitive.
//
// User-specified pointing method is stored in CameraModel's subtype member
// variable.  The following pointing methods supported for MSL:
//
// CM=KINEMATICS (default) 
//   Uses dynamically derived camera models. 
//   For mast based instruments use physical pointing models(based on joint 
//   angles), for arm use 6- or 7-dof, for rover mounted instruments use body
//   fixed model.
// CM=LABEL 
//   Uses camera models from the label.  For mast based instruments
//   use 2DOF AzEl GenericCamera pointing model.  Body Fixed cameras will use 
//   0DOF GenericCamera pointing model.  Arm cameras use generic 6DOF.
//
// PM=xxx
//   Overrides the pointing model in an instrument-specific way.
// PM=2DOF
//   Mast-mounted cameras only.  Uses the 2-DOF az/el MSL-specific model.
// PM=3DOF
//   Mast-mounted cameras only.  Uses the 2-DOF az/el/twist MSL-specific model.
// PM=6DOF
//   MAHLI only.  Uses 6-DOF (euler angle plus position) MSL model.
//!!!! Should this be allowed for mast too?  Probably so...
// PM=7DOF
//   MAHLI only.  Used 7-DOF (quaternion plus position) MSL model.
//!!!! Should this be allowed for mast too?  Probably so...
// 
////////////////////////////////////////////////////////////////////////

PigPointingModel *PigMSL::createPointingModel(PigCameraModel *cm,
			const char *instrument, const char *type,
			bool allow_type_override)
{
    int count;
    PigPointingModel *model;
    char msg[1024];
    char point_method[256];
    char *pointMethodValue = NULL;
    char model_type[30];

    if (type != NULL)
        strcpy(model_type, type);
    else
        strcpy(model_type, "");


    if (instrument == NULL)
        return NULL;

    // Get the pointing model name to use
    getParam("POINT_METHOD", point_method, &count, 1, 0);

    if (count == 1)
        pointMethodValue = parseParamString(point_method, "pm");
    
///////////////////////////
// MAST CAMERAS
///////////////////////////

    if ((strncasecmp(instrument, "MAST", 4) == 0) ||
        (strncasecmp(instrument, "NAV", 3) == 0) ||
	(strncasecmp(instrument, "CHEMCAM_RMI", 11) == 0)) {

        if (cm->getCameraConstruction() != NULL &&
	    ((strncasecmp(cm->getCameraConstruction(), "label", 5) == 0)))
            strcpy(model_type, "GenericCamera_AzEl");

        else if (allow_type_override && pointMethodValue != NULL) {
            if (strncasecmp(pointMethodValue, "2dof", 4) == 0) {
                if (type != NULL && strcasecmp(type, "MSLmastCamera") != 0)
                    PigModelBase::printUniqueStaticMsg(
"WARNING: Pointing Model overridden to MSLmastCamera via command line",
                                                 PigMsgWarning);

                strcpy(model_type, "MSLmastCamera");
            }
            else if ((strncasecmp(pointMethodValue, "3dof", 4) == 0)) {
                if (type != NULL && strcasecmp(type, "MSLmastCamera3dof") != 0)
                    PigModelBase::printUniqueStaticMsg(
"WARNING: Pointing Model overridden to MSLmastCamera3dof via command line",
                                                 PigMsgWarning);
                strcpy(model_type, "MSLmastCamera3dof");
            }
            else if ((strncasecmp(pointMethodValue, "scale", 4) == 0)) {
                if (type != NULL && strcasecmp(type, "MSLmastCameraScale") != 0)
                    PigModelBase::printUniqueStaticMsg(
"WARNING: Pointing Model overridden to MSLmastCameraScale via command line",
                                                 PigMsgWarning);
                strcpy(model_type, "MSLmastCameraScale");
            }
        }

	// We've figured out the model type, now create it

        if (strcasecmp(model_type, "GenericCamera_AzEl") == 0)
            return new PigPointGenericCamera_AzEl(cm, this, NULL);

        // Az/El + twist pointing model
        else if (strcasecmp(model_type, "MSLmastCamera3dof") == 0)
            return new PigPointMSLmastCamera3dof(cm,
					this, instrument);

        // Az/El + twist + scale pointing model
        else if (strcasecmp(model_type, "MSLmastCameraScale") == 0)
            return new PigPointMSLmastCameraScale(cm,
					this, instrument);
        else
            // Default 2dof(Az/El) pointing model
            return new PigPointMSLmastCamera(cm, this, instrument);

    }

///////////////////////////
// MAHLI.  7dof (quat + pos) is the default, can select 6dof (pos + angles)
///////////////////////////

    else if (strncasecmp(instrument, "MAHLI", 5) == 0) {

        if (cm->getCameraConstruction() != NULL &&
	    ((strncasecmp(cm->getCameraConstruction(), "label", 5) == 0)))
            strcpy(model_type, "GenericCamera_7dof");
        else if (allow_type_override && pointMethodValue != NULL) {
            if (strncasecmp(pointMethodValue, "6dof", 4) == 0) {
                if (type != NULL && strcasecmp(type, "MSLmahliCamera6dof") != 0)
                    PigModelBase::printUniqueStaticMsg(
"WARNING: Pointing Model overridden to MSLmahliCamera6dof via command line",
                                                 PigMsgWarning);
                strcpy(model_type, "MSLmahliCamera6dof");
            }
            else if ((strncasecmp(pointMethodValue, "7dof", 4) == 0)) {
                if (type != NULL && strcasecmp(type, "MSLmahliCamera") != 0)
                    PigModelBase::printUniqueStaticMsg(
"WARNING: Pointing Model overridden to MSLmahliCamera via command line",
                                                 PigMsgWarning);
                strcpy(model_type, "MSLmahliCamera7dof");
            }

	}

	// We've figured out the model type, now create it

        if (strcasecmp(model_type, "GenericCamera_7dof") == 0)
            return new PigPointGenericCamera_7dof(cm, this, NULL);

        else if (strcasecmp(model_type, "MSLmahliCamera6dof") == 0)
            return new PigPointMSLmahliCamera6dof(cm, this, instrument);
        else
            // Default 7dof pointing model
            return new PigPointMSLmahliCamera(cm, this, instrument);

    }

///////////////////////////
// BODY-MOUNTED CAMERAS
///////////////////////////

    else if ((strstr(instrument, "HAZ") != NULL) ||
	     (strncasecmp(instrument, "MARDI",5) == 0)) {
      if(cm->getCameraConstruction() != NULL &&
         (strncasecmp(cm->getCameraConstruction(), "label", 5) == 0))
          return new PigPointGenericCamera_0DOF(cm, this, NULL);
      else
          return new PigPointMSLBodyFixedCamera(cm, this, 
                                                 instrument);
    }

    else {
        sprintf(msg, "Unknown MSL instrument %s", instrument);
        PigModelBase::printStaticMsg(msg, PigMsgFatal);
        return NULL;
    }
    // shouldn't be here...
    return NULL;
}

////////////////////////////////////////////////////////////////////////
// Create a MSL radiometry model given an image file.
////////////////////////////////////////////////////////////////////////

RadiometryModel *PigMSL::createRadiometryModel(PigFileModel *file)
{
    return RadiometryMSL::create(file);
}

////////////////////////////////////////////////////////////////////////
// Create a MSL LabelModel.
////////////////////////////////////////////////////////////////////////

PigLabelModel *PigMSL::createLabelModel(int unit)
{
    return new PigLabelModelMSL(unit, getMissionName());
}

////////////////////////////////////////////////////////////////////////
// Create a MSL FileModel.
////////////////////////////////////////////////////////////////////////

PigFileModel *PigMSL::createFileModel(const char *filename, int unit)
{
    return new PigFileModelMSL(filename, unit, getMissionName());
}

////////////////////////////////////////////////////////////////////////
// Return the borders appropriate for the instrument type.
// This should only be called by PigFileModel!!
////////////////////////////////////////////////////////////////////////

int PigMSL::getBorders(PigFileModel *file, int &sl, int &ss,
                                           int &el, int &es)
{
    sl = 3;
    ss = 3;
    el = sl + file->getNL() - 6;
    es = ss + file->getNS() - 6;
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

int PigMSL::canonicalizeFrameName(const char *frame, const char *inst_id,
                char *&canon_frame, char *&short_frame,
                int &max_indices, int *mask_indices)
{
    if (frame == NULL) {
        printError("NULL frame name given, ignored");
        canon_frame = "SITE_FRAME";     // same as FIXED
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

        canon_frame = "SITE_FRAME";
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

        canon_frame = "LOCAL_LEVEL_FRAME";
        short_frame = "LL";
        max_indices = 3;
        if (mask_indices) {
            mask_indices[0] = 1;
            mask_indices[1] = 1;
            mask_indices[2] = 1;
        }
        return 1;
    }

    if ((strcasecmp(frame, "Rover") == 0) ||
        (strcasecmp(frame, "RNAV") == 0) ||
        (strcasecmp(frame, "ROVER_NAV") == 0) ||
        (strcasecmp(frame, "ROVER_NAV_FRAME") == 0) ||
        (strcasecmp(frame, "ROVER_FRAME") == 0) ||
        (strcasecmp(frame, "Rover Navigation") == 0)) { // alias

        canon_frame = "ROVER_NAV_FRAME";
        short_frame = "ROVER";
        max_indices = 3;
        if (mask_indices) {
            mask_indices[0] = 1;
            mask_indices[1] = 1;
            mask_indices[2] = 1;
        }
        return 1;
    }

    if ((strcasecmp(frame, "RMECH") == 0) ||
        (strcasecmp(frame, "ROVER_MECH") == 0) ||
        (strcasecmp(frame, "ROVER_MECH_FRAME") == 0) ||
        (strcasecmp(frame, "Rover Mechanical") == 0)) { // alias

        canon_frame = "ROVER_MECH_FRAME";
        short_frame = "RMECH";
        max_indices = 3;
        if (mask_indices) {
            mask_indices[0] = 1;
            mask_indices[1] = 1;
            mask_indices[2] = 1;
        }
        return 1;
    }

    if ((strcasecmp(frame, "RSM") == 0) ||
	(strcasecmp(frame, "RSM_HEAD_FRAME") == 0) ||
        (strcasecmp(frame, "RSM_FRAME") == 0)) {

        canon_frame = "RSM_FRAME";
        short_frame = "RSM";
        max_indices = 10;
        if (mask_indices) {
            mask_indices[0] = 1;
            mask_indices[1] = 1;
            mask_indices[2] = 1;
            mask_indices[3] = -1;       // ARM
            mask_indices[4] = -1;       // CHIMRA
            mask_indices[5] = -1;       // DRILL
            mask_indices[6] = 1;        // RSM
            mask_indices[7] = -1;       // HGA
            mask_indices[8] = -1;       // DRT
            mask_indices[9] = -1;       // IC
        }
        return 1;
    }

    int arm = FALSE;

    if ((strcasecmp(frame, "ARM_APXS_FRAME") == 0) ||
	(strcasecmp(frame, "APXS") == 0) ||
        (strcasecmp(frame, "ARM_APXS") == 0)) {

        canon_frame = "ARM_APXS_FRAME";
        short_frame = "APXS";
	arm = TRUE;
    }

    if ((strcasecmp(frame, "ARM_DRILL_FRAME") == 0) ||
	(strcasecmp(frame, "DRILL") == 0) ||
        (strcasecmp(frame, "ARM_DRILL") == 0)) {

        canon_frame = "ARM_DRILL_FRAME";
        short_frame = "DRILL";
	arm = TRUE;
    }

    if ((strcasecmp(frame, "ARM_MAHLI_FRAME") == 0) ||
	(strcasecmp(frame, "MAHLI") == 0) ||
        (strcasecmp(frame, "ARM_MAHLI") == 0)) {

        canon_frame = "ARM_DRILL_FRAME";
        short_frame = "DRILL";
	arm = TRUE;
    }

    if ((strcasecmp(frame, "ARM_TURRET_FRAME") == 0) ||
	(strcasecmp(frame, "TURRET") == 0) ||
        (strcasecmp(frame, "ARM_TURRET") == 0)) {

        canon_frame = "ARM_TURRET_FRAME";
        short_frame = "TURRET";
	arm = TRUE;
    }

    if ((strcasecmp(frame, "ARM_DRT_FRAME") == 0) ||
	(strcasecmp(frame, "DRT") == 0) ||
        (strcasecmp(frame, "ARM_DRT") == 0)) {

        canon_frame = "ARM_DRT_FRAME";
        short_frame = "DRT";
	arm = TRUE;
    }

    if ((strcasecmp(frame, "ARM_PORTION_FRAME") == 0) ||
	(strcasecmp(frame, "PORTION") == 0) ||
        (strcasecmp(frame, "ARM_PORTION") == 0)) {

        canon_frame = "ARM_PORTION_FRAME";
        short_frame = "PORTION";
	arm = TRUE;
    }

    if ((strcasecmp(frame, "ARM_SCOOP_TIP_FRAME") == 0) ||
	(strcasecmp(frame, "SCOOP_TIP") == 0) ||
        (strcasecmp(frame, "ARM_SCOOP_TIP") == 0)) {

        canon_frame = "ARM_SCOOP_TIP_FRAME";
        short_frame = "SCOOP_TIP";
	arm = TRUE;
    }

    if ((strcasecmp(frame, "ARM_SCOOP_TCP_FRAME") == 0) ||
	(strcasecmp(frame, "SCOOP_TCP") == 0) ||
        (strcasecmp(frame, "ARM_SCOOP_TCP") == 0)) {

        canon_frame = "ARM_SCOOP_TCP_FRAME";
        short_frame = "SCOOP_TCP";
	arm = TRUE;
    }

    if (arm) {
        max_indices = 10;
        if (mask_indices) {
            mask_indices[0] = 1;
            mask_indices[1] = 1;
            mask_indices[2] = 1;
            mask_indices[3] = 1;        // ARM
            mask_indices[4] = -1;       // CHIMRA
            mask_indices[5] = -1;       // DRILL
            mask_indices[6] = -1;       // RSM
            mask_indices[7] = -1;       // HGA
            mask_indices[8] = -1;       // DRT
            mask_indices[9] = -1;       // IC
        }
        return 1;
    }

    char msg[256];
    sprintf(msg, "Unknown MSL coordinate system frame: '%s', Ignored", frame);
    printError(msg);

    canon_frame = "SITE_FRAME"; // same as FIXED
    short_frame = "SITE";
    max_indices = 1;
    if (mask_indices)
        mask_indices[0] = 1;

    return 0;
}

////////////////////////////////////////////////////////////////////////
// Return the RMC indices for this mission.
////////////////////////////////////////////////////////////////////////

const char *PigMSL::getRmcIndexName(int i)
{
    if (i == 0)  return "SITE";
    if (i == 1)  return "DRIVE";
    if (i == 2)  return "POSE";
    if (i == 3)  return "ARM";
    if (i == 4)  return "CHIMRA";
    if (i == 5)  return "DRILL";
    if (i == 6)  return "RSM";
    if (i == 7)  return "HGA";
    if (i == 8)  return "DRT";
    if (i == 9)  return "IC";

    return "UNKNOWN";   // oops!
}

///////////////////////////////////////////////////////////////////////
// Add constant coordinate systems related to this one to the RSM
// database.  For MSL, constant coord systems are:
// RMECH - constant child of RNAV
// LOCAL_LEVEL - constant peer of RNAV
// Note that the csdefs are merely put in as potential CS's.  The CS
// is not actually instantiated unless asked for.
////////////////////////////////////////////////////////////////////////

void PigMSL::addConstantCoordSystems(PigRoverStateManager *rsm,
                                     PigCSDefinition *csdef)
{
    // Add Local Level
    PigMission::addConstantCoordSystems(rsm, csdef);

    PigCSReference *ident = csdef->getIdentity();
    const char *parent = ident->getFrameName();

    if (strcmp(parent, "ROVER_NAV_FRAME") == 0) {

	// Add RMECH

        PigCSReference *child = new PigCSReference(
                this,
                "ROVER_MECH_FRAME",
                ident->getSolutionId(),
                ident->getIndices(),
                ident->getNumIndices(),
                ident->getInstId());

        PigVector rmech_offset = PigVector(MSL_RMECH_TO_RNAV_OFFSET_X,
                                           MSL_RMECH_TO_RNAV_OFFSET_Y,
                                           MSL_RMECH_TO_RNAV_OFFSET_Z);

        PigQuaternion rmech_q;

        PigCSDefinition *rmech = new PigCSDefinition(this, child, ident,
                                rmech_offset, rmech_q);

        // addSolution will only add it if it's unique

        rsm->addSolution(rmech);
        delete child;
        delete rmech;
    }
}

////////////////////////////////////////////////////////////////////////
// Get the PigRoverStateManager to use for this mission.  It is a singleton
// per mission.
////////////////////////////////////////////////////////////////////////

PigRoverStateManager *PigMSL::getRoverStateManager()
{
    if (_msl_rsm == NULL) {
        _msl_rsm = new PigRoverStateManagerMSL(this);
        // Add "telemetry" as the lowest priority...
        _msl_rsm->addPriority("telemetry");
    }
    return _msl_rsm;
}
