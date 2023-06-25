//////////////////////////////////////////////////////////////////////////
// PigNSYT
//
// Contains all mission-specific code for dealing 
// with the Mars InSight mission.
// 
////////////////////////////////////////////////////////////////////////

#include <stdlib.h>

#include "PigNSYT.h"
#include "PigLabelModelNSYT.h"
#include "PigFileModelNSYT.h"
#include "PigCAHVOR.h"
#include "PigCAHVORE.h"
#include "PigPointGenericCamera_0DOF.h"
#include "PigPointGenericCamera_7dof.h"
#include "PigPointNSYTBodyFixedCamera.h"
#include "PigPointNSYTarmCamera.h"
#include "PigPointNSYTarmCamera6dof.h"
#include "PigRoverStateManagerNSYT.h"
#include "RadiometryNSYT.h"
#include "PigCSReference.h"
#include "PigCSDefinition.h"
#include "PigColorModelNSYT.h"

#include "PigXerces.h"

PigRoverStateManager *PigNSYT::_nsyt_rsm = NULL;

PigCoordSystem *PigNSYT::_nsyt_cs_db[PIG_MAX_CS];
int PigNSYT::_nsyt_cs_db_count = 0;
PigCoordSystem *PigNSYT::_nsyt_fixed_cs = NULL;

////////////////////////////////////////////////////////////////////////
// The INSTRUMENT_HOST_ID label contains "FM" or "TB".  Because these
// are not very unique, we internally change the host ID to add a
// NSYT prefix.  This value is what is used to find config files.  Just
// in case the host already has NSYT, we prepend only if it's not already
// there.  We save the unadorned value for use in comparing against
// the list of existing PigNSYT objects in PigMission::getMissionObject().
// That should be the ONLY user of the "orig" host ID.
////////////////////////////////////////////////////////////////////////
PigNSYT::PigNSYT(const char *mission_name, const char *host_id)
{

    strcpy(_mission_name, mission_name);
    if (strncmp(host_id, "NSYT", 4) == 0) {
        strcpy(_orig_host_id, host_id);
        strcpy(_host_id, host_id);
    } else {
        strcpy(_orig_host_id, host_id);
        strcpy(_host_id, "NSYT");
        strcat(_host_id, host_id);
    }
}

////////////////////////////////////////////////////////////////////////
// Create a NSYT camera model given an image file.
////////////////////////////////////////////////////////////////////////

PigCameraModel *PigNSYT::createCameraModel(PigFileModel *file,
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
    // TBD: create a mosaic camera model if that's what it is...   !!!!

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

    // Put subframe and downsampling info into the 
    // construction string
    char sub_down_str[255];
    sprintf(sub_down_str, "kinematics type=%s subframe=(%d,%d) downsampling=(%d,%d)", 
	    geom_proj_type, dx, dy, hscale, vscale);

    // Combine filter and temperature for the subtype string
    // Unlike MSL we don't include focus or partner temp (since there is
    // no partner and no focus).  Temp isn't used but is retained just in
    // case we have to deal with a temperature dependency later.

    char subtype[150];
    const char *filt = file->getFilterNumber();
    double our_temp = file->getInstrumentTemperature(0);

    sprintf(subtype, "filter=%s focus=%d zoom=%d temp=%f partner_temp=%f\n",
			(filt == NULL) ? "NULL" : filt,
			0, 0,		// no focus or zoom
			our_temp, 0.0);	// no partner temp

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
                                subtype,			// filter+temp
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
                                subtype,			// filter+temp
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
                                subtype,			// filter+temp
                                special,                        // special
                                sub_down_str,                   // construction
				file->getCalibrationSourceId(), // calibration
				cs);                            // coord_sys
        
    return model;
}

////////////////////////////////////////////////////////////////////////
// InSight does not support stereo partner.  So check for that, issue an
// error if needed, then call the base class implementation.
////////////////////////////////////////////////////////////////////////

PigCameraModel *PigNSYT::createCameraModel(const char *instrument,
                                          const char *version, 
                                          const char *subtype, 
                                          const char *special,
                                          const char *construction, 
                                          const char *calibration, 
                                          PigCoordSystem *cs)
{
    if (special != NULL && strlen(special) > 0) {
        if (strcasecmp(special, "stereo") == 0) {
	    PigModelBase::printStaticMsg(
		"Stereo partners are not supported for InSight", PigMsgError);
	}
	else {
	    PigModelBase::printStaticMsg(
		"Unrecognized 'special' string in camera model", PigMsgError);
	}
        return NULL;		// all specials are invalid
    }

    return createCameraModelInterp(instrument, version, subtype, special,
			construction, calibration, cs);
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
void PigNSYT::constructCameraModelFileName(const char *calibration,
					  const char *serial,
					  const char *subtype,
					  const char *type,
					  int interp_focus,
					  int interp_zoom,
					  int interp_temp,
					  int interp_partner_temp,
					  int use_filt,
					  char *cmod_file)
// interp_focus, interp_zoom, interp_partner_temp are unused
{    
    strcpy(cmod_file, "");
    strcat(cmod_file, "camera_models/NSYT");
    
    if (serial) {
        strcat(cmod_file, "_SN_");
        strcat(cmod_file, serial);
    }

    char filt[100];
    char *filter = NULL;
    double temperature = 0;
    double focus = 0;		// not used
    double zoom = 0;		// not used
    double partner_temp = 0;	// not used
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

    if (interp_temp) {
	strcat(cmod_file, ".interp");
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
PigCameraModel *PigNSYT::createCameraModel(PigCameraMapEntry *entry,
					  const char *subtype,
					  const char *construction,
					  const char *calibration)
{
    // IDC uses CAHVOR by default, while ICC uses CAHVORE.
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

	// Get the default type for known instruments
	if (strncasecmp(entry->getShortID(), "IDC", 3) == 0) {
	    return new PigCAHVOR(getMissionName(), 
				 entry->getID(), entry->getSerialNumber(), 
				 subtype, construction, calibration);
	}
	else if (strncasecmp(entry->getShortID(), "ICC", 3) == 0) {
	    return new PigCAHVORE(getMissionName(), 
				  entry->getID(), entry->getSerialNumber(), 
				  subtype, construction, calibration);
	}
	else {
	  sprintf(msg, "Unknown NSYT instrument %s", entry->getShortID());
	  PigModelBase::printStaticMsg(msg, PigMsgFatal);
	  return NULL;
	}
    }
}

////////////////////////////////////////////////////////////////////////
// Create an NSYT pointing model given strings.  Valid strings are:
// instrument:  "IDC", "ICC"
// All strings are case-insensitive.
//
// User-specified pointing method is stored in CameraModel's subtype member
// variable.  The following pointing methods supported for NSYT:
//
// CM=KINEMATICS (default) 
//   Uses dynamically derived camera models. 
//   For IDC use 6- or 7-dof, for ICC use body fixed model.
// CM=LABEL 
//   Uses camera models from the label.  Body Fixed cameras will use 
//   0DOF GenericCamera pointing model.  Arm cameras use generic 6DOF.
//
// PM=xxx
//   Overrides the pointing model in an instrument-specific way.
// PM=6DOF
//   IDC only.  Uses 6-DOF (euler angle plus position) NSYT model.
// PM=7DOF
//   IDC only.  Used 7-DOF (quaternion plus position) NSYT model.
// 
////////////////////////////////////////////////////////////////////////

PigPointingModel *PigNSYT::createPointingModel(PigCameraModel *cm,
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
// IDC.  7dof (quat + pos) is the default, can select 6dof (pos + angles)
///////////////////////////

    if (strncasecmp(instrument, "IDC", 3) == 0) {

        if (cm->getCameraConstruction() != NULL &&
	    ((strncasecmp(cm->getCameraConstruction(), "label", 5) == 0)))
            strcpy(model_type, "GenericCamera_7dof");
        else if (allow_type_override && pointMethodValue != NULL) {
            if (strncasecmp(pointMethodValue, "6dof", 4) == 0) {
                if (type != NULL && strcasecmp(type, "NSYTcamera6dof") != 0)
                    PigModelBase::printUniqueStaticMsg(
"WARNING: Pointing Model overridden to NSYTcamera6dof via command line",
                                                 PigMsgWarning);
                strcpy(model_type, "NSYTcamera6dof");
            }
            else if ((strncasecmp(pointMethodValue, "7dof", 4) == 0)) {
                if (type != NULL && strcasecmp(type, "NSYTcamera7dof") != 0)
                    PigModelBase::printUniqueStaticMsg(
"WARNING: Pointing Model overridden to NSYTcamera7dof via command line",
                                                 PigMsgWarning);
                strcpy(model_type, "NSYTcamera7dof");
            }

	}

	// We've figured out the model type, now create it

        if (strcasecmp(model_type, "GenericCamera_7dof") == 0)
            return new PigPointGenericCamera_7dof(cm, this, NULL);

        else if (strcasecmp(model_type, "NSYTcamera6dof") == 0)
            return new PigPointNSYTarmCamera6dof(cm, this, instrument);
        else
            // Default 7dof pointing model
            return new PigPointNSYTarmCamera(cm, this, instrument);

    }

///////////////////////////
// BODY-MOUNTED CAMERAS
///////////////////////////

    else if (strncasecmp(instrument, "ICC", 3) == 0) {
      if(cm->getCameraConstruction() != NULL &&
         (strncasecmp(cm->getCameraConstruction(), "label", 5) == 0))
          return new PigPointGenericCamera_0DOF(cm, this, NULL);
      else
          return new PigPointNSYTBodyFixedCamera(cm, this, instrument);
    }

    else {
        sprintf(msg, "Unknown NSYT instrument %s", instrument);
        PigModelBase::printStaticMsg(msg, PigMsgFatal);
        return NULL;
    }
    // shouldn't be here...
    return NULL;
}

////////////////////////////////////////////////////////////////////////
// Create a NSYT radiometry model given an image file.
////////////////////////////////////////////////////////////////////////

RadiometryModel *PigNSYT::createRadiometryModel(PigFileModel *file)
{
    return RadiometryNSYT::create(file);
}

////////////////////////////////////////////////////////////////////////
// Create a NSYT color model given an image file.
////////////////////////////////////////////////////////////////////////

PigColorModel *PigNSYT::createColorModel(PigFileModel *file)
{
    return PigColorModelNSYT::create(file);
}

////////////////////////////////////////////////////////////////////////
// Create a NSYT LabelModel.
////////////////////////////////////////////////////////////////////////

PigLabelModel *PigNSYT::createLabelModel(int unit)
{
    return new PigLabelModelNSYT(unit, getMissionName());
}

////////////////////////////////////////////////////////////////////////
// Create a NSYT FileModel.
////////////////////////////////////////////////////////////////////////

PigFileModel *PigNSYT::createFileModel(const char *filename, int unit)
{
    return new PigFileModelNSYT(filename, unit, getMissionName());
}

////////////////////////////////////////////////////////////////////////
// Return the borders appropriate for the instrument type.
// This should only be called by PigFileModel!!
////////////////////////////////////////////////////////////////////////

int PigNSYT::getBorders(PigFileModel *file, int &sl, int &ss,
                                           int &el, int &es)
{
    // 3 pixels should be enough for the Bayer border...
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

int PigNSYT::canonicalizeFrameName(const char *frame, const char *inst_id,
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
        max_indices = 2;
        if (mask_indices) {
            mask_indices[0] = 1;
            mask_indices[1] = 1;
        }
        return 1;
    }

    if ((strcasecmp(frame, "Rover") == 0) ||
        (strcasecmp(frame, "RNAV") == 0) ||
        (strcasecmp(frame, "ROVER_NAV") == 0) ||
        (strcasecmp(frame, "ROVER_NAV_FRAME") == 0) ||
        (strcasecmp(frame, "ROVER_FRAME") == 0) ||
	(strcasecmp(frame, "LANDER") == 0) ||
	(strcasecmp(frame, "LANDER_FRAME") == 0) ||
        (strcasecmp(frame, "Rover Navigation") == 0)) { // alias

        canon_frame = "LANDER_FRAME";
        short_frame = "LANDER";
        max_indices = 2;
        if (mask_indices) {
            mask_indices[0] = 1;
            mask_indices[1] = 1;
        }
        return 1;
    }

    char msg[256];
    sprintf(msg, "Unknown NSYT coordinate system frame: '%s', Ignored", frame);
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

const char *PigNSYT::getRmcIndexName(int i)
{
    if (i == 0)  return "SITE";
    if (i == 1)  return "DRIVE";

    return "UNKNOWN";   // oops!
}

////////////////////////////////////////////////////////////////////////
// Get the PigRoverStateManager to use for this mission.  It is a singleton
// per mission.
////////////////////////////////////////////////////////////////////////

PigRoverStateManager *PigNSYT::getRoverStateManager()
{
    if (_nsyt_rsm == NULL) {
        _nsyt_rsm = new PigRoverStateManagerNSYT(this);
        // Add "telemetry" as the lowest priority...
        _nsyt_rsm->addPriority("telemetry");
    }
    return _nsyt_rsm;
}
