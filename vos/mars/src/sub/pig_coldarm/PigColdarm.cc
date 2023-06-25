//////////////////////////////////////////////////////////////////////////
// PigColdarm
//
// Contains all mission-specific code for dealing 
// with the lunar Coldarm mission.
// 
////////////////////////////////////////////////////////////////////////

#include <stdlib.h>

#include "PigColdarm.h"
#include "PigLabelModelColdarm.h"
#include "PigFileModelColdarm.h"
#include "PigCAHVOR.h"
#include "PigCAHVORE.h"
#include "PigPointGenericCamera_0DOF.h"
#include "PigPointGenericCamera_6dof.h"
#include "PigPointGenericCamera_7dof.h"
#include "PigPointColdarmBodyFixedCamera.h"
#include "PigRoverStateManagerColdarm.h"
#include "RadiometryColdarm.h"
#include "PigCSReference.h"
#include "PigCSDefinition.h"
#include "PigColorModelColdarm.h"

#include "PigXerces.h"

PigRoverStateManager *PigColdarm::_coldarm_rsm = NULL;

PigCoordSystem *PigColdarm::_coldarm_cs_db[PIG_MAX_CS];
int PigColdarm::_coldarm_cs_db_count = 0;
PigCoordSystem *PigColdarm::_coldarm_fixed_cs = NULL;


////////////////////////////////////////////////////////////////////////
// The INSTRUMENT_HOST_ID label contains "COLDARM".
// This value is what is used to find config files.
////////////////////////////////////////////////////////////////////////
PigColdarm::PigColdarm(const char *mission_name, const char *host_id)
{
    strcpy(_mission_name, mission_name);
    strcpy(_host_id, host_id);
}
////////////////////////////////////////////////////////////////////////
// Create a Coldarm camera model given an image file.
////////////////////////////////////////////////////////////////////////

PigCameraModel *PigColdarm::createCameraModel(PigFileModel *file,
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

    if (strstr(inst_id, "RIGHT") != NULL) {
        if (frame != NULL && frame[0] != 'R')
            match = FALSE;
    }
    else if (strstr(inst_id, "LEFT") != NULL) {
        if (frame != NULL && frame[0] != 'L')
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
    int focus = file->getInstrumentFocusPosition(0);
    int zoom = file->getInstrumentZoomPosition(0);

    sprintf(subtype, "filter=%s focus=%d zoom=%d temp=%f partner_temp=%f\n",
			(filt == NULL) ? "NULL" : filt,
			focus, zoom, our_temp, partner_temp);

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
                                 subtype,	// filter+focus+temp+zoom
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
                                subtype,	// filter+focus+temp+zoom
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
                                subtype,	// filter+focus+temp+zoom
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
                                subtype,	// filter+focus+temp+zoom
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
void PigColdarm::constructCameraModelFileName(const char *calibration,
					  const char *serial,
					  const char *subtype,
					  const char *type,
					  int interp_focus,
					  int interp_zoom,
					  int interp_temp,
					  int interp_partner_temp,
					  int use_filt,
					  char *cmod_file)
{    
    strcpy(cmod_file, "");
    strcat(cmod_file, "camera_models/Coldarm");
    
    if (serial) {
        strcat(cmod_file, "_SN_");
        strcat(cmod_file, serial);
    }

    char filt[100];
    char *filter = NULL;
    double focus = 0;	// read as float for maximum flexibility
    double zoom = 0;
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

    if (interp_focus || interp_temp || interp_zoom) {
	strcat(cmod_file, ".interp");
	if (interp_focus)
	    strcat(cmod_file, "f");
	if (interp_zoom)
	    strcat(cmod_file, "z");
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
PigCameraModel *PigColdarm::createCameraModel(PigCameraMapEntry *entry,
					  const char *subtype,
					  const char *construction,
					  const char *calibration)
{

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

	// Get the default type
	return new PigCAHVORE(getMissionName(), 
				  entry->getID(), entry->getSerialNumber(), 
				  subtype, construction, calibration);
    }
}

////////////////////////////////////////////////////////////////////////
// Create an Coldarm pointing model given strings.  Valid strings for instrument
// are in the camera mapping file ("NAVCAM_LEFT" for example).
// All strings are case-insensitive.
//
// User-specified pointing method is stored in CameraModel's subtype member
// variable.  The following pointing methods supported for Coldarm:
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
// PM=6DOF
//   Arm cameras only.  Uses 6-DOF (euler angle plus position) generic model.
// PM=7DOF
//   Arm cameras only.  Used 7-DOF (quaternion plus position) generic model.
//
// Although Coldarm has no arm cameras, 6DOF and 7DOF could be used to
// tweak pointing of the fixed cameras.
//
////////////////////////////////////////////////////////////////////////

PigPointingModel *PigColdarm::createPointingModel(PigCameraModel *cm,
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
    
    // Body-mounted camera

    // Override case
    if (allow_type_override && pointMethodValue != NULL) {
	if (strncasecmp(pointMethodValue, "6dof", 4) == 0) {
            if (type != NULL && strcasecmp(type, "GenericCamera_6dof") != 0) {
                PigModelBase::printUniqueStaticMsg(
"WARNING: Pointing Model overridden to Generic_Camera6dof via command line",
                                                 PigMsgWarning);
	    }
            strcpy(model_type, "GenericCamera_6dof");
        } else if (strncasecmp(pointMethodValue, "7dof", 4) == 0) {
            if (type != NULL && strcasecmp(type, "GenericCamera_7dof") != 0) {
                PigModelBase::printUniqueStaticMsg(
"WARNING: Pointing Model overridden to Generic_Camera7dof via command line",
                                                 PigMsgWarning);
	    }
            strcpy(model_type, "GenericCamera_7dof");
        } else {
          strcpy(model_type, "GenericCamera_0DOF");
	}

    // No-override case

    } else if (cm->getCameraConstruction() != NULL &&
         (strncasecmp(cm->getCameraConstruction(), "label", 5) == 0)) {

          strcpy(model_type, "GenericCamera_0DOF");
    } else {
	if (strlen(model_type) == 0)
	    strcpy(model_type, "ColdarmBodyFixedCamera");
    }

    // We've figured out the model type, now create it

    if (strcasecmp(model_type, "GenericCamera_7dof") == 0)
        return new PigPointGenericCamera_7dof(cm, this, NULL);
    else if (strcasecmp(model_type, "GenericCamera_6dof") == 0)
        return new PigPointGenericCamera_6dof(cm, this, NULL);
    else if (strcasecmp(model_type, "GenericCamera_0DOF") == 0)
        return new PigPointGenericCamera_0DOF(cm, this, NULL);
    else if (strcasecmp(model_type, "ColdarmBodyFixedCamera") == 0)
        return new PigPointColdarmBodyFixedCamera(cm, this, instrument);

    // shouldn't be here...

    sprintf(msg, "Unknown Coldarm pointing model: %s", model_type);
    PigModelBase::printStaticMsg(msg, PigMsgFatal);

    return NULL;
}

////////////////////////////////////////////////////////////////////////
// Create a Coldarm radiometry model given an image file.
////////////////////////////////////////////////////////////////////////

RadiometryModel *PigColdarm::createRadiometryModel(PigFileModel *file)
{
    return RadiometryColdarm::create(file);
}

////////////////////////////////////////////////////////////////////////
// Create a Coldarm color model given an image file.
////////////////////////////////////////////////////////////////////////

PigColorModel *PigColdarm::createColorModel(PigFileModel *file)
{
    return PigColorModelColdarm::create(file);
}

////////////////////////////////////////////////////////////////////////
// Create a Coldarm LabelModel.
////////////////////////////////////////////////////////////////////////

PigLabelModel *PigColdarm::createLabelModel(int unit)
{
    return new PigLabelModelColdarm(unit, getMissionName());
}

////////////////////////////////////////////////////////////////////////
// Create a Coldarm FileModel.
////////////////////////////////////////////////////////////////////////

PigFileModel *PigColdarm::createFileModel(const char *filename, int unit)
{
    return new PigFileModelColdarm(filename, unit, getMissionName());
}

////////////////////////////////////////////////////////////////////////
// Return the borders appropriate for the instrument type.
// This should only be called by PigFileModel!!
// Assume 3 to remove Malvar fuzz
////////////////////////////////////////////////////////////////////////

int PigColdarm::getBorders(PigFileModel *file, int &sl, int &ss,
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

int PigColdarm::canonicalizeFrameName(const char *frame, const char *inst_id,
		char *&canon_frame, char *&short_frame,
		int &max_indices, int *mask_indices)
{
    if (frame == NULL) {
	printError("NULL frame name given, ignored");
	canon_frame = "SITE_FRAME";	// same as FIXED
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
        (strcasecmp(frame, "ORIGIN") == 0) ||
        (strcasecmp(frame, "ORIGIN_FRAME") == 0) ||
        (strcasecmp(frame, "ROVER_NAV") == 0) ||
        (strcasecmp(frame, "ROVER_NAV_FRAME") == 0) ||
        (strcasecmp(frame, "ROVER_FRAME") == 0) ||
        (strcasecmp(frame, "Rover Navigation") == 0)) { // alias

	canon_frame = "ORIGIN_FRAME";
	short_frame = "ORIGIN";
	max_indices = 3;
	if (mask_indices) {
	    mask_indices[0] = 1;
	    mask_indices[1] = 1;
	    mask_indices[2] = 1;
	}
	return 1;
    }

    if ((strcasecmp(frame, "RMECH") == 0) ||
        (strcasecmp(frame, "RA") == 0) ||
        (strcasecmp(frame, "RA_FRAME") == 0) ||
        (strcasecmp(frame, "ROVER_MECH") == 0) ||
        (strcasecmp(frame, "ROVER_MECH_FRAME") == 0) ||
        (strcasecmp(frame, "Rover Mechanical") == 0)) { // alias

        canon_frame = "RA_FRAME";
        short_frame = "RA";
        max_indices = 3;
        if (mask_indices) {
            mask_indices[0] = 1;
            mask_indices[1] = 1;
            mask_indices[2] = 1;
        }
        return 1;
    }


    char msg[256];
    sprintf(msg, "Unknown Coldarm coordinate system frame: '%s', Ignored", frame);
    printError(msg);

    canon_frame = "SITE_FRAME";	// same as FIXED
    short_frame = "SITE";
    max_indices = 1;
    if (mask_indices)
	mask_indices[0] = 1;

    return 0;
}

////////////////////////////////////////////////////////////////////////
// Return the RMC indices for this mission.  
////////////////////////////////////////////////////////////////////////

const char *PigColdarm::getRmcIndexName(int i)
{
    if (i == 0)  return "SITE";
    if (i == 1)  return "DRIVE";
    if (i == 2)  return "POSE";
    if (i == 3)  return "ARM";

    return "UNKNOWN";	// oops!
}

////////////////////////////////////////////////////////////////////////
// Add constant coordinate systems related to this one to the RSM
// database.  For Coldarm, constant coord systems are:
// RA (like RA frame) - constant child of ORIGIN (like ROVER frame)
// Note that the csdefs are merely put in as potential CS's.  The CS
// is not actually instantiated unless asked for.
////////////////////////////////////////////////////////////////////////

void PigColdarm::addConstantCoordSystems(PigRoverStateManager *rsm,
                                     PigCSDefinition *csdef)
{
    // Add Local Level
    PigMission::addConstantCoordSystems(rsm, csdef);

    PigCSReference *ident = csdef->getIdentity();
    const char *parent = ident->getFrameName();

    if (strcmp(parent, "ORIGIN_FRAME") == 0) {

        // Add RA

        PigCSReference *child = new PigCSReference(
                this,
                "RA_FRAME",
                ident->getSolutionId(),
                ident->getIndices(),
                ident->getNumIndices(),
                ident->getInstId());

        PigVector rmech_offset = PigVector(COLDARM_RMECH_TO_RNAV_OFFSET_X,
                                           COLDARM_RMECH_TO_RNAV_OFFSET_Y,
                                           COLDARM_RMECH_TO_RNAV_OFFSET_Z);

        PigQuaternion rmech_q(COLDARM_RMECH_TO_RNAV_QUAT_S,
				COLDARM_RMECH_TO_RNAV_QUAT_V1,
				COLDARM_RMECH_TO_RNAV_QUAT_V2,
				COLDARM_RMECH_TO_RNAV_QUAT_V3);

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

PigRoverStateManager *PigColdarm::getRoverStateManager()
{
    if (_coldarm_rsm == NULL) {
        _coldarm_rsm = new PigRoverStateManagerColdarm(this);
        // Add "telemetry" as the lowest priority...
        _coldarm_rsm->addPriority("telemetry");
    }
    return _coldarm_rsm;
}
