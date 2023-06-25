//////////////////////////////////////////////////////////////////////////
// PigM20
//
// Contains all mission-specific code for dealing 
// with the Mars 2020 mission.
// 
////////////////////////////////////////////////////////////////////////

#include <stdlib.h>

#include "PigM20.h"
#include "PigLabelModelM20.h"
#include "PigFileModelM20.h"
#include "PigCAHVOR.h"
#include "PigCAHVORE.h"
#include "PigPointGenericCamera_0DOF.h"
#include "PigPointGenericCamera_AzEl.h"
#include "PigPointGenericCamera_7dof.h"
#include "PigPointGenericCamera_6dof.h"
#include "PigPointM20mastCamera.h"
#include "PigPointM20mastCamera3dof.h"
#include "PigPointM20mastCameraScale.h"
#include "PigPointM20BodyFixedCamera.h"
#include "PigPointM20armCamera.h"
#include "PigPointM20armCamera6dof.h"
#include "PigPointM20heliCamera.h"     
#include "PigPointM20heliCamera6dof.h"
#include "PigRoverStateManagerM20.h"
#include "RadiometryM20.h"
#include "PigCSReference.h"
#include "PigCSDefinition.h"
#include "PigColorModelM20.h"

#include "PigXerces.h"

PigRoverStateManager *PigM20::_m20_rsm = NULL;

PigCoordSystem *PigM20::_m20_cs_db[PIG_MAX_CS];
int PigM20::_m20_cs_db_count = 0;
PigCoordSystem *PigM20::_m20_fixed_cs = NULL;


////////////////////////////////////////////////////////////////////////
// The INSTRUMENT_HOST_ID label contains "M20" or "M20SIM".
// This value is what is used to find config files.
////////////////////////////////////////////////////////////////////////
PigM20::PigM20(const char *mission_name, const char *host_id)
{
    strcpy(_mission_name, mission_name);
    strcpy(_host_id, host_id);
}
////////////////////////////////////////////////////////////////////////
// Create a M20 camera model given an image file.
////////////////////////////////////////////////////////////////////////

PigCameraModel *PigM20::createCameraModel(PigFileModel *file,
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
    if (strcmp(file->getModelName(), "FileModelM20") == 0) {
	our_temp =
		((PigFileModelM20 *)file)->getInstrumentTemperature(0, false);
	partner_temp =
		((PigFileModelM20 *)file)->getInstrumentTemperature(0, true);
    }
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
void PigM20::constructCameraModelFileName(const char *calibration,
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
    strcat(cmod_file, "camera_models/M20");
    
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
PigCameraModel *PigM20::createCameraModel(PigCameraMapEntry *entry,
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

	// Get the default type for known instruments
	char *id = entry->getShortID();
	if((strncasecmp(id, "EA", 2) == 0) ||
	   (strncasecmp(id, "EB", 2) == 0) ||
	   (strncasecmp(id, "EC", 2) == 0) ||
	   (strncasecmp(id, "ED", 2) == 0) ||
	   (strncasecmp(id, "ES", 2) == 0) ||
	   (strncasecmp(id, "EU", 2) == 0) ||
	   (strncasecmp(id, "LR", 2) == 0) ||
	   (strncasecmp(id, "PC", 2) == 0) ||
	   (strncasecmp(id, "SC", 2) == 0) ||
	   (strncasecmp(id, "SI", 2) == 0) ||
	   (strncasecmp(id, "WS", 2) == 0) ||
	   (strncasecmp(id, "ZL", 2) == 0) ||
	   (strncasecmp(id, "ZR", 2) == 0)) {
	    return new PigCAHVOR(getMissionName(), 
				 entry->getID(), entry->getSerialNumber(), 
				 subtype, construction, calibration);
	}
	else if((strncasecmp(id, "BL", 2) == 0) ||
	        (strncasecmp(id, "BR", 2) == 0) ||
	        (strncasecmp(id, "FL", 2) == 0) ||
	        (strncasecmp(id, "FR", 2) == 0) ||
	        (strncasecmp(id, "NL", 2) == 0) ||
	        (strncasecmp(id, "NR", 2) == 0) ||
	        (strncasecmp(id, "RL", 2) == 0) ||
	        (strncasecmp(id, "RR", 2) == 0) ||
	        (strncasecmp(id, "EL", 2) == 0) ||
	        (strncasecmp(id, "HN", 2) == 0) ||
	        (strncasecmp(id, "HS", 2) == 0)) {
	    return new PigCAHVORE(getMissionName(), 
				  entry->getID(), entry->getSerialNumber(), 
				  subtype, construction, calibration);
	}
	else if((strncasecmp(id, "CC", 2) == 0)) {
            return new PigCAHV(getMissionName(), 
			     	  entry->getID(), entry->getSerialNumber(), 
			     	  subtype, construction, calibration);
	}
	else {
	  sprintf(msg, "Unknown M2020 instrument %s", entry->getShortID());
	  PigModelBase::printStaticMsg(msg, PigMsgFatal);
	  return NULL;
	}
    }
}

////////////////////////////////////////////////////////////////////////
// Create an M20 pointing model given strings.  Valid strings for instrument
// are in the camera mapping file ("NAVCAM_LEFT" for example).
// All strings are case-insensitive.
//
// User-specified pointing method is stored in CameraModel's subtype member
// variable.  The following pointing methods supported for M20:
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
//   Mast-mounted cameras only.  Uses the 2-DOF az/el M20-specific model.
// PM=3DOF
//   Mast-mounted cameras only.  Uses the 3-DOF az/el/twist M20-specific model.
// PM=SCALE
//   Mast-mounted cameras only.  Uses the 4-DOF az/el/twist/scale M20-sp model.
// PM=MAST6
//   Mast-mounted cameras only.  Uses generic 6-DOF (euler angle plus position)
// PM=MAST7
//   Mast-mounted cameras only.  Uses generic 7-DOF (quat plus position)
// PM=6DOF
//   Arm cameras only.  Uses 6-DOF (euler angle plus position) M20 model.
// PM=7DOF
//   Arm/Heli cameras only.  Used 7-DOF (quaternion plus position) M20 model.
//
// The MAST6 and 6DOF are really the same, but separating them allows
// independent control over mast-mounted and arm cameras.
// 
////////////////////////////////////////////////////////////////////////

PigPointingModel *PigM20::createPointingModel(PigCameraModel *cm,
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

    if ((strncasecmp(instrument, "MCZ", 3) == 0) ||
        (strncasecmp(instrument, "NAV", 3) == 0) ||
	(strncasecmp(instrument, "SUPERCAM_RMI", 12) == 0)) {

        if (cm->getCameraConstruction() != NULL &&
	    ((strncasecmp(cm->getCameraConstruction(), "label", 5) == 0)))
            strcpy(model_type, "GenericCamera_AzEl");

        else if (allow_type_override && pointMethodValue != NULL) {
            if (strncasecmp(pointMethodValue, "2dof", 4) == 0) {
                if (type != NULL && strcasecmp(type, "M20mastCamera") != 0)
                    PigModelBase::printUniqueStaticMsg(
"WARNING: Pointing Model overridden to M20mastCamera via command line",
                                                 PigMsgWarning);

                strcpy(model_type, "M20mastCamera");
            }
            else if ((strncasecmp(pointMethodValue, "3dof", 4) == 0)) {
                if (type != NULL && strcasecmp(type, "M20mastCamera3dof") != 0)
                    PigModelBase::printUniqueStaticMsg(
"WARNING: Pointing Model overridden to M20mastCamera3dof via command line",
                                                 PigMsgWarning);
                strcpy(model_type, "M20mastCamera3dof");
            }
            else if ((strncasecmp(pointMethodValue, "scale", 4) == 0)) {
                if (type != NULL && strcasecmp(type, "M20mastCameraScale") != 0)
                    PigModelBase::printUniqueStaticMsg(
"WARNING: Pointing Model overridden to M20mastCameraScale via command line",
                                                 PigMsgWarning);
                strcpy(model_type, "M20mastCameraScale");
            }
        } else {
	    if (strlen(model_type) == 0)
	        strcpy(model_type, "M20mastCamera");	// default
	}
    }

///////////////////////////
// ARM.  7dof (quat + pos) is the default, can select 6dof (pos + angles)
///////////////////////////

    else if ((strncasecmp(instrument, "PIXL_MCC", 8) == 0) ||
	     (strncasecmp(instrument, "SHERLOC_WATSON", 14) == 0) ||
	     (strncasecmp(instrument, "SHERLOC_ACI", 10) == 0)) {

        if (cm->getCameraConstruction() != NULL &&
	    ((strncasecmp(cm->getCameraConstruction(), "label", 5) == 0)))
            strcpy(model_type, "GenericCamera_7dof");
        else if (allow_type_override && pointMethodValue != NULL) {
            if (strncasecmp(pointMethodValue, "6dof", 4) == 0) {
                if (type != NULL && strcasecmp(type, "M20armCamera6dof") != 0)
                    PigModelBase::printUniqueStaticMsg(
"WARNING: Pointing Model overridden to M20armCamera6dof via command line",
                                                 PigMsgWarning);
                strcpy(model_type, "M20armCamera6dof");
            }
            else if ((strncasecmp(pointMethodValue, "7dof", 4) == 0)) {
                if (type != NULL && strcasecmp(type, "M20armCamera") != 0)
                    PigModelBase::printUniqueStaticMsg(
"WARNING: Pointing Model overridden to M20armCamera via command line",
                                                 PigMsgWarning);
                strcpy(model_type, "M20armCamera");
            }

	} else {
	    if (strlen(model_type) == 0)
	        strcpy(model_type, "M20armCamera");	// default
	}
    }

///////////////////////////
// HELICOPTER.  The cameras are actually body-fixed to the heli.  
// Conceptually, the heli is treated similar to an ARM instrument. The 
// instrument has two fixed cameras on it (nav and rte), and is at the end of a 
// very long (virtual) arm whose base is the take off location.
// The heli pointing is managed by the relative pose and orientation of the 
// M-frame CS (attached to the heli) w.r.t. the G-frame CS (heli pose and
// orientation at takeoff. 
// The class to manage heli pointing derive from the ARM class and is similar
// on every aspect except for the way the pointing is retrieved from the
// label (i.e., the M-frame pose and orientation w.r.t. G-frame).
// Initially, the arm pointing model was used and retrieve the pointing from the
// point file which was incorrect because the M-frame is attached to the Heli.
//
// HELICOPTER. Initial and Deprecated approach:
// The cameras are actually nody-fixed to the heli. So ideally, we'd move the 
// heli as a unit (a coord sys thing).  However, coord sys pointing correction 
// is not super well developed, and there is measurably zero chance of getting a
// simultaneous RTE and NAV picture, so it's easiest to leave the heli where it
// is and just adjust the pointing of the camera itself.  This breaks down a 
// little if the heli is on the ground (a stable position) and takes images from
// both cameras, but that's not a big concern.  We actually use the arm pointing
// model because it reads the point file and the point file supports 
// camera-specific cal pointing now.  The cal position is the orientation of the
// M frame (+x).
///////////////////////////

    else if ((strncasecmp(instrument, "HELI_NAV", 8) == 0) ||
	     (strncasecmp(instrument, "HELI_RTE", 8) == 0)) {

        if (allow_type_override && pointMethodValue != NULL) {
            if (strncasecmp(pointMethodValue, "6dof", 4) == 0) {
                if (type != NULL && strcasecmp(type, "M20heliCamera") != 0)
                    PigModelBase::printUniqueStaticMsg(
"WARNING: Pointing Model overridden to GenericCamera_6dof via command line",
                                                 PigMsgWarning);
                strcpy(model_type, "M20heliCamera6dof");
            }
	}
        if (strlen(model_type) == 0)
           strcpy(model_type, "M20heliCamera");	// default
    }

///////////////////////////
// BODY-MOUNTED CAMERAS
///////////////////////////

    else if ((strstr(instrument, "HAZ") != NULL) ||
	     (strncasecmp(instrument, "CACHECAM",8) == 0) ||
	     (strncasecmp(instrument, "EDL", 3) == 0) ||
	     (strncasecmp(instrument, "LCAM", 4) == 0) ||
	     (strncasecmp(instrument, "SKYCAM", 6) == 0)) {
      if(cm->getCameraConstruction() != NULL &&
         (strncasecmp(cm->getCameraConstruction(), "label", 5) == 0))
          strcpy(model_type, "GenericCamera_0DOF");
      else {
	if (strlen(model_type) == 0)
	    strcpy(model_type, "M20BodyFixedCamera");
      }
    }

    else {
        sprintf(msg, "Unknown M20 instrument %s", instrument);
        PigModelBase::printStaticMsg(msg, PigMsgFatal);
        return NULL;
    }

    // We've figured out the model type, now create it

        if (strcasecmp(model_type, "GenericCamera_AzEl") == 0)
            return new PigPointGenericCamera_AzEl(cm, this, NULL);

        // Az/El + twist pointing model
        else if (strcasecmp(model_type, "M20mastCamera3dof") == 0)
            return new PigPointM20mastCamera3dof(cm,
					this, instrument);

        // Az/El + twist + scale pointing model
        else if (strcasecmp(model_type, "M20mastCameraScale") == 0)
            return new PigPointM20mastCameraScale(cm,
					this, instrument);
        else if (strcasecmp(model_type, "M20mastCamera") == 0)
            return new PigPointM20mastCamera(cm, this, instrument);

        else if (strcasecmp(model_type, "GenericCamera_7dof") == 0)
            return new PigPointGenericCamera_7dof(cm, this, NULL);
        else if (strcasecmp(model_type, "GenericCamera_6dof") == 0)
            return new PigPointGenericCamera_6dof(cm, this, NULL);

        else if (strcasecmp(model_type, "M20armCamera6dof") == 0)
            return new PigPointM20armCamera6dof(cm,this,instrument);
        else if (strcasecmp(model_type, "M20armCamera7dof") == 0)	// should not be used
            return new PigPointM20armCamera(cm, this, instrument);
        else if (strcasecmp(model_type, "M20armCamera") == 0)		// should be used
            return new PigPointM20armCamera(cm, this, instrument);

        else if (strcasecmp(model_type, "M20heliCamera6dof") == 0)
            return new PigPointM20heliCamera6dof(cm,this,instrument);
        else if (strcasecmp(model_type, "M20heliCamera7dof") == 0)	// should not be used
            return new PigPointM20heliCamera(cm, this, instrument);
        else if (strcasecmp(model_type, "M20heliCamera") == 0)		// should be used
            return new PigPointM20heliCamera(cm, this, instrument);

	else if (strcasecmp(model_type, "GenericCamera_0DOF") == 0)
          return new PigPointGenericCamera_0DOF(cm, this, NULL);

	else if (strcasecmp(model_type, "M20BodyFixedCamera") == 0)
          return new PigPointM20BodyFixedCamera(cm, this, 
                                                 instrument);
    // shouldn't be here...

    sprintf(msg, "Unknown M20 pointing model: %s", model_type);
    PigModelBase::printStaticMsg(msg, PigMsgFatal);

    return NULL;
}

////////////////////////////////////////////////////////////////////////
// Create a M20 radiometry model given an image file.
////////////////////////////////////////////////////////////////////////

RadiometryModel *PigM20::createRadiometryModel(PigFileModel *file)
{
    return RadiometryM20::create(file);
}

////////////////////////////////////////////////////////////////////////
// Create a M20 color model given an image file.
////////////////////////////////////////////////////////////////////////

PigColorModel *PigM20::createColorModel(PigFileModel *file)
{
    return PigColorModelM20::create(file);
}

////////////////////////////////////////////////////////////////////////
// Create a M20 LabelModel.
////////////////////////////////////////////////////////////////////////

PigLabelModel *PigM20::createLabelModel(int unit)
{
    return new PigLabelModelM20(unit, getMissionName());
}

////////////////////////////////////////////////////////////////////////
// Create a M20 FileModel.
////////////////////////////////////////////////////////////////////////

PigFileModel *PigM20::createFileModel(const char *filename, int unit)
{
    return new PigFileModelM20(filename, unit, getMissionName());
}

////////////////////////////////////////////////////////////////////////
// Return the borders appropriate for the instrument type.
// This should only be called by PigFileModel!!
//!!!! This really needs to be set per instrument..... !!!!
////////////////////////////////////////////////////////////////////////

int PigM20::getBorders(PigFileModel *file, int &sl, int &ss,
                                           int &el, int &es)
{
    sl = 3;
    ss = 3;
    el = sl + file->getNL() - 6;
    es = ss + file->getNS() - 6;

    const char *inst = file->getInstrumentId();

    // For Mastcam-Z full frame there are 23 dark pixels, 1608 photosensitive
    // ones, and 17 more darks.  To this, we add 2 to eliminate the worst of
    // the Malvar zipper edge effects.  These are all based on the full sensor,
    // so we have to look at the subframe (and downsample).  Because ZCAM has no
    // downsampling, if it's anything other than 1 we assume it's a thumb and
    // don't adjust the borders.

    if (inst != NULL && strncasecmp(inst, "MCZ", 3) == 0) {
	if (file->getDownsampleXFactor(1.0) == 1.0 &&
		file->getDownsampleYFactor(1.0) == 1.0) {

	    // Remember first_* is 1-based but ss is 0-based
	    int first_samp = file->getFirstLineSample(1);
	    if (first_samp <= 25) {
		ss = (25 - (first_samp-1));
	    }

	    // last 1-based good pixel is 1680_23 = 1631.  Sub 2 for zipper.

	    int last_samp = first_samp + file->getNS() - 1;
	    if (last_samp > 1629) {
		es = file->getNS() - (last_samp-1629);
	    }
	}
    }

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
// Note that 2020 is the first mission where the instrument matters
// (the inst frame for PIXL MCC is PIXL_SENSOR).
// Return 1 for success, 0 for frame not recognized (in which case it's
// as if FIXED was given)
// mask_indices (output) will contain 1 for relevant indices and -1 for 
// those that should be wildcarded.  Valid pointer must be provided, or
// NULL for dont-care.  Mask will be filled up to max_indices.
////////////////////////////////////////////////////////////////////////

int PigM20::canonicalizeFrameName(const char *frame, const char *inst_id,
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

	canon_frame = "RSM_HEAD_FRAME";
	short_frame = "RSM";
	max_indices = 10;
	if (mask_indices) {
	    mask_indices[0] = 1;
	    mask_indices[1] = 1;
	    mask_indices[2] = 1;
	    mask_indices[3] = -1;	// ARM
	    mask_indices[4] = -1;	// SHA
	    mask_indices[5] = -1;	// DRILL
	    mask_indices[6] = 1;	// RSM
	    mask_indices[7] = -1;	// HGA
	    mask_indices[8] = -1;	// BITCAR
	    mask_indices[9] = -1;	// SEAL
	}
	return 1;
    }

    if ((strcasecmp(frame, "TURRET") == 0) ||
        (strcasecmp(frame, "TURRET_FRAME") == 0) ||
        (strcasecmp(frame, "ARM_TURRET_FRAME") == 0)) {

	canon_frame = "ARM_TURRET_FRAME";
	short_frame = "TURRET";
	max_indices = 10;
	if (mask_indices) {
	    mask_indices[0] = 1;
	    mask_indices[1] = 1;
	    mask_indices[2] = 1;
	    mask_indices[3] = 1;	// ARM
	    mask_indices[4] = -1;	// SHA
	    mask_indices[5] = -1;	// DRILL
	    mask_indices[6] = -1;	// RSM
	    mask_indices[7] = -1;	// HGA
	    mask_indices[8] = -1;	// BITCAR
	    mask_indices[9] = -1;	// SEAL
	}
	return 1;
    }

    if ((strcasecmp(frame, "DRILL") == 0) ||
        (strcasecmp(frame, "DRILL_FRAME") == 0) ||
        (strcasecmp(frame, "ARM_DRILL_FRAME") == 0)) {

	canon_frame = "ARM_DRILL_FRAME";
	short_frame = "DRILL";
	max_indices = 10;
	if (mask_indices) {
	    mask_indices[0] = 1;
	    mask_indices[1] = 1;
	    mask_indices[2] = 1;
	    mask_indices[3] = 1;	// ARM
	    mask_indices[4] = -1;	// SHA
	    mask_indices[5] = -1;	// DRILL
	    mask_indices[6] = -1;	// RSM
	    mask_indices[7] = -1;	// HGA
	    mask_indices[8] = -1;	// BITCAR
	    mask_indices[9] = -1;	// SEAL
	}
	return 1;
    }

    if ((strcasecmp(frame, "PIXL_TOOL") == 0) ||
        (strcasecmp(frame, "ARM_PIXL_FRAME") == 0) ||
        (strcasecmp(frame, "PIXL_TOOL_FRAME") == 0)) {

	canon_frame = "ARM_PIXL_FRAME";
	short_frame = "ARM_PIXL";
	max_indices = 10;
	if (mask_indices) {
	    mask_indices[0] = 1;
	    mask_indices[1] = 1;
	    mask_indices[2] = 1;
	    mask_indices[3] = 1;	// ARM
	    mask_indices[4] = -1;	// SHA
	    mask_indices[5] = -1;	// DRILL
	    mask_indices[6] = -1;	// RSM
	    mask_indices[7] = -1;	// HGA
	    mask_indices[8] = -1;	// BITCAR
	    mask_indices[9] = -1;	// SEAL
	}
	return 1;
    }

    //!!!! ... other ARM frames as needed ...

    // PIXL-specific frames

    if ((strcasecmp(frame, "PIXL_BASE") == 0) ||
        (strcasecmp(frame, "PIXL_BASE_FRAME") == 0)) {

	canon_frame = "PIXL_BASE_FRAME";
	short_frame = "PIXL_BASE";
	max_indices = 10;
	if (mask_indices) {
	    mask_indices[0] = 1;
	    mask_indices[1] = 1;
	    mask_indices[2] = 1;
	    mask_indices[3] = 1;	// ARM
	    mask_indices[4] = -1;	// SHA
	    mask_indices[5] = -1;	// DRILL
	    mask_indices[6] = -1;	// RSM
	    mask_indices[7] = -1;	// HGA
	    mask_indices[8] = -1;	// BITCAR
	    mask_indices[9] = -1;	// SEAL
	}
	return 1;
    }

    // Note that PIXL SENSOR has a 12-element RMC...

    if ((strcasecmp(frame, "PIXL_SENSOR") == 0) ||
        (strcasecmp(frame, "PIXL_SENSOR_FRAME") == 0)) {

	canon_frame = "PIXL_SENSOR_FRAME";
	short_frame = "PIXL_SENSOR";
	max_indices = 12;
	if (mask_indices) {
	    mask_indices[0] = 1;
	    mask_indices[1] = 1;
	    mask_indices[2] = 1;
	    mask_indices[3] = 1;	// ARM
	    mask_indices[4] = -1;	// SHA
	    mask_indices[5] = -1;	// DRILL
	    mask_indices[6] = -1;	// RSM
	    mask_indices[7] = -1;	// HGA
	    mask_indices[8] = -1;	// BITCAR
	    mask_indices[9] = -1;	// SEAL
	    mask_indices[10] = 1;	// RTT
	    mask_indices[11] = 1;	// PMC
	}
	return 1;
    }

    if ((strcasecmp(frame, "DOCKING_POST") == 0) ||
        (strcasecmp(frame, "DOCKING_POST_FRAME") == 0) ||
        (strcasecmp(frame, "ARM_DOCKING_POST_FRAME") == 0)) {

	canon_frame = "ARM_DOCKING_POST_FRAME";
	short_frame = "DOCKING_POST";
	max_indices = 10;
	if (mask_indices) {
	    mask_indices[0] = 1;
	    mask_indices[1] = 1;
	    mask_indices[2] = 1;
	    mask_indices[3] = 1;	// ARM
	    mask_indices[4] = -1;	// SHA
	    mask_indices[5] = -1;	// DRILL
	    mask_indices[6] = -1;	// RSM
	    mask_indices[7] = -1;	// HGA
	    mask_indices[8] = -1;	// BITCAR
	    mask_indices[9] = -1;	// SEAL
	}
	return 1;
    }

    if ((strcasecmp(frame, "GDRT") == 0) ||
        (strcasecmp(frame, "GDRT_FRAME") == 0) ||
        (strcasecmp(frame, "ARM_GDRT_FRAME") == 0)) {

	canon_frame = "ARM_GDRT_FRAME";
	short_frame = "GDRT";
	max_indices = 10;
	if (mask_indices) {
	    mask_indices[0] = 1;
	    mask_indices[1] = 1;
	    mask_indices[2] = 1;
	    mask_indices[3] = 1;	// ARM
	    mask_indices[4] = -1;	// SHA
	    mask_indices[5] = -1;	// DRILL
	    mask_indices[6] = -1;	// RSM
	    mask_indices[7] = -1;	// HGA
	    mask_indices[8] = -1;	// BITCAR
	    mask_indices[9] = -1;	// SEAL
	}
	return 1;
    }

    if ((strcasecmp(frame, "FCS") == 0) ||
        (strcasecmp(frame, "FCS_FRAME") == 0) ||
        (strcasecmp(frame, "ARM_FCS_FRAME") == 0)) {

	canon_frame = "ARM_FCS_FRAME";
	short_frame = "FCS";
	max_indices = 10;
	if (mask_indices) {
	    mask_indices[0] = 1;
	    mask_indices[1] = 1;
	    mask_indices[2] = 1;
	    mask_indices[3] = 1;	// ARM
	    mask_indices[4] = -1;	// SHA
	    mask_indices[5] = -1;	// DRILL
	    mask_indices[6] = -1;	// RSM
	    mask_indices[7] = -1;	// HGA
	    mask_indices[8] = -1;	// BITCAR
	    mask_indices[9] = -1;	// SEAL
	}
	return 1;
    }

    if ((strcasecmp(frame, "WATSON") == 0) ||
        (strcasecmp(frame, "WATSON_FRAME") == 0) ||
        (strcasecmp(frame, "ARM_WATSON_FRAME") == 0)) {

	canon_frame = "ARM_WATSON_FRAME";
	short_frame = "WATSON";
	max_indices = 10;
	if (mask_indices) {
	    mask_indices[0] = 1;
	    mask_indices[1] = 1;
	    mask_indices[2] = 1;
	    mask_indices[3] = 1;	// ARM
	    mask_indices[4] = -1;	// SHA
	    mask_indices[5] = -1;	// DRILL
	    mask_indices[6] = -1;	// RSM
	    mask_indices[7] = -1;	// HGA
	    mask_indices[8] = -1;	// BITCAR
	    mask_indices[9] = -1;	// SEAL
	}
	return 1;
    }

    if ((strcasecmp(frame, "SHERLOC") == 0) ||
        (strcasecmp(frame, "SHERLOC_FRAME") == 0) ||
        (strcasecmp(frame, "ARM_SHERLOC_FRAME") == 0)) {

	canon_frame = "ARM_SHERLOC_FRAME";
	short_frame = "SHERLOC";
	max_indices = 10;
	if (mask_indices) {
	    mask_indices[0] = 1;
	    mask_indices[1] = 1;
	    mask_indices[2] = 1;
	    mask_indices[3] = 1;	// ARM
	    mask_indices[4] = -1;	// SHA
	    mask_indices[5] = -1;	// DRILL
	    mask_indices[6] = -1;	// RSM
	    mask_indices[7] = -1;	// HGA
	    mask_indices[8] = -1;	// BITCAR
	    mask_indices[9] = -1;	// SEAL
	}
	return 1;
    }

    if ((strcasecmp(frame, "CUSTOM_TCP") == 0) ||
        (strcasecmp(frame, "CUSTOM_TCP_FRAME") == 0) ||
        (strcasecmp(frame, "ARM_CUSTOM_TCP_FRAME") == 0)) {

	canon_frame = "ARM_CUSTOM_TCP_FRAME";
	short_frame = "CUSTOM_TCP";
	max_indices = 10;
	if (mask_indices) {
	    mask_indices[0] = 1;
	    mask_indices[1] = 1;
	    mask_indices[2] = 1;
	    mask_indices[3] = 1;	// ARM
	    mask_indices[4] = -1;	// SHA
	    mask_indices[5] = -1;	// DRILL
	    mask_indices[6] = -1;	// RSM
	    mask_indices[7] = -1;	// HGA
	    mask_indices[8] = -1;	// BITCAR
	    mask_indices[9] = -1;	// SEAL
	}
	return 1;
    }

    if ((strcasecmp(frame, "HELI_G") == 0) ||
        (strcasecmp(frame, "HELI_G_FRAME") == 0)) {

	canon_frame = "HELI_G_FRAME";
	short_frame = "HELI_G";
	max_indices = 1;
	if (mask_indices) {
	    mask_indices[0] = 1;
	}
	return 1;
    }

    if ((strcasecmp(frame, "HELI_M") == 0) ||
        (strcasecmp(frame, "HELI_M_FRAME") == 0)) {

	canon_frame = "HELI_M_FRAME";
	short_frame = "HELI_M";
	max_indices = 2;
	if (mask_indices) {
	    mask_indices[0] = 1;
	    mask_indices[1] = 1;
	}
	return 1;
    }

    if ((strcasecmp(frame, "HELI_S1") == 0) ||
        (strcasecmp(frame, "HELI_S1_FRAME") == 0)) {

	canon_frame = "HELI_S1_FRAME";
	short_frame = "HELI_S1";
	max_indices = 2;
	if (mask_indices) {
	    mask_indices[0] = 1;
	    mask_indices[1] = 1;
	}
	return 1;
    }

    if ((strcasecmp(frame, "HELI_S2") == 0) ||
        (strcasecmp(frame, "HELI_S2_FRAME") == 0)) {

	canon_frame = "HELI_S2_FRAME";
	short_frame = "HELI_S2";
	max_indices = 2;
	if (mask_indices) {
	    mask_indices[0] = 1;
	    mask_indices[1] = 1;
	}
	return 1;
    }

    // LVS frames... CINT is attached to camera

    if ((strcasecmp(frame, "CINT") == 0) ||
        (strcasecmp(frame, "CINT_FRAME") == 0)) {

	canon_frame = "CINT_FRAME";
	short_frame = "CINT";
	max_indices = 2;
	if (mask_indices) {
	    mask_indices[0] = 1;
	    mask_indices[1] = 1;
	}
	return 1;
    }

    // LVS frames... MCMF is measured from the center (!) of Mars

    if ((strcasecmp(frame, "MCMF") == 0) ||
        (strcasecmp(frame, "MCMF_FRAME") == 0) ||
        (strcasecmp(frame, "MARS_CENTERED_MARS_FIXED_FRAME") == 0)) {

	canon_frame = "MCMF_FRAME";
	short_frame = "MCMF";
	max_indices = 1;
	if (mask_indices) {
	    mask_indices[0] = 1;
	}
	return 1;
    }

    char msg[256];
    sprintf(msg, "Unknown M20 coordinate system frame: '%s', Ignored", frame);
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

const char *PigM20::getRmcIndexName(int i)
{
    if (i == 0)  return "SITE";
    if (i == 1)  return "DRIVE";
    if (i == 2)  return "POSE";
    if (i == 3)  return "ARM";
    if (i == 4)  return "SHA";
    if (i == 5)  return "DRILL";
    if (i == 6)  return "RSM";
    if (i == 7)  return "HGA";
    if (i == 8)  return "BITCAR";
    if (i == 9)  return "SEAL";
    if (i == 10) return "RTT";
    if (i == 11) return "PMC";

    return "UNKNOWN";	// oops!
}

///////////////////////////////////////////////////////////////////////
// Add constant coordinate systems related to this one to the RSM
// database.  For 2020, constant coord systems are:
// RMECH - constant child of RNAV
// PIXL_BASE - constant child of ARM_PIXL
// LOCAL_LEVEL - constant peer or RNAV
// Note that the csdefs are merely put in as potential CS's.  The CS
// is not actually instantiated unless asked for.
////////////////////////////////////////////////////////////////////////

void PigM20::addConstantCoordSystems(PigRoverStateManager *rsm,
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

	PigVector rmech_offset = PigVector(M20_RMECH_TO_RNAV_OFFSET_X,
					   M20_RMECH_TO_RNAV_OFFSET_Y,
					   M20_RMECH_TO_RNAV_OFFSET_Z);

	PigQuaternion rmech_q;

	PigCSDefinition *rmech = new PigCSDefinition(this, child, ident,
				rmech_offset, rmech_q);

	// addSolution will only add it if it's unique

	rsm->addSolution(rmech);
	delete child;
	delete rmech;
    }

    if (strcmp(parent, "ARM_PIXL_FRAME") == 0) {
	PigCSReference *child = new PigCSReference(
		this,
		"PIXL_BASE_FRAME",
		ident->getSolutionId(),
		ident->getIndices(),
		ident->getNumIndices(),
		ident->getInstId());

	PigVector pbase_offset = PigVector(M20_PIXL_TOOL_TO_BASE_OFFSET_X,
					   M20_PIXL_TOOL_TO_BASE_OFFSET_Y,
					   M20_PIXL_TOOL_TO_BASE_OFFSET_Z);
	PigQuaternion pbase_quat =
		PigQuaternion(PigVector(0,0,1), M20_PIXL_BASE_TO_TOOL_YAW) *
		PigQuaternion(PigVector(1,0,0), M20_PIXL_BASE_TO_TOOL_ROLL);

	PigCSDefinition *pbase = new PigCSDefinition(this, child, ident,
				pbase_offset, pbase_quat);

	// addSolution will only add it if it's unique

	rsm->addSolution(pbase);
	delete child;
	delete pbase;
    }
}


////////////////////////////////////////////////////////////////////////
// Get the PigRoverStateManager to use for this mission.  It is a singleton
// per mission.
////////////////////////////////////////////////////////////////////////

PigRoverStateManager *PigM20::getRoverStateManager()
{
    if (_m20_rsm == NULL) {
        _m20_rsm = new PigRoverStateManagerM20(this);
        // Add "telemetry" as the lowest priority...
        _m20_rsm->addPriority("telemetry");
    }
    return _m20_rsm;
}
