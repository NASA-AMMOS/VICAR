////////////////////////////////////////////////////////////////////////
// RadiometryFIDO
//
// Subclass for FIDO Cameras Radiometry Model.  
// Responsible for maintaining calibration information for a camera 
// and applying radiometric correction when requested.
//
//
////////////////////////////////////////////////////////////////////////

#include "RadiometryFIDO.h"

#include "PigFileModel.h"

#include "zvproto.h"
#include "applic.h"			/* for SUCCESS */

#include <stdlib.h>

float RadiometryFIDO::_flat[FLAT_NL_FIDO][FLAT_NS_FIDO];
char RadiometryFIDO::_flat_tag[256];
int RadiometryFIDO::_flat_valid = FALSE;

////////////////////////////////////////////////////////////////////////
// Constructor
////////////////////////////////////////////////////////////////////////

RadiometryFIDO::RadiometryFIDO(const char *mission,
				 const char *instrument,
				 int filter, float exptime, 
				 float temperature,
				 int sl, int ss, int el, int es)
                 : RadiometryModel(mission, instrument, sl, ss, el, es)

{
    _filter = filter;
    _exptime = exptime;
    _temperature = temperature;
}

RadiometryFIDO::RadiometryFIDO() : RadiometryModel()
{
    _filter = 0;
    _exptime = 1.0;
    _temperature = 0.0;
}

////////////////////////////////////////////////////////////////////////
// Destructor
////////////////////////////////////////////////////////////////////////

RadiometryFIDO::~RadiometryFIDO()
{
}

////////////////////////////////////////////////////////////////////////
// This factory method creates and returns an instance of this
// subclass for the camera associated with the given file (determined
// by the labels).
// It could be a constructor instead, but this allows us to return NULL.
////////////////////////////////////////////////////////////////////////

RadiometryFIDO *RadiometryFIDO::create(PigFileModel *file)
{
    int sl, ss, el, es;

    sl = file->getFirstLine(0) - 1;		// convert to 0-based
    ss = file->getFirstLineSample(0) - 1;

    if (sl < 0) sl = 0; // clip if negative
    if (ss < 0) ss = 0;

    el = sl + file->getNL() - 1; // compute ending line/sample
    es = ss + file->getNS() - 1;

    // grab the observation time
    float exptime = file->getExposureDuration(1000) / 1000.;  // cvt to seconds

    // grab the CCD temperature
    float temperature = file->getInstrumentTemperature(0);

    // grab the filter number
    int filter;
    const char *filter_string = file->getFilterName();
    if (filter_string == NULL)
      filter = 0;  //Red
    else if (strcasecmp(filter_string, "RED") == 0 )
	filter = 0;
    else if (strcasecmp(filter_string, "GREEN") == 0 )
	filter = 1;
    else if (strcasecmp(filter_string, "BLUE") == 0 )
	filter = 2;
    else
      filter = 0;  //Red

    // now do the actual creation by calling the constructor
    const char *inst_id = file->getInstrumentId();
    const char *frame   = file->getFrameId();

   if (strcasecmp(inst_id, "PAN") == 0) {
     if (frame && frame[0] == 'R') {
       return new RadiometryFIDO(file->getMissionName(), "Pancam Right",
			      filter, exptime, temperature, 
			      sl, ss, el, es);
     }
     else {
       return new RadiometryFIDO(file->getMissionName(), "Pancam Left",
			      filter, exptime, temperature, 
			      sl, ss, el, es);
     }
   }

   else if (strcasecmp(inst_id, "NAV") == 0) {
     if (frame && frame[0] == 'R') {
       return new RadiometryFIDO(file->getMissionName(), "Navcam Right",
			      filter, exptime, temperature, 
			      sl, ss, el, es);
     }
     else {
       return new RadiometryFIDO(file->getMissionName(), "Navcam Left",
			      filter, exptime, temperature, 
			      sl, ss, el, es);
     }
   }

   else if (strcasecmp(inst_id, "FHZ") == 0) {
     if (frame && frame[0] == 'R') {
       return new RadiometryFIDO(file->getMissionName(), "Hazcam Front Right",
			      filter, exptime, temperature, 
			      sl, ss, el, es);
     }
     else {
       return new RadiometryFIDO(file->getMissionName(), "Hazcam Front Left",
			      filter, exptime, temperature, 
			      sl, ss, el, es);
     }
   }

   else if (strcasecmp(inst_id, "RHZ") == 0) {
     if (frame && frame[0] == 'R') {
       return new RadiometryFIDO(file->getMissionName(), "Hazcam Rear Right",
			      filter, exptime, temperature, 
			      sl, ss, el, es);
     }
     else {
       return new RadiometryFIDO(file->getMissionName(), "Hazcam Rear Left",
			      filter, exptime, temperature, 
			      sl, ss, el, es);
     }
   }

   else if (strcasecmp(inst_id, "BCM") == 0) {
     if (frame && frame[0] == 'R') {
       return new RadiometryFIDO(file->getMissionName(), "Bellycam Right",
			      filter, exptime, temperature, 
			      sl, ss, el, es);
     }
     else {
       return new RadiometryFIDO(file->getMissionName(), "Bellycam Left",
			      filter, exptime, temperature, 
			      sl, ss, el, es);
     }
   }

   else {
      PigModelBase::printStaticMsg(
                   "Unrecognized InstrumentId, Pancam assumed", PigMsgWarning);
       return new RadiometryFIDO(file->getMissionName(), "Pancam Left",
			      filter, exptime, temperature, 
			      sl, ss, el, es);
    }
      
    return NULL;		// never reached
}

////////////////////////////////////////////////////////////////////////

void RadiometryFIDO::print()

{
    char msg[256];

    RadiometryModel::print();
    sprintf(msg, "Filter Setting: %d", _filter);
    printInfo(msg);
    sprintf(msg, "Exposure time: %f", _exptime);
    printInfo(msg);
    sprintf(msg, "Temperature: %f", _temperature);
    printInfo(msg);
}

////////////////////////////////////////////////////////////////////////
// Load a flat-field image.  The image is cached in a static class variable
// so we don't re-load it on each image.
////////////////////////////////////////////////////////////////////////

int RadiometryFIDO::loadFlatField(float *&flat, int &width, int &height)
{
    int i, unit, status;
    int nl, ns;
    char *flat_field_name;
    char flat_field_path[256];
    char msg[512];
    static int inst = 1;

    flat = NULL;
    width = FLAT_NS_FIDO;
    height = FLAT_NL_FIDO;

    // Get the right file
    if (strcasecmp(_instrument, "Pancam Left") == 0) {
      if(_filter == 0)
	flat_field_name = "flat_fields/flat_field_FIDO_PAN_L_RED.vicar";
      else if(_filter == 1)
	flat_field_name = "flat_fields/flat_field_FIDO_PAN_L_GRN.vicar";
      else if(_filter == 2)
	flat_field_name = "flat_fields/flat_field_FIDO_PAN_L_BLU.vicar";
	
    }
    else if (strcasecmp(_instrument, "Pancam Right") == 0) {
      if(_filter == 0)
	flat_field_name = "flat_fields/flat_field_FIDO_PAN_R_RED.vicar";
      else if(_filter == 1)
	flat_field_name = "flat_fields/flat_field_FIDO_PAN_R_GRN.vicar";
      else if(_filter == 2)
	flat_field_name = "flat_fields/flat_field_FIDO_PAN_R_BLU.vicar";
    }
    else if (strcasecmp(_instrument, "Navcam Left") == 0) {
	flat_field_name = "flat_fields/flat_field_FIDO_NAV_L.vicar";
    }
    else if (strcasecmp(_instrument, "Navcam Right") == 0) {
	flat_field_name = "flat_fields/flat_field_FIDO_NAV_R.vicar";
    }
    else if(strcasecmp(_instrument, "Hazcam Front Left") == 0) {
	flat_field_name = "flat_fields/flat_field_FIDO_FHZ_L.vicar"; 
    }
    else if(strcasecmp(_instrument, "Hazcam Front Right") == 0) {
	flat_field_name = "flat_fields/flat_field_FIDO_FHZ_R.vicar";
    }
    else if(strcasecmp(_instrument, "Hazcam Rear Left") == 0) {
	flat_field_name = "flat_fields/flat_field_FIDO_RHZ_L.vicar";
    }
    else if(strcasecmp(_instrument, "Hazcam Rear Right") == 0) {
	flat_field_name = "flat_fields/flat_field_FIDO_RHZ_R.vicar";
    }
    else if(strcasecmp(_instrument, "Bellycam Left") == 0) {
	flat_field_name = "flat_fields/flat_field_FIDO_BCM_L.vicar";
    }
    else if(strcasecmp(_instrument, "Bellycam Right") == 0) {
	flat_field_name = "flat_fields/flat_field_FIDO_BCM_R.vicar";
    }
    else {
	sprintf(msg, "Unknown FIDO instrument %s", _instrument);
	PigModelBase::printStaticMsg(msg, PigMsgFatal);
	return FALSE;
    }

    // check and see if we need to load the flat field, which is
    // stored in the static variable _flat

    if (!_flat_valid || strcmp(_flat_tag, flat_field_name) != 0) {

	// We need a new flat field

	_flat_valid = FALSE;		// until it loads properly

	// change the tag name to reflect the active flat field
	strcpy(_flat_tag, flat_field_name);
	
	// Find the file
	status = -1;
	unit = -1;
	FILE *f = PigModelBase::openConfigFile(flat_field_name,flat_field_path);
	if (f != NULL) {
	    fclose(f);
	    sprintf(msg, "loading flat field file %s", flat_field_path);
	    printInfo(msg);

	    // re-open the flat field image using VICAR

	    zvunit(&unit, "rad_fido_flat", inst++,
				"U_NAME", flat_field_path, NULL);
	    status = zvopen(unit, "U_FORMAT", "REAL",
				"OPEN_ACT", "", "IO_ACT", "S", NULL);
	}

      	if (status != SUCCESS) {
            sprintf(msg, "Unable to open flat field file %s", flat_field_name);
	    printError(msg);
            printWarning("No flat field correction applied");
	    if (unit != -1)
		zvclose(unit, "clos_act", "free", NULL);
	    return FALSE;
        }

	zvget(unit, "NS", &ns, "NL", &nl, NULL);
	if (nl != FLAT_NL_FIDO || ns != FLAT_NS_FIDO) {
	    sprintf(msg, "Flat field file must be %d lines by %d samples",
				FLAT_NL_FIDO, FLAT_NS_FIDO);
	    printError(msg);
	    printWarning("No flat field correction applied");
	    zvclose(unit, "clos_act", "free", NULL);
	    return FALSE;
	}

    	for (i = 0; i < FLAT_NL_FIDO; i++) {
	    status = zvread(unit, &_flat[i][0], NULL);
      	    if (status != SUCCESS) {
            	sprintf(msg, "I/O error reading flat field file %s",
			flat_field_path);
		printError(msg);
		printWarning("No flat field correction applied");
	        zvclose(unit, "clos_act", "free", NULL);
		return FALSE;
	    }
	}

	zvclose(unit, "clos_act", "free", NULL);
	_flat_valid = TRUE;
    }

    flat = (float *)_flat;
    return TRUE;
}

////////////////////////////////////////////////////////////////////////
// Return the responsivity coefficient given the temperature and exposure time
// !!!! FUNCTION???? !!!!
// This coefficient is divided into the input DN to give a number with the
// units watts/(meter**2,steradian,micron).
//
////////////////////////////////////////////////////////////////////////

double RadiometryFIDO::getResponsivity(int band)
{
  // We don't have any temperature response curves or radiometric 
  // calibration information.
    return 1.0;
}

