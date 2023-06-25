////////////////////////////////////////////////////////////////////////
// RadiometryMER
//
// Subclass for MER Cameras Radiometry Model.  
// Responsible for maintaining calibration information for a camera 
// and applying radiometric correction when requested.
//
//
////////////////////////////////////////////////////////////////////////

#include "RadiometryMER.h"
#include "PigFileModel.h"

#include "zvproto.h"
#include "applic.h"			/* for SUCCESS */

#include "PigXerces.h"
#include "PigCameraMapper.h"
#include "PigCameraMapEntry.h"

#include <stdlib.h>

 float RadiometryMER::_flat[FLAT_NL_MER][FLAT_NS_MER];
 char RadiometryMER::_flat_tag[256];
 int RadiometryMER::_flat_valid = FALSE;

////////////////////////////////////////////////////////////////////////
// Constructor
////////////////////////////////////////////////////////////////////////

RadiometryMER::RadiometryMER(const char *mission,
			     const char *host_id,
			     const char *instrument,
			     const char *filter,
			     const char *isFFonBoardApplied,
			     float *flat_parms,
			     float exptime, 
			     float temperature,
			     int sl, int ss, int el, int es,
	                     int hscale, int vscale,
                             float solar_elevation, float tau)
                 : RadiometryModel(mission, instrument, sl, ss, el, es)

{
    strcpy(_host_id, host_id);
    strcpy(_instrument, instrument);
    strcpy(_filter, filter);
    _exptime = exptime;
    _temperature = temperature;
    if (strncmp(isFFonBoardApplied, "TRUE", 4)) {
         // no on-board correction has been applied
        _isFFonBoardApplied = 0;
	_flat_parms[0] = _flat_parms[1] = _flat_parms[2] = 
                         _flat_parms[3] = _flat_parms[4] = 0.0;
    }
    else {
        _isFFonBoardApplied = 1;
	_flat_parms[0] = flat_parms[0];
	_flat_parms[1] = flat_parms[1];
	_flat_parms[2] = flat_parms[2];
	_flat_parms[3] = flat_parms[3];
	_flat_parms[4] = flat_parms[4];
    }
    _hscale = hscale;
    _vscale = vscale;

    _solar_elevation = solar_elevation;
    _tau = tau;

    read_flat_field_parms("param_files/MER_flat_fields.parms");
}

RadiometryMER::RadiometryMER() : RadiometryModel()
{
    strcpy(_host_id,"");
    strcpy(_instrument, "");
    strcpy(_filter, "");
    _isFFonBoardApplied = 0;
    _exptime = 1.0;
    _temperature = 0.0;
    _hscale = _vscale = 1;
    _flat_parms[0] = _flat_parms[1] = _flat_parms[2] = 
                     _flat_parms[3] = _flat_parms[4] = 0.0;
    _solar_elevation = 5.0;
    _tau = 0.6;
}

////////////////////////////////////////////////////////////////////////
// Destructor
////////////////////////////////////////////////////////////////////////

RadiometryMER::~RadiometryMER()
{
}

////////////////////////////////////////////////////////////////////////
// This factory method creates and returns an instance of this
// subclass for the camera associated with the given file (determined
// by the labels).
// It could be a constructor instead, but this allows us to return NULL.
////////////////////////////////////////////////////////////////////////

RadiometryMER *RadiometryMER::create(PigFileModel *file)
{
    // Get Solar elevation value from the label, and if it is specified
    // with POINT_METHOD, overwrite the value retrieved from the label.
    char msg[256];
    char point_method[256], *value;
    int count;
    float solar_elevation_deg = file->getSolarElevation(-10000.0);

    float min_solar_elevation_deg = 5.0;
    getStaticParam("POINT_METHOD", point_method, &count, 1, 0);
    if (count != 0) {
        value = parseParamString(point_method, "SOLAR_MIN_ELEV");

        if (value != NULL) {
            min_solar_elevation_deg = atof(value);
        }
    }

    if (solar_elevation_deg < min_solar_elevation_deg) {
        solar_elevation_deg = min_solar_elevation_deg;
        sprintf(msg, "Solar elevation is reset to the minimum value %f",
                min_solar_elevation_deg);
        printStaticMsg(msg, PigMsgInfo);
    }

    float solar_elevation = PigDeg2Rad(solar_elevation_deg);

    // Get TAU value from the parameter
    double tau;
    getStaticParam("TAU", &tau, &count, 1, 0);
    if (count == 0) {
        tau = 0.6;
    }

    int sl, ss, el, es;

    sl = file->getFirstLine(0) - 1;		// convert to 0-based
    ss = file->getFirstLineSample(0) - 1;

    if (sl < 0) sl = 0; // clip if negative
    if (ss < 0) ss = 0;

    el = sl + file->getNL() - 1; // compute ending line/sample
    es = ss + file->getNS() - 1;

    // for downsampled images
    int hscale = (int)(file->getDownsampleXFactor(1.0));
    int vscale = (int)(file->getDownsampleYFactor(1.0));

    float flat_parms[5];
    int num_parms = PIG_MAX_FLAT_FIELD_INDEX;
    flat_parms[0] = flat_parms[1] = flat_parms[2] = 
                    flat_parms[3] = flat_parms[4] = 0.0;
    file->getFlatFieldCorrectionCounter(flat_parms, num_parms);
    // now do the actual creation by calling the constructor
    return new RadiometryMER(file->getMissionName(),
			     file->getInstrumentHostId(),
			     file->getInstrumentId(),
			     file->getFilterNumber(),
   	                     file->getFlatFieldCorrectionFlag(),
			     flat_parms,
    /*convert to seconds*/   file->getExposureDuration(1000.0) / 1000.0,
			     file->getInstrumentTemperature(0),
			     sl, ss, el, es, hscale, vscale,
                             solar_elevation, tau);
}

////////////////////////////////////////////////////////////////////////

void RadiometryMER::print()

{
    char msg[256];

    RadiometryModel::print();
    sprintf(msg, "Filter Setting: %s", _filter);
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

int RadiometryMER::loadFlatField(float *&flat, int &width, int &height)
{
    int nl, ns;
    char flat_field_path[PIG_MAX_FILENAME_SIZE];
    char flat_field_file[PIG_MAX_FILENAME_SIZE];
    char msg[512];
    static int inst = 1;

    flat = NULL;
    width = FLAT_NS_MER;
    height = FLAT_NL_MER;

    // Find out the name of the flat field file
    PigXerces::initialize();
    PigCameraMapper *map = new PigCameraMapper(NULL, _host_id);
    PigCameraMapEntry *entry = NULL;

    if (_instrument)
        entry = map->findFromID(_instrument);

    if (entry) {
      strcpy(flat_field_file, "flat_fields/MER_FLAT");
      if (entry->getSerialNumber()) {
	strcat(flat_field_file, "_SN_");
        strcat(flat_field_file, entry->getSerialNumber());
      }
    if (entry->getFilters()) {
        strcat(flat_field_file, "_F_");
        strcat(flat_field_file, _filter);
    }
        strcat(flat_field_file, ".");
        strcat(flat_field_file, "IMG");
    }
     
    // check and see if we need to load the flat field, which is
    // stored in the static variable _flat

    if (!_flat_valid || strcmp(_flat_tag, flat_field_file) != 0) {

	// We need a new flat field

	_flat_valid = FALSE;		// until it loads properly

	// change the tag name to reflect the active flat field
	strcpy(_flat_tag, flat_field_file);
	
	// Find the file
	int status = -1;
	int unit = -1;
	FILE *f = PigModelBase::openConfigFile(flat_field_file,
					       flat_field_path);
	if (f != NULL) {
	    fclose(f);
	    sprintf(msg, "loading flat field file %s", flat_field_path);
	    printInfo(msg);

	    // re-open the flat field image using VICAR

	    zvunit(&unit, "rad_mer_flat", inst++,
				"U_NAME", flat_field_path, NULL);
	    status = zvopen(unit, "U_FORMAT", "REAL",
				"OPEN_ACT", "", "IO_ACT", "S", NULL);
	}

      	if (status != SUCCESS) {
            sprintf(msg, "Unable to open flat field file %s", flat_field_file);
	    printError(msg);
            printWarning("No flat field correction applied");
	    if (unit != -1)
		zvclose(unit, "clos_act", "free", NULL);
	    return FALSE;
        }

	zvget(unit, "NS", &ns, "NL", &nl, NULL);
	if (nl != FLAT_NL_MER || ns != FLAT_NS_MER) {
	    sprintf(msg, "Flat field file must be %d lines by %d samples",
				FLAT_NL_MER, FLAT_NS_MER);
	    printError(msg);
	    printWarning("No flat field correction applied");
	    zvclose(unit, "clos_act", "free", NULL);
	    return FALSE;
	}

    	for (int i = 0; i < FLAT_NL_MER; i++) {
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

// Assumes "image_*", and "max_ns" are local variables
#define IMAGE(line, samp) (*((image_int) + (line) * max_ns + (samp)))
#define IMAGEF(line, samp) (*((image_float) + (line) * max_ns + (samp)))

////////////////////////////////////////////////////////////////////////
// Apply the correction to an image.  max_nl and max_ns represent the
// physical size of the buffer, not the logical extent.  The _sl, _ss,
// _el, and _es member variables define where this physical buffer lies
// in the logical image.  So, an _sl of 2 means that the first line of
// the physical buffer corresponds to the second line of the flat field.
////////////////////////////////////////////////////////////////////////
void RadiometryMER::applyCorrectionInternal(void *image, int max_nl, int max_ns,
					int is_float, int band, int hasBeenRad)
{
    float *flat;
    int flat_width, flat_height;
    int i, j;
    double dn;
    double scale_factor = 1;

    short int *image_int = (short int *)image;
    float *image_float = (float *)image;

    // Get values from subclasses

    int use_flat = loadFlatField(flat, flat_width, flat_height);
    double responsivity = getResponsivity(band);
    double dnscale = getDnScalingFactor();
    double dnoff = getDnScalingOffset();

    double zenith_factor = 1.0;
    if (doScaledRad()) {
        zenith_factor = getZenithFactor();
        if (zenith_factor == 0.0) {
            zenith_factor = 1.0;
        }
    }

    for (i=0; i < (_el - _sl + 1); i++) {
	for (j=0; j < (_es - _ss + 1); j++) {
	    if (_isFFonBoardApplied) {
	        // First undo on-board rad. correction
		register double degrees = (j-_flat_parms[0])*(j-_flat_parms[0]) + (i-_flat_parms[1])*(i-_flat_parms[1]);
		scale_factor = 1 + _flat_parms[2]*degrees + 
		                   _flat_parms[3]*degrees*degrees + 
	                           _flat_parms[4]*degrees*degrees*degrees;
	    }
	        
	    if (is_float)
		dn = IMAGEF(i,j);
	    else
		dn = IMAGE(i,j);

	    // if flat field correction
            if (!hasBeenRad) {
	        if (use_flat && i < flat_height && getFlatFieldDN(flat, i, j) > 0) {
	            // apply scale_factor to undo on-board flat-field correction
		    dn = (dn / scale_factor) / (getFlatFieldDN(flat, i, j) * responsivity);
	        } else {
		    dn = (dn / scale_factor) / responsivity;
                }
            }

            // Apply scaled radiance factor.  If zenith is off, the
            // factor is 1.0, so no need for a conditional

            dn = dn / zenith_factor;

	    // Apply scaling (should be 0 and 1 for floats, but it's
	    // applied anyway to make sure labels are consistent))

	    dn = (dn - dnoff) / dnscale;

	    // Round to short int and make sure it is within range
	    // and store back into the image

	    if (is_float) {
	        IMAGEF(i, j) = (float)dn;
	    }
	    else {
		dn += 0.5;		// round
		if (dn > 32767)		// clip
		    dn = 32767;
		if (dn < 0)
		    dn = 0;
		short int short_dn = (short int)dn;

	        IMAGE(i, j) = short_dn;
	    }
	}
    }

    freeFlatField(flat);

}

// Assumes "flat" is a local variable,and _sl/_ss/_el/_es are member variables.
#define FLAT(line, samp) (*((flat) + ((line)+_sl) * FLAT_NS_MER + ((samp)+_ss)))
// return flat field's dn value for a given sample/line pair
float RadiometryMER::getFlatFieldDN(float *flat, int line, int samp)
{
  float newDN = 0.0;
  for (int i = 0; i < _hscale; i++) {
      for (int j = 0; j < _vscale; j++) {
	  newDN = newDN + FLAT(line*_vscale + j, samp*_hscale + i);
      }
  }
  return newDN/(_hscale*_vscale);
}
////////////////////////////////////////////////////////////////////////
// Return the responsivity coefficient given the temperature and exposure 
// time.
// This coefficient is divided into the input DN to give a number with the
// units watts/(meter**2,steradian,micron).
//
// According to Justin Maki, Jim Bell has listed the Pancam responsivities
// as the *inverse* of the way it was done for MPF and other missions 
// prior to MER.  Since we want to keep API the same for all mission and
// all other missions divide by responsivity we are returning here 1/x -
// an inverse responsivity.  That way in calculating rad-corrected value
// we *multiply* by the responsivity and *divide* by the exposure time.
////////////////////////////////////////////////////////////////////////

double RadiometryMER::getResponsivityFactor(int band)
{
    return (_resp[0] + _resp[1]*_temperature + 
		     _resp[2]*_temperature*_temperature);
}

double RadiometryMER::getResponsivity(int band)
{   
    return _exptime / getResponsivityFactor(band);
}



////////////////////////////////////////////////////////////////////////
// Read in the calibration pointing parameters for a given "point" file.
////////////////////////////////////////////////////////////////////////

void RadiometryMER::read_flat_field_parms(char *filename)
{
    FILE *inClientFile;
    char line[255];
    char responsivity_parms[100];
    char flat_darkening_factor[200];
    char msg[256];

    // Assign default values
    // Currently have only one set of values used by Pancam team.
    // Should be updated once we get values for all cameras.
    _resp[0] = 2.77E-07;
    _resp[1] = -3.04E-11;
    _resp[2] = 0.0;

    strcpy(responsivity_parms, "\0");
    strcpy(flat_darkening_factor, "\0");

    // open the file
    inClientFile = PigModelBase::openConfigFile(filename, NULL);

    if (inClientFile == NULL) {
        sprintf(line, 
		"Flat field parameters file %s could not be opened, using default values",
		filename);
	printWarning(line);
    }
    else {
        // Find out the serial number
        PigXerces::initialize();
	PigCameraMapper *map = new PigCameraMapper(NULL, _host_id);
	PigCameraMapEntry *entry = NULL;
	if (_instrument)
            entry = map->findFromID(_instrument);
	if (entry) {
	    if (entry->getSerialNumber()) {
	        strcat(responsivity_parms, "SN_");
		strcat(responsivity_parms, entry->getSerialNumber());
                strcat(flat_darkening_factor, "SN_");
                strcat(flat_darkening_factor, entry->getSerialNumber());
	    }
	    if (entry->getFilters()) {
	        strcat(responsivity_parms, "_F_");
	        strcat(responsivity_parms, _filter);
                strcat(flat_darkening_factor, "_F_");
                strcat(flat_darkening_factor, _filter);
	    }
	    strcat(responsivity_parms, "_responsivity = %lf %lf %lf");
            strcat(flat_darkening_factor, "_flat_darkening_factor = %lf");
	}
	  
        short int found = 0;
        short int found_flat_darkening_factor = 0;
	while (fgets(line, sizeof(line), inClientFile) != NULL) {
	    if (strncasecmp(line, responsivity_parms, 10) == 0) {
	        found = 1;
	        sscanf(line, responsivity_parms,
		       &_resp[0], &_resp[1], &_resp[2]);
	    }
            if (strncasecmp(line, flat_darkening_factor, 29) == 0) {
                    found_flat_darkening_factor = 1;
                    sscanf(line, flat_darkening_factor, &_flat_darkening_factor);
            }
	}
	fclose(inClientFile);
	// report if no data found
	if (!found) {
	    sprintf(line, 
		    "Responsivity parameters were not found in %s, using default values!",
		    filename);
	    printWarning(line);
	}
   
        if (!found_flat_darkening_factor) {
            sprintf(msg, "Flat darkening factor is not found in %s, using the "
                   "default value.", filename);
            PigModelBase::printUniqueStaticMsg(msg, PigMsgInfo);
            _flat_darkening_factor = 1.0;
        }
        sprintf(msg, "Flat darkening factor used is %g.", _flat_darkening_factor);
        PigModelBase::printUniqueStaticMsg(msg, PigMsgInfo);
    }
}
