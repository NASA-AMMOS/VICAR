////////////////////////////////////////////////////////////////////////
// RadiometryNSYT
//
// Subclass for NSYT Cameras Radiometry Model.
// Responsible for maintaining calibration information for a camera 
// and applying radiometric correction when requested.
////////////////////////////////////////////////////////////////////////

#include "RadiometryNSYT.h"
#include "PigFileModel.h"
#include "PigMission.h"

#include "zvproto.h"
#include "applic.h"			/* for SUCCESS */

#include "PigXerces.h"
#include "PigCameraMapper.h"
#include "PigCameraMapEntry.h"

#include <stdlib.h>

RadiometryCalImage *RadiometryNSYT::_flat_cache[NSYT_MAX_FLAT_CACHE];
char *RadiometryNSYT::_flat_cache_names[NSYT_MAX_FLAT_CACHE];
int RadiometryNSYT::_flat_cache_init = 0;

////////////////////////////////////////////////////////////////////////
// Constructor
////////////////////////////////////////////////////////////////////////

RadiometryNSYT::RadiometryNSYT(const char *mission,
			      const char *host_id,
			      const char *instrument,
			      const char *filter,
			      const char *isFFonBoardApplied,
			      float *flat_parms,
                              float *preboost_parms,
			      float exptime, 
			      float temperature,
			      int sl, int ss, int el, int es,
	                      int hscale, int vscale, const char *mode,
			      int color, float solar_elevation, float tau)
                 : RadiometryModel(mission, instrument, sl, ss, el, es)

{
    strcpy(_host_id, host_id);
    strcpy(_instrument, instrument);
    if (filter != NULL)
        strcpy(_filter, filter);
    else
	strcpy(_filter, "");
    _exptime = exptime;
    _temperature = temperature;
    if (isFFonBoardApplied == NULL ||
			(strncmp(isFFonBoardApplied, "TRUE", 4) != 0)) {
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

    if (preboost_parms[0] == 0.0 || preboost_parms[1] == 0.0 || 
        preboost_parms[2] == 0.0) {
        // no on-board preboosting has been applied
        _isOnboardBoostApplied = 0;
        _preboost_parms[0] = _preboost_parms[1] = _preboost_parms[2] = 0.0;
    } else {
        _isOnboardBoostApplied = 1;
        _preboost_parms[0] = preboost_parms[0];
        _preboost_parms[1] = preboost_parms[1];
        _preboost_parms[2] = preboost_parms[2];
    }

    _hscale = hscale;
    _vscale = vscale;

    _ff_file = NULL;
    _ff_desc = NULL;

    _mode = NULL;
    if (mode != NULL)
	_mode = strdup(mode);

    _color = color;
    _solar_elevation = solar_elevation;
    _tau = tau;

    if (!_flat_cache_init) {
	for (int i=0; i < NSYT_MAX_FLAT_CACHE; i++) {
	    _flat_cache_names[i] = NULL;
	    _flat_cache[i] = new RadiometryCalImage(mission, FALSE);
	}
	_flat_cache_init = TRUE;
    }

    read_flat_field_parms("param_files/NSYT_flat_fields.parms");
    loadFlatField(_mode);		// populate the id for the label
}

RadiometryNSYT::RadiometryNSYT() : RadiometryModel()
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
    _preboost_parms[0] = _preboost_parms[1] = _preboost_parms[2] = 0.0;
    _ff_file = NULL;
    _ff_desc = NULL;
    _color = FALSE;
    _solar_elevation = 5.0; 
    _tau = 0.6;
}

////////////////////////////////////////////////////////////////////////
// Destructor
////////////////////////////////////////////////////////////////////////

RadiometryNSYT::~RadiometryNSYT()
{
    if (_ff_file != NULL)
	delete _ff_file;
    if (_ff_desc != NULL)
	delete _ff_desc;
}

////////////////////////////////////////////////////////////////////////
// This factory method creates and returns an instance of this
// subclass for the camera associated with the given file (determined
// by the labels).
// It could be a constructor instead, but this allows us to return NULL.
////////////////////////////////////////////////////////////////////////

RadiometryNSYT *RadiometryNSYT::create(PigFileModel *file)
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

    float preboost_parms[3];
    preboost_parms[0] = preboost_parms[1] = preboost_parms[2] = 0.0;
    file->getOnboardResponsivity(preboost_parms);

    // Bayer mode
    char *mode = NULL;
    const char *bay = file->getBayerMode();
    if (bay != NULL && strcasecmp(bay, "RAW_BAYER") == 0) {
        mode = "RAWBAYER";
    }

    int color = (file->getNB() == 3);

    PigMission *m = PigMission::getMissionObject(file);
    // now do the actual creation by calling the constructor
    return new RadiometryNSYT(file->getMissionName(),
			     m->getHostID(),
			     file->getInstrumentId(),
			     file->getFilterNumber(),
   	                     file->getFlatFieldCorrectionFlag(),
			     flat_parms,
                             preboost_parms,
    /*convert to seconds*/   file->getExposureDuration(1000.0) / 1000.0,
			     file->getInstrumentTemperature(0),
			     sl, ss, el, es, hscale, vscale, mode, color, 
                             solar_elevation, tau);
}

////////////////////////////////////////////////////////////////////////

void RadiometryNSYT::print()

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
// Load a flat-field image.  The image is cached and the slot # is returned,
// or -1 on error.
////////////////////////////////////////////////////////////////////////

int RadiometryNSYT::loadFlatField(const char *mode)
{
    // Find out the name of the flat field file
    PigXerces::initialize();
    PigCameraMapper *map = new PigCameraMapper(NULL, _host_id);
    PigCameraMapEntry *entry = NULL;

    if (_instrument)
        entry = map->findFromID(_instrument);

    if (entry) {
	// We know that cache[0] always has a RadiometryCalImage object
	// because we initialized them in the constructor.

	char *fn = _flat_cache[0]->constructFilename(entry->getSerialNumber(),
				(entry->getFilters() ? _filter : NULL), mode);

	int slot = getCacheSlot(fn, _flat_cache_names, NSYT_MAX_FLAT_CACHE);

	// Reload in case it's new or overflow.  No-op for re-read
	_flat_cache[slot]->loadFile(entry->getSerialNumber(),
				(entry->getFilters() ? _filter : NULL), mode);
	if (_flat_cache[slot]->_product_id != NULL) {
	    _ff_file = strdup(_flat_cache[slot]->_product_id);
	}
	else {
	    _ff_file = NULL;		// probably not needed
	}
	if (_flat_cache[slot]->_product_desc != NULL) {
	    _ff_desc = strdup(_flat_cache[slot]->_product_desc);
	}
	else {
	    _ff_desc = NULL;		// probably not needede
	}

	return slot;
    }

    return -1;
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
// Note: band is 0-based.
////////////////////////////////////////////////////////////////////////
void RadiometryNSYT::applyCorrectionInternal(void *image, int max_nl,int max_ns,
					int is_float, int band, int hasBeenRad)
{
    int i, j;
    double dn;
    double scale_factor = 1;

    short int *image_int = (short int *)image;
    float *image_float = (float *)image;

    // Get values from subclasses

    int slot = loadFlatField(_mode);
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
		register double degrees = (j-_flat_parms[0])*(j-_flat_parms[0]) +
                                          (i-_flat_parms[1])*(i-_flat_parms[1]);
		scale_factor = 1 + _flat_parms[2]*degrees + 
		                   _flat_parms[3]*degrees*degrees + 
	                           _flat_parms[4]*degrees*degrees*degrees;
	    }
	        
	    if (is_float)
		dn = IMAGEF(i,j);
	    else
		dn = IMAGE(i,j);

            // undo onboard preboosting
            if (_isOnboardBoostApplied && !hasBeenRad) {
                dn = dn / _preboost_parms[band];
            }

	    // if flat field correction
	    double flat_dn;
            if (!hasBeenRad) {
	        if (slot >= 0 &&
		        (flat_dn=getFlatFieldDN(_flat_cache[slot], band, i, j)) > 0)
	            // apply scale_factor to undo on-board flat-field correction
		    dn = (dn / scale_factor) / (flat_dn * responsivity);
	        else {
		    // If we actually loaded a flat and the DN is 0, zero the
		    // output.  This lets us blank out areas if needed (e.g.
		    // arm masked area).
		    if (slot >= 0 && _flat_cache[slot]->_valid) {
		        dn = 0.0;
		    } else {
		        // If we didn't load a flat, let the value go through
		        dn = (dn / scale_factor) / responsivity;
		    }
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
                dn += 0.5;              // round
                if (dn > 32767)         // clip
                    dn = 32767;
                if (dn < 0)
                    dn = 0;
                short int short_dn = (short int)dn;

	        IMAGE(i, j) = short_dn;
	    }
	}
    }

}

// return flat field's dn value for a given sample/line pair
float RadiometryNSYT::getFlatFieldDN(RadiometryCalImage *flat,
					int band, int line, int samp)
{
  float newDN = 0.0;
  for (int i = 0; i < _hscale; i++) {
      for (int j = 0; j < _vscale; j++) {
	  newDN = newDN + flat->getValue(band, line*_vscale + j + _sl,
					       samp*_hscale + i + _ss);
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
// As with MER and MSL, NSYT responsivities are the *inverse* of the way it
// was done for MPF and other missions prior to MER.  Since we want to keep
// API the same for all mission and all other missions divide by
// responsivity we are returning here 1/x - an inverse responsivity.  That
// way in calculating rad-corrected value we *multiply* by the responsivity
// and *divide* by the exposure time.
//
// The (0-based) band number is ignored unles _color is true.
////////////////////////////////////////////////////////////////////////

double RadiometryNSYT::getResponsivityFactor(int band)
{
    if (_color) {
	return (_color_resp[band][0] +
		_color_resp[band][1]*_temperature +
		_color_resp[band][2]*_temperature*_temperature);
    }
    return (_resp[0] + _resp[1]*_temperature + 
		       _resp[2]*_temperature*_temperature);
}

double RadiometryNSYT::getResponsivity(int band)
{   
    return _exptime / getResponsivityFactor(band);
}



////////////////////////////////////////////////////////////////////////
// Read in the calibration pointing parameters for a given "point" file.
////////////////////////////////////////////////////////////////////////

void RadiometryNSYT::read_flat_field_parms(char *filename)
{
    FILE *inClientFile;
    char line[255];
    char responsivity_parms[200], responsivity_rgb[200];
    char flat_darkening_factor[200];
    char msg[256];

//!!!! NOT UPDATED FOR NSYT!!!!
    // Assign default values
    // Currently have only one set of values used by Pancam team.
    // Should be updated once we get values for all cameras.
    _resp[0] = 2.77E-07;
    _resp[1] = -3.04E-11;
    _resp[2] = 0.0;
    for (int b=0; b<3; b++) {
	_color_resp[b][0] = _resp[0];
	_color_resp[b][1] = _resp[1];
	_color_resp[b][2] = _resp[2];
    }
//!!!! NOT UPDATED FOR NSYT!!!!

    strcpy(responsivity_parms, "\0");
    strcpy(responsivity_rgb, "\0");
    strcpy(flat_darkening_factor, "\0");

    // open the file
    inClientFile = PigModelBase::openConfigFile(filename, NULL);

    if (inClientFile == NULL) {
        sprintf(msg, 
		"Flat field parameters file %s could not be opened, using default values",
		filename);
	printWarning(msg);
    }
    else {
        // Find out the serial number
        PigXerces::initialize();
	PigCameraMapper *map = new PigCameraMapper(NULL, _host_id);
	PigCameraMapEntry *entry = NULL;
	if (_instrument)
            entry = map->findFromID(_instrument);
	short int found = 0;
	short int found_rgb = 0;
        short int found_flat_darkening_factor = 0;
	if (entry) {
	    int color = entry->getColor();
	    if (entry->getSerialNumber()) {
	        strcat(responsivity_parms, "SN_");
		strcat(responsivity_parms, entry->getSerialNumber());
	        strcat(responsivity_rgb, "SN_");
		strcat(responsivity_rgb, entry->getSerialNumber());
                strcat(flat_darkening_factor, "SN_");
                strcat(flat_darkening_factor, entry->getSerialNumber());
	    }
	    if (entry->getFilters()) {
	        strcat(responsivity_parms, "_F_");
	        strcat(responsivity_parms, _filter);
	        strcat(responsivity_rgb, "_F_");
	        strcat(responsivity_rgb, _filter);
                strcat(flat_darkening_factor, "_F_");
                strcat(flat_darkening_factor, _filter);
	    }
	    if (color) {
		strcat(responsivity_rgb, "_RGB");
	    }
	    strcat(responsivity_parms, "_responsivity = %lf %lf %lf");
	    strcat(responsivity_rgb,
                        "_responsivity = %lf %lf %lf %lf %lf %lf %lf %lf %lf");
            strcat(flat_darkening_factor, "_flat_darkening_factor = %lf");

	    while (fgets(line, sizeof(line), inClientFile) != NULL) {
	        if (strncasecmp(line, responsivity_parms, 15) == 0) {
	            found = 1;
	            sscanf(line, responsivity_parms,
		           &_resp[0], &_resp[1], &_resp[2]);
	        }
                if (color && strncasecmp(line, responsivity_rgb, 15) == 0) {
                    found_rgb = 1;
                    sscanf(line, responsivity_rgb,
                      &_color_resp[0][0],&_color_resp[0][1],&_color_resp[0][2],
                      &_color_resp[1][0],&_color_resp[1][1],&_color_resp[1][2],
                      &_color_resp[2][0],&_color_resp[2][1],&_color_resp[2][2]);
                }
                if (strncasecmp(line, flat_darkening_factor, 29) == 0) {
                    found_flat_darkening_factor = 1;
                    sscanf(line, flat_darkening_factor, &_flat_darkening_factor);
                }
	    }
	}
	fclose(inClientFile);
	// report if no data found
	if ((!_color && !found) || (_color && !found_rgb)) {
	    sprintf(msg, 
	  "Responsivity parameters were not found in %s, using default values!",
		    filename);
	    printWarning(msg);
	}
	else {
            sprintf(msg, "Responsivity parameters read from %s", filename);
            PigModelBase::printUniqueStaticMsg(msg, PigMsgInfo);
	}
        // If we didn't find the right color mode, copy stuff across
        if (found && !found_rgb) {
            for (int b=0; b<3; b++) {
                _color_resp[b][0] = _resp[0];
                _color_resp[b][1] = _resp[1];
                _color_resp[b][2] = _resp[2];
            }
        }
        if (!found && found_rgb) {      // use Green band as default
            _resp[0] = _color_resp[1][0];
            _resp[1] = _color_resp[1][1];
            _resp[2] = _color_resp[1][2];
        }
        if (_color) {
            sprintf(msg,
             "Responsivity values used: R=(%g %g %g) G=(%g %g %g) B=(%g %g %g)",
                _color_resp[0][0], _color_resp[0][1], _color_resp[0][2],
                _color_resp[1][0], _color_resp[1][1], _color_resp[1][2],
                _color_resp[2][0], _color_resp[2][1], _color_resp[2][2]);
            PigModelBase::printUniqueStaticMsg(msg, PigMsgInfo);
        } else {
            sprintf(msg, "Responsivity values used: (%g %g %g)",
                _resp[0], _resp[1], _resp[2]);
            PigModelBase::printUniqueStaticMsg(msg, PigMsgInfo);
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
