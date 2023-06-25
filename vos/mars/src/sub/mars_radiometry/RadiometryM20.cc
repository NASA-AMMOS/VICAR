////////////////////////////////////////////////////////////////////////
// RadiometryM20
//
// Subclass for M20 Cameras Radiometry Model.
// Responsible for maintaining calibration information for a camera 
// and applying radiometric correction when requested.
////////////////////////////////////////////////////////////////////////

#include "RadiometryM20.h"
#include "PigFileModel.h"
#include "PigMission.h"

#include "zvproto.h"
#include "applic.h"			/* for SUCCESS */

#include "PigXerces.h"
#include "PigCameraMapper.h"
#include "PigCameraMapEntry.h"

#include <math.h>
#include <stdlib.h>

RadiometryCalImage *RadiometryM20::_flat_cache[M20_MAX_FLAT_CACHE];
char *RadiometryM20::_flat_cache_names[M20_MAX_FLAT_CACHE];
int RadiometryM20::_flat_cache_init = 0;

#define NO_VALUE_FLAG -99999

////////////////////////////////////////////////////////////////////////
// Constructor
////////////////////////////////////////////////////////////////////////

RadiometryM20::RadiometryM20(const char *mission,
			     const char *host_id,
			     const char *instrument,
			     const char *filter,
			     const char *isFFonBoardApplied,
			     float *flat_parms,
			     float exptime, 
			     float temperature,
			     int sl, int ss, int el, int es,
	                     int hscale, int vscale, const char *mode,
			     int color, float solar_elevation, float tau,
			     int sample_bits, int zoom_pos, int focus_pos,
			     int onboard_shutter, float pcb_temp, int dc_offset)
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
    _hscale = hscale;
    _vscale = vscale;

    for (int i=0; i < M20_NUM_SLOTS; i++) {
        _ff_file[i] = NULL;
        _ff_desc[i] = NULL;
    }

    _mode = NULL;
    if (mode != NULL)
	_mode = strdup(mode);

    _color = color;
    _solar_elevation = solar_elevation;
    _tau = tau;
    _sample_bits = sample_bits;
    _zoom_pos = zoom_pos;
    _focus_pos = focus_pos;
    _tref = 0;
    _use_zcam_tcomp = FALSE;
    _flat_filter_ref = 0;
    _use_zcam_flat = FALSE;
    _use_skycam_rad = FALSE;
    _skycam_B[0] = _skycam_B[1] = _skycam_B[2] = 0.0;
    _skycam_A[0] = _skycam_A[1] = 0.0;
    _skycam_S[0] = _skycam_S[1] = 0.0;
    _onboard_shutter_flag = onboard_shutter;
    _pcb_temp = pcb_temp;
    _dc_offset = dc_offset;
    _bias = 0.0;
    _exp_overhead = 0.0;

    if (!_flat_cache_init) {
	for (int i=0; i < M20_MAX_FLAT_CACHE; i++) {
	    _flat_cache_names[i] = NULL;
	    _flat_cache[i] = new RadiometryCalImage(mission, FALSE);
	}
	_flat_cache_init = TRUE;
    }

    read_flat_field_parms("param_files/M20_rad_cal.parms");

    _exptime = _exptime + _exp_overhead;

    loadFlatFields(_mode);	// populate the id for the label, and pre-cache
}

RadiometryM20::RadiometryM20() : RadiometryModel()
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
    for (int i=0; i<M20_NUM_SLOTS; i++) {
        _ff_file[i] = NULL;
        _ff_desc[i] = NULL;
    }
    _color = FALSE;
    _solar_elevation = 5.0;
    _tau = 0.6;
}

////////////////////////////////////////////////////////////////////////
// Destructor
////////////////////////////////////////////////////////////////////////

RadiometryM20::~RadiometryM20()
{
    for (int i=0; i<M20_NUM_SLOTS; i++) {
        if (_ff_file[i] != NULL)
	    delete _ff_file[i];
        if (_ff_desc[i] != NULL)
	    delete _ff_desc[i];
    }
}

////////////////////////////////////////////////////////////////////////
// This factory method creates and returns an instance of this
// subclass for the camera associated with the given file (determined
// by the labels).
// It could be a constructor instead, but this allows us to return NULL.
////////////////////////////////////////////////////////////////////////

RadiometryM20 *RadiometryM20::create(PigFileModel *file)
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

    // Bayer mode
    char *mode = NULL;
    const char *bay = file->getBayerMode();
    if (bay != NULL && strcasecmp(bay, "RAW_BAYER") == 0) {
	mode = "RAWBAYER";
    }

    int color = (file->getNB() == 3);

    // Shutter subtraction is for skycam only...
    const char *shutter_str = file->getShutterCorrectionMode();
    int shutter = FALSE;
    if (shutter_str != NULL && strcasecmp(shutter_str, "ALWAYS") == 0)
	shutter = TRUE;

    PigMission *m = PigMission::getMissionObject(file);
    // now do the actual creation by calling the constructor
    return new RadiometryM20(file->getMissionName(),
			     m->getHostID(),
			     file->getInstrumentId(),
			     file->getFilterNumber(),
   	                     file->getFlatFieldCorrectionFlag(),
			     flat_parms,
    /*convert to seconds*/   file->getExposureDuration(1000.0) / 1000.0,
			     file->getInstrumentTemperature(0),
			     sl, ss, el, es, hscale, vscale, mode, color,
                             solar_elevation, tau,
			     file->getSampleBits(-1),
			     file->getInstrumentZoomPosition(NO_VALUE_FLAG),
			     file->getInstrumentFocusPosition(NO_VALUE_FLAG),
			     shutter,
			     file->getInstrumentElectronicsTemperature(0),
			     file->getDCOffset(4095));
}

////////////////////////////////////////////////////////////////////////

void RadiometryM20::print()

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
// Load all the flat field images.  Doesn't do anything with them other
// than pre-cache them (and obtain label info).
////////////////////////////////////////////////////////////////////////

void RadiometryM20::loadFlatFields(const char *mode)
{
    if (_use_zcam_flat) {

	// Load all three
	// THIS IS NOT REENTRANT CODE
	// The "FZOOM" flats need to be loaded for filter 0, so we
	// simply save and temporarily munge the _filter value.

	int canon_pos = _zoom_pos;
	_slot[0] = loadFlatField(mode, "RAD", &canon_pos, 0);

	// the above pos feeds through to here

	char save_filter[20];
	strcpy(save_filter, _filter);
	sprintf(_filter, "%d", _flat_filter_ref);

	_slot[1] = loadFlatField(mode, "FZOOM", &canon_pos, 1);

	canon_pos = _zoom_pos;		// reset
	_slot[2] = loadFlatField(mode, "FZOOM", &canon_pos, 2);

	strcpy(_filter, save_filter);

	_slot[3] = loadFlatField(mode, "STATIC", NULL, 3);
	_slot[4] = loadFlatField(mode, "DYN", NULL, 4);

    } else if (_use_skycam_rad) {

	// Load all four
	// THIS IS NOT REENTRANT CODE

	_slot[0] = loadFlatField(mode, NULL, NULL, 0);
	_slot[1] = loadFlatField(mode, "BIASCOLUMN", NULL, 1);
	_slot[2] = loadFlatField(mode, "FRAMETRANSFER", NULL, 2);
	_slot[3] = loadFlatField(mode, "ACTIVEDARK", NULL, 3);

    } else {
	_slot[0] = loadFlatField(mode, NULL, NULL, 0);
    }
}


////////////////////////////////////////////////////////////////////////
// Load a flat-field image.  The image is cached and the slot # is returned,
// or -1 on error.
//
// This is a little tricky with var_flats.  If it's var_flat, we actually
// have to read the varflat file in order to find the name of the actual
// flat to use (the one closest given the variable involved).  We then read
// that.  Returns the zoom value actually found, if pos is non-null.
// index is used to save the FF name and descriptions.
////////////////////////////////////////////////////////////////////////

int RadiometryM20::loadFlatField(const char *mode, const char *type, int *pos,
				int index)
{
    // Find out the name of the flat field file
    PigXerces::initialize();
    PigCameraMapper *map = new PigCameraMapper(NULL, _host_id);
    PigCameraMapEntry *entry = NULL;

    int zoom_pos = 0;
    if (pos) zoom_pos = *pos;

    if (pos) *pos = 0;

    if (_instrument)
        entry = map->findFromID(_instrument);

    if (entry) {
	// We know that cache[0] always has a RadiometryCalImage object
	// because we initialized them in the constructor.

	int var_flat = entry->getVarFlat();

	char *fn = _flat_cache[0]->constructFilename(entry->getSerialNumber(),
				(entry->getFilters() ? _filter : NULL), mode,
				var_flat, type);

	if (var_flat) {
	    int return_value = zoom_pos;;
	    fn = readVarFlat(fn, return_value);
	    if (pos) *pos = return_value;
	}

	int slot = getCacheSlot(fn, _flat_cache_names, M20_MAX_FLAT_CACHE);

	// Reload in case it's new or overflow.  No-op for re-read
        _flat_cache[slot]->loadFile(fn);
	if (_flat_cache[slot]->_product_id != NULL) {
	    _ff_file[index] = strdup(_flat_cache[slot]->_product_id);
	}
	else {
	    _ff_file[index] = NULL;		// probably not needed
	}
	if (_flat_cache[slot]->_product_desc != NULL) {
	    _ff_desc[index] = strdup(_flat_cache[slot]->_product_desc);
	}
	else {
	    _ff_desc[index] = NULL;		// probably not needed
	}

	return slot;
    }
    return -1;
}

////////////////////////////////////////////////////////////////////////
// Reads the var_flat file.  The file looks like:
//
// # comments
// FOCUS
// 3
// 1740 flat_focus_1740.IMG
// 2000 flat_focus_2000.IMG
//
// where the first line is FOCUS or ZOOM or TEMPERATURE, the second line is
// the number of entries, and each entry has the variable value and then the
// name of the flat file associated with it.  Values must be sorted in
// ascending order.
//
// The routine will pick the flat field closest to the given variable and
// return that name.  It does not read the image itself.
//
// Note: Returned string may be a pointer to a static buffer.  Use it
// quickly or copy it.  Not reentrant!!
//
// zoom_value is the zoom value to use on input, and returns the zoom value
// actually used on output.
////////////////////////////////////////////////////////////////////////

char *RadiometryM20::readVarFlat(const char *fn, int &zoom_value)
{
    char fn_path[1024];
    static char filename[PIG_MAX_FILENAME_SIZE];
    int is_focus = false;	// comes from the file itself
    int is_zoom = false;
    int is_temperature = false;
    char msg[1024];
    int n_models = 0;

    PigCameraModel *interp_models[PIG_MAX_INTERP_CAMERA_MODELS];
    double interp_value[PIG_MAX_INTERP_CAMERA_MODELS];
    int i, status;
    int is_temp = false;

    strcpy(filename, fn);		// backup in case of early return
    char *ext = strstr(filename, ".");	// just try .IMG
    if (ext != NULL) {
	strcpy(ext, ".IMG");
    }
    int input_zoom = zoom_value;
    zoom_value = 0;

    // Open the var_flat file

    FILE *f = PigModelBase::openConfigFile((char *)fn, fn_path);
    if (f == NULL) {
	sprintf(msg, "Unable to find var_flat flat field file: %s (path=%s)",
			fn, fn_path);
	PigModelBase::printUniqueStaticMsg(msg, PigMsgWarning);
	PigModelBase::printUniqueStaticMsg("Trying again with .IMG",
								PigMsgWarning);
	return filename;
    }

    char line[2048];
    line[0] = '#';
    while (line[0] == '#') {		// skip comments
	char *l = fgets(line, sizeof(line), f);
	if (l == NULL) {
	    sprintf(msg, "Premature end of var_flat file: %s (path=%s)",
				fn, fn_path);
	    PigModelBase::printUniqueStaticMsg(msg, PigMsgWarning);
	    PigModelBase::printUniqueStaticMsg("Trying again with .IMG",
								PigMsgWarning);
	    fclose(f);
	    return filename;
	}
    }

    if (strncasecmp(line, "FOCUS", 5) == 0) {
	is_focus = true;
    }
    if (strncasecmp(line, "ZOOM", 4) == 0) {
	is_zoom = true;
    }
    if (strncasecmp(line, "TEMPERATURE", 11) == 0) {
	is_temperature = true;
    }

    if (!is_focus && !is_zoom && !is_temperature) {
	sprintf(msg, "One of FOCUS or ZOOM or TEMPERATURE must be in var_flat file: %s (path=%s)",
				fn, fn_path);
	PigModelBase::printUniqueStaticMsg(msg, PigMsgWarning);
	PigModelBase::printUniqueStaticMsg("Trying again with .IMG",
								PigMsgWarning);
	fclose(f);
	return filename;
    }

    line[0] = '#';
    while (line[0] == '#') {		// skip comments
	char *l = fgets(line, sizeof(line), f);
	if (l == NULL) {
	    sprintf(msg, "Premature end of var_flat file: %s (path=%s)",
				    fn, fn_path);
	    PigModelBase::printUniqueStaticMsg(msg, PigMsgWarning);
	    PigModelBase::printUniqueStaticMsg("Trying again with .IMG",
								PigMsgWarning);
	    fclose(f);
	    return filename;
	}
    }

    sscanf(line, "%d", &n_models);

    if (n_models <= 0 || n_models > PIG_MAX_VAR_FLAT_FILES) {
	sprintf(msg, "Unreasonable var_flat n_files=%d from file %s",
				n_models, fn_path);
	PigModelBase::printUniqueStaticMsg(msg, PigMsgWarning);
	PigModelBase::printUniqueStaticMsg("Trying again with .IMG",
								PigMsgWarning);
	fclose(f);
	return filename;
    }

    int prior_value = 0;
    int prior_diff = 100000;
    char prior_name[PIG_MAX_FILENAME_SIZE];
    int cur_value = 0;
    char cur_name[PIG_MAX_FILENAME_SIZE];

    int target_value = _focus_pos;
    if (is_zoom)
	target_value = input_zoom;
    if (is_temperature)
        target_value = (int)_temperature; // don't really need fractions

    for (i=0; i < n_models; i++) {
	line[0] = '#';
	while (line[0] == '#') {		// skip comments
	    char *l = fgets(line, sizeof(line), f);
	    if (l == NULL) {
	        sprintf(msg, "Premature end of var_flat file: %s (path=%s)",
				    fn, fn_path);
	        PigModelBase::printUniqueStaticMsg(msg, PigMsgWarning);
	        PigModelBase::printUniqueStaticMsg("Trying again with .IMG",
								PigMsgWarning);
	        fclose(f);
	        return filename;
	    }
	}
	sscanf(line, "%d %s", &cur_value, cur_name);

	int cur_diff = target_value - cur_value;

	// If the difference is negative, we went too far.  Pick this or
	// the prior depending on who's closer.  If i==0 then we're on the
	// the first one, so current is closer

	if (cur_diff < 0) {
	    if (i==0 || (-cur_diff) <= prior_diff) {	// current is best
		zoom_value = cur_value;
		strcpy(filename, cur_name);
		fclose(f);
		return filename;
	    }
	    zoom_value = prior_value;			// prior is best
	    strcpy(filename, prior_name);
	    fclose(f);
	    return filename;
	}

	prior_value = cur_value;
	strcpy(prior_name, cur_name);
	prior_diff = cur_diff;
    }

    // If we end the loop, the last (current) is the best

    zoom_value = cur_value;
    strcpy(filename, cur_name);
    fclose(f);
    return filename;
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
//
// For SkyCam:
// B = 24.52 + 0.595*Te + 0.004505*Te^2 + (4095.-Voff)/2.
// image1 = edr - B - bias_column
// S = 1.556 * exp(0.124 * Tc)
// A = 2.285 * exp(0.111 * Tc)
// image2 = image1 - S * frame_transfer - A * active_dark * exposure_duration
// rad = image2 * resp / (rad_flat * exposure_duration)
//
// Or, if onboard shutter subtraction has been used:
// rad = (edr - A * active_dark * exposure_duration) * resp /
//                                                (rad_flat * exposure_duration)
//
// where Tc is the CCD temperature in deg C and Te is the PCB temp and Voff is
// the DC_OFFSET.
////////////////////////////////////////////////////////////////////////
void RadiometryM20::applyCorrectionInternal(void *image, int max_nl, int max_ns,
					int is_float, int band, int hasBeenRad)
{
    int i, j;
    double dn;
    double scale_factor = 1;

    short int *image_int = (short int *)image;
    float *image_float = (float *)image;

    // Get values from subclasses

    // Re-load flats in case they overflowed.  This does mean re-reading the
    // var_flat file for each image (for those that use that), but small price
    // to pay for protecting against cache overflow.  It should not re-read the
    // actual flat unless the cache overflowed.

    loadFlatFields(_mode);

    double responsivity = getResponsivity(band);
    double resp_factor = getResponsivityFactor(band);
    double dnscale = getDnScalingFactor();
    double dnoff = getDnScalingOffset();

    // Get skycam coefficients
    double skycam_b = 0.0;
    double skycam_a = 0.0;
    double skycam_s = 0.0;

    if (_use_skycam_rad) {
	skycam_b = _skycam_B[0] + _skycam_B[1]*_pcb_temp +
		   _skycam_B[2]*_pcb_temp*_pcb_temp +
		   (4095. - _dc_offset) / 2.;
	skycam_s = _skycam_S[0] * exp(_skycam_S[1] * _temperature);
	skycam_a = _skycam_A[0] * exp(_skycam_A[1] * _temperature);
    }

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
	    double orig_dn = dn;

	    // if flat field correction

	    double flat_dn = 0.0;
	    int flat_valid = FALSE;
            if (!hasBeenRad) {
		double flat_dn = 0.0;
		if (_use_zcam_flat) {

		    // Add in the DC_OFFSET

		    dn = dn + _dc_offset;

		    // Subtract the static bias

		    double static_bias = 0.0;
		    if (_flat_cache[_slot[3]]->_valid) {
			static_bias = getFlatFieldDN(_flat_cache[_slot[3]],
								band, i, j);
		    }

		    dn = dn - static_bias;

		    // Modify the exposure time based on the dynamic bias.
		    // Because this is already folded into responsivity,
		    // we actually update the responsvity value manually.

		    double dynamic_bias = 0.0;
		    if (_flat_cache[_slot[4]]->_valid) {
			dynamic_bias = getFlatFieldDN(_flat_cache[_slot[4]],
								band, i, j);
		    }

		    responsivity = (_exptime + dynamic_bias) / resp_factor;

		    // Mastcam-Z style flats.  See Mastcam-Z cal paper, section
		    // 4.1.5.  Basically, we have a few zooms at all filters;
		    // we scale that the with ratio of many zooms to the few
		    // zooms at filter 0.

		    // rad_flat
		    double dn1 = 0.0;
		    if (_flat_cache[_slot[0]]->_valid) {
			flat_valid = TRUE;
		        dn1 = getFlatFieldDN(_flat_cache[_slot[0]], band,i,j);
		    }
		    // fzoom_canon
		    double dn2 = 0.0;
		    if (_flat_cache[_slot[1]]->_valid) {
			flat_valid = TRUE;
		        dn2 = getFlatFieldDN(_flat_cache[_slot[1]], band,i,j);
		    }
		    // fzoom actual
		    double dn3 = 0.0;
		    if (_flat_cache[_slot[2]]->_valid) {
			flat_valid = TRUE;
		        dn3 = getFlatFieldDN(_flat_cache[_slot[2]], band,i,j);
		    }

		    if (dn1 > 0 && dn2 > 0 && dn3 > 0) {
			flat_dn = dn1 * (dn3 / dn2);
		    }
		}
		else if (_use_skycam_rad) {
		    flat_dn = getFlatFieldDN(_flat_cache[_slot[0]], band, i, j);
		    double bias_column =
			     getFlatFieldDN(_flat_cache[_slot[1]], band, i, 0);
		    double frame_transfer =
			     getFlatFieldDN(_flat_cache[_slot[2]], band, i, j);
		    double active_dark =
			     getFlatFieldDN(_flat_cache[_slot[3]], band, i, j);
		    flat_valid = TRUE;

		    // Modify the DN based on the formulas.  Onboard shutter
		    // subtraction uses a truncated formula

		    if (_onboard_shutter_flag) {	// partial formula
			dn = dn - skycam_a * active_dark * _exptime;
		    } else {				// full formula
		        double img1 = dn - skycam_b - bias_column;
		        dn = img1 - skycam_s * frame_transfer -
				skycam_a * active_dark * _exptime;
		    }
		} else {				// NORMAL FLAT
	            if (_slot[0] >= 0 && _flat_cache[_slot[0]]->_valid) {
		        flat_dn=getFlatFieldDN(_flat_cache[_slot[0]], band,i,j);
			flat_valid = TRUE;
		    }
		}

		// Now apply the flat

		if (flat_valid) {
		    if (flat_dn > 0.0) {
	                // apply scale_factor to undo on-board flat-field corr
		        dn = ((dn - _bias) / scale_factor) /
						(flat_dn * responsivity);
		    } else {
		        // If we actually loaded a flat and the DN is 0, zero
		        // the output.  This lets the corners of e.g. RMI go
			// transparent.  We set orig_dn to 0 too as a flag.
		        dn = 0.0;
			orig_dn = 0.0;
		    }
		} else {
		    // If we didn't load a flat, let the value go through
		    dn = ((dn - _bias) / scale_factor) / responsivity;
	        }
            }

	    // If exposure time is 0, we get an infinity value. Per Alex Hayes:
	    // "By definition, the rad output of a zero-exposure image should
	    // be an array of zeros as the image should technically be
	    // subtracted by itself."
	    // So, we force to 0 if exptime is 0.

	    if (_exptime == 0.0)
		dn = 0.0;

            // Apply scaled radiance factor.  If zenith is off, the
	    // factor is 1.0, so no need for a conditional

            dn = dn / zenith_factor;

	    // Apply scaling (should be 0 and 1 for floats, but it's
	    // applied anyway to make sure labels are consistent))

	    dn = (dn - dnoff) / dnscale;

	    // Round to short int and make sure it is within range
	    // and store back into the image

	    if (is_float) {
		// Bit of a hack, but if the input is non-0 we really do not
		// want the output to be 0 or it goes transparent.  Unless
		// the exptime is 0 in which case the entire image is 0.
		if (orig_dn != 0.0 && dn == 0.0 && _exptime != 0.0)
		    dn = 1e-20;
		// On the other hand, if the original DN is 0 we really want the
		// output to be 0 too to preserve transparency
		if (orig_dn == 0.0)
		    dn = 0.0;
	        IMAGEF(i, j) = (float)dn;
	    }
	    else {
                dn += 0.5;              // round
                if (dn > 32767)         // clip
                    dn = 32767;
                if (dn < 0)
                    dn = 0;
		// Clip at 1 if the input is non-zero to avoid bias making
		// the dn go to 0 and turn weirdly transparent  Unless
		// the exptime i s0 in which cas the entire image is 0.
		if (orig_dn != 0.0 && dn < 1 && _exptime != 0.0)
		    dn = 1;
		// If the original DN was 0, make sure the output is too in
		// order to preserve transparency
		if (orig_dn == 0.0)
		    dn = 0;
                short int short_dn = (short int)dn;

	        IMAGE(i, j) = short_dn;
	    }
	}
    }

}

// return flat field's dn value for a given sample/line pair
float RadiometryM20::getFlatFieldDN(RadiometryCalImage *flat,
					int band, int line, int samp)
{
  float newDN = 0.0;
  int zero = FALSE;
  for (int i = 0; i < _hscale; i++) {
      for (int j = 0; j < _vscale; j++) {
          float flat_dn = flat->getValue(band, line*_vscale + j + _sl,
					       samp*_hscale + i + _ss);
          if (flat_dn == 0) zero = TRUE;
          newDN = newDN + flat_dn;
      }
  }
  if (zero)
      return 0.0;
  return newDN/(_hscale*_vscale);

#if 0

//!!!! HORRID HACK to scale the blue FF more at shorter exposures
double factor = 1.0;		//!!!!
if (band == 2) {		//!!!!
    factor = (.020 - _exptime);		//!!!!
    if (factor < 0) factor = 0;		//!!!!
    factor *= 10;		//!!!!
    factor += 1;		//!!!!
}		//!!!!
if (band == 0) {		//!!!!
    factor = (.010 - _exptime);		//!!!!
    if (factor < 0) factor = 0;		//!!!!
    factor *= 8;		//!!!!
    factor += 1;		//!!!!
}		//!!!!

if (line == 100 && samp == 100) printf("b=%d exptie = %f, factor = %f\n", band, _exptime, factor);	//!!!!
  return (newDN/(_hscale*_vscale) - 1) * factor + 1;	//!!!!;
//!!!!  return newDN/(_hscale*_vscale);

#endif

}

////////////////////////////////////////////////////////////////////////
// Return the responsivity coefficient given the temperature and exposure 
// time.
// This coefficient is divided into the input DN to give a number with the
// units watts/(meter**2,steradian,micron).
//
// As with MER and MSL, M20 responsivities are the *inverse* of the way it was
// done for MPF and other missions prior to MER.  Since we want to keep
// API the same for all mission and all other missions divide by
// responsivity we are returning here 1/x - an inverse responsivity.  That
// way in calculating rad-corrected value we *multiply* by the responsivity
// and *divide* by the exposure time.
//
// The (0-based) band number is ignored unles _color is true.
//
// If nominal_bits is set, then we get the current # of bits and compare
// to it, and adjust things as necessary so they match.
////////////////////////////////////////////////////////////////////////

double RadiometryM20::getResponsivityFactor(int band)
{
    double bits_factor = 1.0;

    if (_nominal_bits > 0 && _sample_bits > 0) {
	int diff = _nominal_bits - _sample_bits;
	bits_factor = pow(2.0, (double)diff);
    }

    double *resp_array = _resp;
    if (_color)
	resp_array= _color_resp[band];

    // Choose the responsivity formula

    double resp;

    if (_use_zcam_tcomp) {
        PigModelBase::printUniqueStaticMsg(
			"Using Mastcam-Z temperature compensation", PigMsgInfo);
	// array[0] is the responsivity and array[1] is the Beta value
	resp = resp_array[0] / (1 + resp_array[1] * (_temperature - _tref));
    }
    else {
	// standard quadratic based on temperature
        resp = resp_array[0] +
	       resp_array[1]*_temperature +
	       resp_array[2]*_temperature*_temperature;
    }

    return resp * bits_factor;
}


double RadiometryM20::getResponsivity(int band)
{
    return _exptime / getResponsivityFactor(band);
}

////////////////////////////////////////////////////////////////////////
// Read in the calibration pointing parameters for a given "point" file.
//
// Parameters allowed in the file:
//
// SN_xxx_responsivity = %f %f %f
// SN_xxx_F_x_responsiity = %f %f %f
// SN_xxx_RGB_responsivity = %f %f %f %f %f %f %f %f %f
// SN_xxx_F_x_RGB_responsivity = %f %f %f %f %f %f %f %f %f
// SN_xxx_flat_darkening_factor = %f
// SN_xxx_nominal_bits = %d
// SN_xxx_zoom_fnum_table = %s
// SN_xxx_zoom_fnum_ref = %f
// SN_xxx_temperature_ref = %f
// SN_xxx_flat_filter_ref = %d
// SN_xxx_skycam_B = %f %f %f
// SN_xxx_skycam_S = %f %f
// SN_xxx_skycam_A = %f %f
// SN_xxx_bias = %f
// SN_xxx_exp_overhead = %f
//
// Responsivity params are 3-vectors containing the coefficiencts of the
// responsivity polynomial (based on temperature).  Constant, linear, and
// quadratic terms.  The _RGB versions have 3-vectors for each of the Red,
// Green, Blue channels.  The _F_x versions apply to a given filter only.
//
// flat_darkening_factor is an overall factor that is used to compensate
// for flat fields that change the overall brightness (they should average
// to 1, ideally).
//
// nominal_bits is the number of bits in the input that the rad cal params
// are tuned for.  In the case of SuperCam RMI, it can actually return
// 10, 12, or 13 bits depending on the mode.  If not compensated for, that
// would result in a factor of 8 difference in rad (which is incorrect).
// If nominal_bits is 12, then the 10-bit data is boosted by a factor of
// 4 while the 13-bit data is cut in half, so the radiometry is always the
// same and correct.
//
// zoom_fnum_table is a filename (from the $MARS_CONFIG_PATH root) containing
// the conversion from zoom motor count to f/number.  That is used in
// conjunction with zoom_fnum0 to adjust the responivities based on zoom.
//
// zoom_fnum_ref is the reference f/num for the zoom adjustment.
//
// The zoom_fnum_table is a simple CSV in the format:
// zoom,fnum
// where zoom is the motor count and fnm is the f/number (all float).
// An optional third column is ignored.
// Comments are indicated by #
// It is assumed the table is in order!!  Once we pass the desired zoom
// we stop looking and interpolate the last two entries.  The whole table
// is not stored in memory.
//
// temperature_ref sets the reference temperature **AND** changes the
// temperature compensation model to the ZCAM style, which is a different
// formula.
//
// flat_filter_ref sets the reference filter for flat fields **AND** changes
// the flat field mechanism to use the ZCAM style (which combines 3 flats to
// achieve zoom compensated flats).  The reference filter is the one used
// to find the proper fzoom_flat.
//
// skycam_X are parameters for the skycam radiometric correction.
//
// bias is a (constant) value subtracted from the DN before applying rad
// correction.  Takes the place of shutter subtraction (which is not
// available for M20 ecams).
//
// exp_overhead is a (constant) value added to the exposure time before
// correction.
////////////////////////////////////////////////////////////////////////

void RadiometryM20::read_flat_field_parms(char *filename)
{
    FILE *inClientFile;
    char line[255];
    char fmt_sn[200];
    char fmt_responsivity_parms[200], fmt_responsivity_rgb[200];
    char fmt_flat_darkening_factor[200], fmt_nominal_bits[200];
    char fmt_zoom_fnum_table[200], fmt_zoom_fnum_ref[200];
    char fmt_temperature_ref[200];
    char fmt_flat_filter_ref[200];
    char fmt_skycam_B[200];
    char fmt_skycam_S[200];
    char fmt_skycam_A[200];
    char fmt_bias[200];
    char fmt_exp_overhead[200];
    char msg[256];

    // Assign default values, what's used for the Navcam.
    // This is a guess only, and DOES NOT apply to other cameras!
    _resp[0] = 1.210E-07;
    _resp[1] = 0.0;
    _resp[2] = 0.0;
    for (int b=0; b<3; b++) {
	_color_resp[b][0] = _resp[0];
	_color_resp[b][1] = _resp[1];
	_color_resp[b][2] = _resp[2];
    }
    _nominal_bits = 0;

    char zoom_fnum_table[200];
    strcpy(zoom_fnum_table, "");

    double zoom_fnum_ref = 0.0;

    _bias = 0.0;
    _exp_overhead = 0.0;

    strcpy(fmt_sn, "\0");
    strcpy(fmt_responsivity_parms, "\0");
    strcpy(fmt_responsivity_rgb, "\0");
    strcpy(fmt_flat_darkening_factor, "\0");
    strcpy(fmt_nominal_bits, "\0");
    strcpy(fmt_zoom_fnum_table, "\0");
    strcpy(fmt_zoom_fnum_ref, "\0");
    strcpy(fmt_temperature_ref, "\0");
    strcpy(fmt_flat_filter_ref, "\0");
    strcpy(fmt_skycam_B, "\0");
    strcpy(fmt_skycam_S, "\0");
    strcpy(fmt_skycam_A, "\0");
    strcpy(fmt_bias, "\0");
    strcpy(fmt_exp_overhead, "\0");

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
	        strcat(fmt_sn, "SN_");
		strcat(fmt_sn, entry->getSerialNumber());
		strcat(fmt_sn, "_");
	    }
	    if (entry->getFilters() && _filter != NULL && strlen(_filter)!= 0) {
		strcat(fmt_responsivity_parms, "F_");
		strcat(fmt_responsivity_parms, _filter);
		strcat(fmt_responsivity_parms, "_");
		strcat(fmt_responsivity_rgb, "F_");
		strcat(fmt_responsivity_rgb, _filter);
		strcat(fmt_responsivity_rgb, "_");
                strcat(fmt_flat_darkening_factor, "_F_");
                strcat(fmt_flat_darkening_factor, _filter);
                strcat(fmt_flat_darkening_factor, "_");
	    }
	    if (color) {
		strcat(fmt_responsivity_rgb, "RGB_");
	    }

	    strcat(fmt_responsivity_parms, "responsivity = %lf %lf %lf");
	    strcat(fmt_responsivity_rgb,
			"responsivity = %lf %lf %lf %lf %lf %lf %lf %lf %lf");
            strcat(fmt_flat_darkening_factor, "flat_darkening_factor = %lf");

	    strcat(fmt_nominal_bits, "nominal_bits = %d");
	    strcat(fmt_zoom_fnum_table, "zoom_fnum_table = %s");
	    strcat(fmt_zoom_fnum_ref, "zoom_fnum_ref = %lf");
	    strcat(fmt_temperature_ref, "temperature_ref = %lf");
	    strcat(fmt_flat_filter_ref, "flat_filter_ref = %d");
	    strcat(fmt_skycam_B, "skycam_B = %lf %lf %lf");
	    strcat(fmt_skycam_A, "skycam_A = %lf %lf");
	    strcat(fmt_skycam_S, "skycam_S = %lf %lf");
	    strcat(fmt_bias, "bias = %lf");
	    strcat(fmt_exp_overhead, "exp_overhead = %lf");

	    while (fgets(line, sizeof(line), inClientFile) != NULL) {
		if (strncasecmp(line, fmt_sn, strlen(fmt_sn)) != 0)
		    continue;				// line is not for us

		char *linep = line + strlen(fmt_sn);	// point past the SN
		int nch = strstr(fmt_responsivity_parms, " =") -
							fmt_responsivity_parms;
	        if (strncasecmp(linep, fmt_responsivity_parms, nch) == 0) {
	            found = 1;
	            sscanf(linep, fmt_responsivity_parms,
		   	    &_resp[0], &_resp[1], &_resp[2]);
		}
		nch = strstr(fmt_responsivity_rgb, " =") - fmt_responsivity_rgb;
	        if (color && (strncasecmp(linep,fmt_responsivity_rgb,nch)==0)) {
	            found_rgb = 1;
	            sscanf(linep, fmt_responsivity_rgb,
		      &_color_resp[0][0],&_color_resp[0][1],&_color_resp[0][2],
		      &_color_resp[1][0],&_color_resp[1][1],&_color_resp[1][2],
		      &_color_resp[2][0],&_color_resp[2][1],&_color_resp[2][2]);
		}
		nch = strstr(fmt_flat_darkening_factor, " =") -
							fmt_flat_darkening_factor;
                if (strncasecmp(linep, fmt_flat_darkening_factor, nch) == 0) {
                    found_flat_darkening_factor = 1;
                    sscanf(linep,fmt_flat_darkening_factor,&_flat_darkening_factor);
                }
		nch = strstr(fmt_nominal_bits, " =") - fmt_nominal_bits;
		if (strncasecmp(linep, fmt_nominal_bits, nch) == 0) {
		    sscanf(linep, fmt_nominal_bits, &_nominal_bits);
		}
		nch = strstr(fmt_zoom_fnum_table, " =") - fmt_zoom_fnum_table;
		if (strncasecmp(linep, fmt_zoom_fnum_table, nch) == 0) {
		    sscanf(linep, fmt_zoom_fnum_table, zoom_fnum_table);
		}
		nch = strstr(fmt_zoom_fnum_ref, " =") - fmt_zoom_fnum_ref;
		if (strncasecmp(linep, fmt_zoom_fnum_ref, nch) == 0) {
		    sscanf(linep, fmt_zoom_fnum_ref, &zoom_fnum_ref);
		}
		nch = strstr(fmt_temperature_ref, " =") - fmt_temperature_ref;
		if (strncasecmp(linep, fmt_temperature_ref, nch) == 0) {
		    sscanf(linep, fmt_temperature_ref, &_tref);
		    _use_zcam_tcomp = TRUE;
		}
		nch = strstr(fmt_flat_filter_ref, " =") - fmt_flat_filter_ref;
		if (strncasecmp(linep, fmt_flat_filter_ref, nch) == 0) {
		    sscanf(linep, fmt_flat_filter_ref, &_flat_filter_ref);
		    _use_zcam_flat = TRUE;
		}
		nch = strstr(fmt_skycam_B, " =") - fmt_skycam_B;
		if (strncasecmp(linep, fmt_skycam_B, nch) == 0) {
		    sscanf(linep, fmt_skycam_B, &_skycam_B[0], &_skycam_B[1],
							       &_skycam_B[2]);
		    _use_skycam_rad = TRUE;
		}
		nch = strstr(fmt_skycam_S, " =") - fmt_skycam_S;
		if (strncasecmp(linep, fmt_skycam_S, nch) == 0) {
		    sscanf(linep, fmt_skycam_S, &_skycam_S[0], &_skycam_S[1]);
		}
		nch = strstr(fmt_skycam_A, " =") - fmt_skycam_A;
		if (strncasecmp(linep, fmt_skycam_A, nch) == 0) {
		    sscanf(linep, fmt_skycam_A, &_skycam_A[0], &_skycam_A[1]);
		}
		nch = strstr(fmt_bias, " =") - fmt_bias;
		if (strncasecmp(linep, fmt_bias, nch) == 0) {
		    sscanf(linep, fmt_bias, &_bias);
		}
		nch = strstr(fmt_exp_overhead, " =") - fmt_exp_overhead;
		if (strncasecmp(linep, fmt_exp_overhead, nch) == 0) {
		    sscanf(linep, fmt_exp_overhead, &_exp_overhead);
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
	if (!found && found_rgb) {	// use Green band as default
	    _resp[0] = _color_resp[1][0];
	    _resp[1] = _color_resp[1][1];
	    _resp[2] = _color_resp[1][2];
	}
	// Adjust responsivities for zoom.  We use the zoom_fnum_table to
	// conver the zoom to fnum, then the adjustment is:
	// resp = orig_resp / (fnum/fnum_ref)^2

	if (_zoom_pos != NO_VALUE_FLAG && (strlen(zoom_fnum_table) != 0) &&
					 zoom_fnum_ref != 0.0) {

	    double fnum = 0.0;

	    // We have both, so do the work...

	    // open the file
	    inClientFile = PigModelBase::openConfigFile(zoom_fnum_table, NULL);
	    if (inClientFile == NULL) {
        	sprintf(msg, 
		"Unable to open zoom_fnum_table file %s", zoom_fnum_table);
		printWarning(msg);
	    }
	    else {

		// Read the file, only as far as we need
		double prev_zoom = 0;
		double prev_fnum = 0;
		double cur_zoom, cur_fnum;
	        while (fgets(line, sizeof(line), inClientFile) != NULL) {
		    if (line[0] == '#')
			continue;			// comment
		    sscanf(line, "%lf,%lf", &cur_zoom, &cur_fnum);

		    // see if we hit the mark exactly
		    if (cur_zoom == _zoom_pos) {
			fnum = cur_fnum;
			break;
		    }
		    if (cur_zoom > _zoom_pos) {	// Passed it, so interp
			double interp = (cur_zoom - _zoom_pos) /
					(cur_zoom - prev_zoom);
			fnum = cur_fnum - (cur_fnum - prev_fnum) * interp;
			break;
		    }
		    prev_zoom = cur_zoom;
		    prev_fnum = cur_fnum;
		}
		fclose(inClientFile);
		if (fnum == 0)
		    fnum = cur_fnum;	// in case we hit the end of the table
	    }

	    if (fnum != 0) {
	        // Now adjust the repsonsivities by the correction factor

	        double corr = ((zoom_fnum_ref * zoom_fnum_ref) / (fnum * fnum));

		sprintf(msg, "Rad zoom compensation factor: %f (fnum: %f)",
			corr, fnum);
		PigModelBase::printUniqueStaticMsg(msg, PigMsgInfo);

		// We only adjust the Constant term.  This is only used for ZCAM
		// which also uses the zcam_tcomp where only [0] should be
		// adjusted.  If it was ever used on something not using that,
		// (and was still using the quadratic terms) this would have to
		// adjust all three.
	        for (int i=0; i<3; i++) {
		    _resp[i] /= corr;
		    _color_resp[i][0] /= corr;
		}
	    }
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
