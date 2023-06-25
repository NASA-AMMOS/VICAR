////////////////////////////////////////////////////////////////////////
// RadiometryPHX
//
// Subclass for PHX Cameras Radiometry Model.  
// Responsible for maintaining calibration information for a camera 
// and applying radiometric correction when requested.
//
// Dark current subtraction is enabled by default, unless POINT_METHOD=NODARK
// is specified.  FORCE_DARK turns it on.
//
// If dark is on, active correction is always on.  However, bias and storage
// correction are on only if onboard dark was NOT done.
//
// Smear correction is likewise on only if dark is on, and onboard was not
// done.  If dark is on, smear can be turned on or off explicitly via
// POINT_METHOD=SMEAR=ON or POINT_METHOD=SMEAR=OFF.  Either override the
// automatic (onboard) setting.  Smear is not used if overall dark is off.
//
// Binning compensation is also enabled, unless POINT_METHOD=NOBINNING is
// specified.  FORCE_BINNING turns it on.  This compensates for an FSW bug
// where the values are too bright by a certain factor when downsampling is
// used.  It is not a true inversion, but gets close.  The factor varies based
// on how much binning (and what type) was done.
//
// If dark is on, there are several methods to use...
// POINT_METHOD=DARK=SHAW		Adam Shaw's method
// POINT_METHOD=DARK=LEMMON		Mark Lemmon's method
// POINT_METHOD=DARK=FAST_LEMMON	Mark Lemmon's faster method (no exp()
//					per pixel)
// The above Lemmon methods use weights in the Tbar computation.  To turn that
// off (all weights = 1.0), append _NOWT, e.g. LEMMON_NOWT or FAST_LEMMON_NOWT.
//
// POINT_METHOD=RAD_FRAME=DARK will make it return the dark frame *only*,
// without the image, or flat field.  Useful for testing dark current.
//
////////////////////////////////////////////////////////////////////////

#include "RadiometryPHX.h"
#include "PigFileModel.h"

#include "zvproto.h"
#include "applic.h"			/* for SUCCESS */

#include "PigXerces.h"
#include "PigCameraMapper.h"
#include "PigCameraMapEntry.h"

#include <stdlib.h>

RadiometryCalImage *RadiometryPHX::_flat = NULL;
RadiometryCalImage *RadiometryPHX::_dark_H = NULL;
RadiometryCalImage *RadiometryPHX::_dark_LowPoints = NULL;
RadiometryCalImage *RadiometryPHX::_dark_Q0 = NULL;
RadiometryCalImage *RadiometryPHX::_dark_Q1 = NULL;
RadiometryCalImage *RadiometryPHX::_dark_Q2 = NULL;
RadiometryCalImage *RadiometryPHX::_L_beta0 = NULL;
RadiometryCalImage *RadiometryPHX::_L_gamma0 = NULL;
RadiometryCalImage *RadiometryPHX::_L_gamma1 = NULL;
RadiometryCalImage *RadiometryPHX::_L_gamma2 = NULL;
RadiometryCalImage *RadiometryPHX::_L_bias_column = NULL;

#define RAC_FOCUS_TERM(f) atan(_rac_focus_scale_term /			\
			     (_rac_focus_Xi - (312 - f) * _rac_focus_delta))

////////////////////////////////////////////////////////////////////////
// Constructor
////////////////////////////////////////////////////////////////////////

RadiometryPHX::RadiometryPHX(const char *mission,
			     const char *host_id,
			     const char *instrument,
			     const char *filter,
			     double exptime, 
			     double Tave, double Ts, double Te, double Tp,
			     double Toptics,
			     int video_offset,
			     int sl, int ss, int el, int es,
	                     int hscale, int vscale,
			     const char *instrument_mode_id,
			     int focus_step, int cover,
			     int onboard_shutter)
                 : RadiometryModel(mission, instrument, sl, ss, el, es)

{
    _host_id = strdup(host_id);
    _filter = strdup(filter);
    _exptime = exptime;
    _Tave = Tave;		// Simple average of start & end temps
    _Ts = Ts;			// start temp
    _Te = Te;			// end temp
    _Tp = Tp;			// PCB (electronics) temp
    _Toptics = Toptics;
    _video_offset = video_offset;

    // for responsivity
    _Tbar = calculate_Tbar(_exptime, _Ts, _Te, FALSE);

    // no on-board correction is available on PHX
    _hscale = hscale;
    _vscale = vscale;

    _focus_step = focus_step;		// for RAC
    _cover = cover;

    // Read camera mapping to get S/N and has-filter status

    PigXerces::initialize();
    PigCameraMapper *map = new PigCameraMapper(_mission, _host_id);
    _serial_number = NULL;
    int has_filt = FALSE;
    if (_instrument) {
        PigCameraMapEntry *entry = map->findFromID(_instrument);
	_serial_number = strdup(entry->getSerialNumber());
	has_filt = entry->getFilters();
    }
    delete map;
    PigXerces::close();

    if (!has_filt) {		// if no filter on camera, null out the field
	if (_filter != NULL) {
	    delete _filter;
	    _filter = NULL;
	}
    }

    _is_ssi = FALSE;
    _is_rac = FALSE;

    if (strncasecmp(_instrument, "SSI", 3) == 0)
	_is_ssi = TRUE;
    if (strncasecmp(_instrument, "RAC", 3) == 0)
	_is_rac = TRUE;

    // See if dark current should be applied

    _use_dark = TRUE;
    if (!_is_ssi)
	_use_dark = FALSE;		// SSI only

    char point_method[256], *value;
    int count;
    getParam("POINT_METHOD", point_method, &count, 1, 0);
    if (count != 0) {
	value = parseParamString(point_method, "NODARK");
	if (value != NULL) {
	    _use_dark = FALSE;
	}
	value = parseParamString(point_method, "FORCE_DARK");
	if (value != NULL) {		// turn it on anyway, even for non-SSI
	    _use_dark = TRUE;
	}
    }

    // Check for dark current method

    _use_lemmon = TRUE;
    _use_fast_dark = FALSE;
    _use_weights = TRUE;
    _dark_frame_only = FALSE;

    if (_use_dark) {
	value = parseParamString(point_method, "DARK");
	if (value != NULL) {
	    if (strncasecmp(value, "SHAW", 4) == 0) {
		_use_lemmon = FALSE;
	    }
	    if (strncasecmp(value, "LEMMON", 6) == 0) {
		_use_lemmon = TRUE;
		_use_fast_dark = FALSE;
		if (strncasecmp(value, "LEMMON_NOWT", 11) == 0) {
		    _use_weights = FALSE;
		}
	    }
	    if (strncasecmp(value, "FAST_LEMMON", 11) == 0) {
		_use_lemmon = TRUE;
		_use_fast_dark = TRUE;
		if (strncasecmp(value, "FAST_LEMMON_NOWT", 16) == 0) {
		    _use_weights = FALSE;
		}
	    }
	}

	value = parseParamString(point_method, "RAD_FRAME");
	if (value != NULL) {
	    if (strncasecmp(value, "DARK", 4) == 0) {
		_dark_frame_only = TRUE;
	    }
	}
    }

    // Check for dark done onboard.  Onboard means no bias, storage, or smear.
    // (active still applies!)

    _dark_done_onboard = FALSE;
    if (onboard_shutter)
	_dark_done_onboard = TRUE;

    // Check for smear.  It's on if dark is on but not onboard, or it can be
    // forced on or off via a parameter.  (overall dark must be on though)

    _use_smear = _use_dark;
    if (_dark_done_onboard)
	_use_smear = FALSE;

    if (_use_dark) {
	value = parseParamString(point_method, "SMEAR");
	if (value != NULL) {
	    if (strncasecmp(value, "ON", 2) == 0) {
		_use_smear = TRUE;
	    }
	    if (strncasecmp(value, "OFF", 3) == 0) {
		_use_smear = FALSE;
	    }
	}
    }

    // Read flat field file and parameters

    if (_flat == NULL)
        _flat = new RadiometryCalImage(mission, FALSE);
    read_flat_field_parms(_serial_number, _filter);

    // Read dark current parameters

    if (_use_dark) {

	// Lemmon method

	if (_use_lemmon) {
	    if (!_dark_done_onboard) {		// storage, bias
	        if (_L_beta0 == NULL)
		    _L_beta0 = new RadiometryCalImageDark(mission, TRUE,
								"L_beta0");
	        if (_L_bias_column == NULL)
		    _L_bias_column = new RadiometryCalImageDark(mission, TRUE,
							"L_bias_column");
	    }

	    // active always on

	    if (_L_gamma0 == NULL)
		_L_gamma0 = new RadiometryCalImageDark(mission, TRUE,
								"L_gamma0");
	    if (!_use_fast_dark) {		// only needed for slow form
	        if (_L_gamma1 == NULL)
		    _L_gamma1 = new RadiometryCalImageDark(mission, TRUE,
								"L_gamma1");
	        if (_L_gamma2 == NULL)
		    _L_gamma2 = new RadiometryCalImageDark(mission, TRUE,
								"L_gamma2");
	    }
	}

	// Shaw method

	else {

            if (_dark_H == NULL)
                _dark_H = new RadiometryCalImageDark(mission, TRUE, "H");
            if (_dark_LowPoints == NULL)
                _dark_LowPoints = new RadiometryCalImageDark(mission, FALSE,
								"LowPoints");
            if (_dark_Q0 == NULL)
                _dark_Q0 = new RadiometryCalImageDark(mission, TRUE, "Q0");
            if (_dark_Q1 == NULL)
                _dark_Q1 = new RadiometryCalImageDark(mission, TRUE, "Q1");
            if (_dark_Q2 == NULL)
                _dark_Q2 = new RadiometryCalImageDark(mission, TRUE, "Q2");

	}

        read_dark_current_parms(_serial_number, _filter);
    }

    // See if binning correction should be applied

    _use_binning_correction = FALSE;
    _use_dark_binning_correction = FALSE;	// needs the above on also

    if (_is_ssi || _is_rac)
	_use_binning_correction = TRUE;

    if (count != 0) {			// getParam() called above
	value = parseParamString(point_method, "NOBINNING");
	if (value != NULL) {
	    _use_binning_correction = FALSE;
	}
	value = parseParamString(point_method, "FORCE_BINNING");
	if (value != NULL) {		// turn it on anyway, even for non-SSI
	    _use_binning_correction = TRUE;
	}
    }

    // If binning correction is on, apply it

    _binning_correction = 1.0;

    if (_use_binning_correction) {
	if (_hscale == 2 && _vscale == 2)
	    _binning_correction = _binning_2x2;
	else if (_hscale == 3 && _vscale == 3)
	    _binning_correction = _binning_3x3;
	else if (_hscale == 4 && _vscale == 1)
	    _binning_correction = _binning_4x1;
	else if (_hscale == 1 && _vscale == 4) {
	    _binning_correction = _binning_1x4;
	    _use_dark_binning_correction = TRUE;
	}
	else if (_hscale == 4 && _vscale == 4) {
	    if ((instrument_mode_id != NULL) &&
		      (strcmp(instrument_mode_id, "4X1SUMMATION_FRAME") == 0)) {
	        _binning_correction = _binning_4x4;
		_use_dark_binning_correction = TRUE;
	    }
	    else
		_binning_correction = _binning_4x4SW;
	}
	// else 1.0
    }

    if (_is_rac)			// precompute IFOV reference
	_rac_ref_ifov = RAC_FOCUS_TERM(_rac_focus_ref);
}

////////////////////////////////////////////////////////////////////////
// Destructor
////////////////////////////////////////////////////////////////////////

RadiometryPHX::~RadiometryPHX()
{
    if (_host_id) delete _host_id;
    if (_filter) delete _filter;
    if (_serial_number) delete _serial_number;
}

////////////////////////////////////////////////////////////////////////
// This factory method creates and returns an instance of this
// subclass for the camera associated with the given file (determined
// by the labels).
// It could be a constructor instead, but this allows us to return NULL.
////////////////////////////////////////////////////////////////////////

RadiometryPHX *RadiometryPHX::create(PigFileModel *file)
{
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

    // RAC cover state
    const char *state = file->getInstrumentDeploymentState();
    int cover = 0;		// default open
    if (state != NULL && (strcasecmp(state, "CLOSED") == 0))
	cover = 1;

    // now do the actual creation by calling the constructor
    return new RadiometryPHX(file->getMissionName(),
			     file->getInstrumentHostId(),
			     file->getInstrumentId(),
			     file->getFilterNumber(),
    /*convert to seconds*/   file->getExposureDuration(1000.0) / 1000.0,
			     file->getInstrumentTemperature(0),
			     file->getInstrumentTemperatureStart(0),
			     file->getInstrumentTemperatureEnd(0),
			     file->getInstrumentElectronicsTemperature(0),
			     file->getInstrumentOpticsTemperature(0),
			     file->getOffsetNumber(4095),
			     sl, ss, el, es, hscale, vscale,
			     file->getInstrumentModeId(),
			     file->getInstrumentFocalLengthCount(6),
			     cover,
			     file->getShutterEffectCorrectionFlag(FALSE));
}

////////////////////////////////////////////////////////////////////////

void RadiometryPHX::print()

{
    char msg[256];

    RadiometryModel::print();
    sprintf(msg, "Filter Setting: %s", _filter == NULL ? "NULL" : _filter);
    printInfo(msg);
    sprintf(msg, "Exposure time: %f", _exptime);
    printInfo(msg);
    sprintf(msg, "Tave: %f", _Tave);
    printInfo(msg);
    sprintf(msg, "Ts: %f", _Ts);
    printInfo(msg);
    sprintf(msg, "Te: %f", _Te);
    printInfo(msg);
    sprintf(msg, "Tp: %f", _Tp);
    printInfo(msg);
    sprintf(msg, "Toptics: %f", _Toptics);
    printInfo(msg);
    sprintf(msg, "Tbar: %f", _Tbar);
    printInfo(msg);
    sprintf(msg, "Video offset: %d", _video_offset);
    printInfo(msg);
    if (_is_rac) {
        sprintf(msg, "Ref IFOV term: %f", _rac_ref_ifov);
        printInfo(msg);
	sprintf(msg, "Focus step: %d", _focus_step);
	printInfo(msg);
	sprintf(msg, "IFOV term: %f", RAC_FOCUS_TERM(_focus_step));
	printInfo(msg);
	sprintf(msg, "RAC cover: %s", _cover ? "closed" : "open");
	printInfo(msg);
    }
    if (_dark_done_onboard)
	printInfo("Onboard Dark Current: TRUE");
    else
	printInfo("Onboard Dark Current: FALSE");
    if (_use_dark) {
	printInfo("Dark current: on");
        if (_use_lemmon) {
	    if (_use_fast_dark)
	        printInfo("Dark current method: Fast Lemmon");
	    else
	        printInfo("Dark current method: Slow Lemmon");
	    if (_use_weights)
		printInfo("Dark Tbar weighting on");
	    else
		printInfo("Dark Tbar weighting off");
	    sprintf(msg, "Dark current Tbar: %f",
		calculate_Tbar(_exptime, _Ts, _Te, _use_weights) + 273.15);
	    printInfo(msg);
        }
        else 
	    printInfo("Dark current method: Shaw");
	if (_use_dark_binning_correction)
	    printInfo("Dark binning correction: on");
	else
	    printInfo("Dark binning correction: off");
    }
    else
	printInfo("Dark current: off");

    if (_use_binning_correction) {
	sprintf(msg, "Binning correction: %f", _binning_correction);
	printInfo(msg);
    }
    else
	printInfo("Binning correction: off");
    sprintf(msg, "Overall Responivity: %f", getResponsivity(0));
    printInfo(msg);
    if (_use_smear) {
	printInfo("Smear correction: on");
	sprintf(msg, "Smear constant: %f", _exptime * 100000 / _vscale);
	printInfo(msg);
    }
    else
	printInfo("Smear correction: off");

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

void RadiometryPHX::applyCorrectionInternal(void *image, int max_nl, int max_ns,
				int is_float, int band)
{
    float *flat;
    int flat_width, flat_height;
    int i, j;
    double dn;
    double scale_factor = 1;

    double delta_bias = 0.0;
    double mean_bias = 0.0;
    double bias_row = 0.0;
    double storage_area_dc_factor = 0.0;
    double active_area_dc_factor = 0.0;
    double dark_adj = 0.0;		// final adjustment

    double L_storage_factor = 0.0;
    double L_tbar = 0.0;
    double L_active_X = 0.0;
    double L_active_X2 = 0.0;
    double L_active_scalar_factor = 0.0;
    double L_active_temp_factor = 0.0;
    double L_mean_bias = 0.0;
    double L_bias_row = 0.0;

    short int *image_int = (short int *)image;
    float *image_float = (float *)image;

//!!!! print();		//!!!!
    // Get values from subclasses

    double responsivity = getResponsivity(band);
    double dnscale = getDnScalingFactor();
    double dnoff = getDnScalingOffset();

    /////////////////////////////////////////////////////
    // RAC uses Focus Step as "filter" for filename.  Others use filter.
    if (_is_rac) {
	char focus_str[100];
	sprintf(focus_str, "%d", _focus_step);
        _flat->loadFile(_serial_number, focus_str, NULL);
    }
    else
        _flat->loadFile(_serial_number, _filter, NULL);

    /////////////////////////////////////////////////////
    // Precompute dark current factors that don't depend on line/sample

    if (_use_dark) {

	if (_use_lemmon) {

	    // Load files.  They're cached if possible.

	    if (!_dark_done_onboard) {		// storage, bias
                _L_beta0->loadFile(_serial_number, _filter, NULL);
                _L_bias_column->loadFile(_serial_number, _filter, NULL);
	    }
            _L_gamma0->loadFile(_serial_number, _filter, NULL);
	    if (!_use_fast_dark) {
                _L_gamma1->loadFile(_serial_number, _filter, NULL);
                _L_gamma2->loadFile(_serial_number, _filter, NULL);
	    }

	    // Active dark

	    // Calculate tbar, the average temperature for active_area

	    L_tbar = calculate_Tbar(_exptime, _Ts, _Te, _use_weights) + 273.15;

	    L_active_X = 1.0 / L_tbar - 1.0 / 273.15;
	    L_active_X2 = L_active_X * L_active_X;
	    L_active_scalar_factor = exp(_L_gamma1_s * L_active_X +
					 _L_gamma2_s * L_active_X2);
	    L_active_temp_factor = pow((L_tbar / 273.15), 1.5);

	    if (!_dark_done_onboard) {

	        // Shutter (storage) dark

	        L_storage_factor = exp(-_L_beta1 *
					(1.0/(_Te+273.15) - 1.0/273.15));

	        // Bias dark

	        L_mean_bias = (4095.0 - _video_offset) / 2.0 +
		    _L_bias_coef[0] + _L_bias_coef[1] *
			exp(-_L_bias_coef[2] * (1.0/(_Tp+273.15) - 1.0/273.15));
	    }

	}
	else {

	    // Load files.  They're cached if possible.

            _dark_H->loadFile(_serial_number, _filter, NULL);
            _dark_LowPoints->loadFile(_serial_number, _filter, NULL);
            _dark_Q0->loadFile(_serial_number, _filter, NULL);
            _dark_Q1->loadFile(_serial_number, _filter, NULL);
            _dark_Q2->loadFile(_serial_number, _filter, NULL);

	    // Mean bias

	    delta_bias = 0.5 * (4095 - _video_offset);

	    mean_bias = _dark_b0 +
		(_dark_b1 * exp(-_dark_b2/(_Tp + 273.15))) + delta_bias;

	    // Storage area dark current factor

	    storage_area_dc_factor = _dark_c0 * exp(-_dark_c1 / (_Te+273.15));

	    // Active area dark current factor

	    active_area_dc_factor = _exptime * _dark_beta0 *
					exp(-_dark_beta1/(_Tave + 273.15));
        }
    }

    int is_right = (strcasecmp(_instrument, "SSI_RIGHT") == 0);

    /////////////////////////////////////////////////////
    // Compute first part of dark current (bias and storage).  Result goes
    // into "dark_image" array, even if dark current is off (to make later
    // steps easier).
    // active dark is computed later

    int nl = (_el - _sl + 1);
    int ns = (_es - _ss + 1);

    double *dark_image = new double[nl * ns];
    if (dark_image == NULL) {
	printError("***ERROR*** Unable to allocate dark_image array!!!");
	printError("***NO RADIOMETRIC CORRECTION APPLIED!!!***");
	return;				// bad !!!!
    }

    for (i=0; i < (_el - _sl + 1); i++) {

	if (_use_dark) {

	    if (_use_lemmon) {

		L_bias_row = 0.0;

		if (!_dark_done_onboard) {
		    // Based on CCD orientation, thus the RIGHT correction
	            int row_number = i;
		    // Note: bias_column_R is pre-flipped, so no longer needed
	            // if (is_right)
		    //     row_number = FLAT_NL_PHX / _vscale  - i - 1;

		    // L_bias_column is 1 row by 1024 columns...
		    for (int jj = 0; jj < _vscale; jj++) {
		        L_bias_row += _L_bias_column->getValue(
				0, 0, row_number * _vscale + jj + _sl);
		    }
		    L_bias_row /= _vscale;
		    L_bias_row += L_mean_bias;
		}

	    } else {			// Shaw

		bias_row = 0.0;

		if (!_dark_done_onboard) {
	            int row_number = (i*_vscale) + _sl;
	            if (is_right)
		        row_number = FLAT_NL_PHX - ((i*_vscale)+_sl) - 1;

	            // Final bias level.  Based on CCD orientation, thus the
	            // RIGHT correction

	            bias_row = mean_bias + _dark_a0 + _dark_a1 *
				pow((row_number + _dark_offset), _dark_a2);
		}
	    }
	}

	for (j=0; j < (_es - _ss + 1); j++) {

	    // Apply dark-current correction

	    if (_use_dark) {

		if (_use_lemmon) {

		    double L_storage_dark = 0.0;
		    if (!_dark_done_onboard) {
		        L_storage_dark = getFlatFieldDN(_L_beta0, i, j) *
					L_storage_factor;
		    }

		    dark_adj = L_storage_dark +  L_bias_row;

		}
		else {			// Shaw method

		    // Storage area dark current

		    double storage_area_dc = 0.0;
		    if (!_dark_done_onboard) {
		        double Te_min = _Te;
		        double min_temp = getFlatFieldDN(_dark_LowPoints,i,j);
		        if (Te_min < min_temp)
		            Te_min = min_temp;

		        double F_T = getFlatFieldDN(_dark_Q0, i, j) +
		             getFlatFieldDN(_dark_Q1, i, j) * Te_min +
		             getFlatFieldDN(_dark_Q2, i, j) * Te_min * Te_min;
		        storage_area_dc = F_T * storage_area_dc_factor;
		    }

		    // Total adjustment

		    dark_adj = bias_row + storage_area_dc;
		}
	    }
	    else
		dark_adj = 0.0;

	    if (_use_dark_binning_correction)
		dark_adj /= 4.0;	// Shutter dark binning compensation

	    if (is_float)
		dn = IMAGEF(i,j);
	    else
		dn = IMAGE(i,j);

	    dark_image[i*ns + j] = dn - dark_adj;

	    if (_dark_frame_only)
		dark_image[i*ns + j]  = dark_adj;	// no image or flat

	}
    }

    /////////////////////////////////////////////////////
    // Phase 2 computes active dark, as well as smear.  This is because
    // smear is based on the dark-corrected image *without* active.
    // We also compute flat field and responsivity here, and scaling, as
    // with any other mission.

    // Smear works by adding all values in the column from the pixel to
    // the top of the image (right eye) or bottom of the image (left eye).
    // Therefore, each successive line simply adds more to the smear.
    // In order to compute this efficiently, we swap the loop order for
    // left vs. right eye, and keep a running total in smear_buffer.
    // Fortunately, none of the rest of the dark calcs care about order.

    double smear_constant = _exptime * 100000 / _vscale;

    double *smear_buffer = new double[ns];
    if (smear_buffer == NULL) {
	printError("***ERROR*** Unable to allocate smear_buffer array!!!");
	printError("***NO RADIOMETRIC CORRECTION APPLIED!!!***");
	delete dark_image;
	return;				// bad !!!!
    }
    for (j=0; j < ns; j++)
	smear_buffer[j] = 0.0;

    for (int ii=0; ii < (_el - _sl + 1); ii++) {
	i = ii;			// for Right, standard order
        if (!is_right)
	    i = nl - 1 - i;	// for Left, reverse order

	for (j=0; j < (_es - _ss + 1); j++) {

	    if (_use_dark) {

		double smear = 0.0;

		if (_use_smear) {

	            // Apply smearing correction
		    // Smear works by summing all pixels from here to the edge
		    // (top or bottom based on eye), adjusting by a constant,
		    // and subtracting the result.  Note that this pixel is
		    // NOT included.

		    smear = smear_buffer[j] / smear_constant;

		    // Add this pixel into the smear_buffer.  Note, it is the
		    // *unsmeared* (post-correction) pixel.

		    smear_buffer[j] += dark_image[i*ns + j] - smear;

		}

	        // Apply active area dark correction

		if (_use_lemmon) {

		    double L_active_dark;
		    if (_use_fast_dark)
			L_active_dark = getFlatFieldDN(_L_gamma0, i, j) *
				L_active_temp_factor *
				L_active_scalar_factor * _exptime;
		    else
			L_active_dark = getFlatFieldDN(_L_gamma0, i, j) *
				L_active_temp_factor *
				exp(getFlatFieldDN(_L_gamma1,i,j)* L_active_X +
				    getFlatFieldDN(_L_gamma2,i,j)* L_active_X2)*
				_exptime;

		    dark_adj = L_active_dark + smear;

		}
		else {			// Shaw method

		    // Active area dark current

		    double active_area_dc = getFlatFieldDN(_dark_H, i, j)
					 	* active_area_dc_factor;

		    // Total adjustment

		    dark_adj = active_area_dc + smear;
		}
	    }
	    else
		dark_adj = 0.0;


	    // Apply flat field correction

	    float flat_dn = getFlatFieldDN(_flat, i, j);
	    if (flat_dn == 0)
		flat_dn = 1.0;

	    dn = (dark_image[i*ns + j] - dark_adj) / (flat_dn * responsivity);

	    if (_dark_frame_only)	// no image or flat
		dn = (dark_image[i*ns + j] - dark_adj) / responsivity;

            // Apply scaling (should be 0 and 1 for floats, but it's
            // applied anyway to make sure labels are consistent)

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

    delete smear_buffer;
    delete dark_image;

}

////////////////////////////////////////////////////////////////////////
// Calculate tbar, Lemmon's average temperature.  Used for Lemmon's
// active_area (with weights, usually) and for responsivity (without
// weight).
////////////////////////////////////////////////////////////////////////

double RadiometryPHX::calculate_Tbar(double exptime, double Ts, double Te,
							int use_weights)
{
    double t_c = 70.0 / 0.00512;		// converted to counts
    double t_exp = exptime / 0.00512;		// converted to counts
    double delta_T0 = (Te - Ts) / (exp(-(t_exp+1000.)/t_c) - 1.0);
			// the 1000 compensates for storage readout time
    double T_inf = Ts - delta_T0;
    double total_temp = 0.0;
    double total_wt = 0.0;
    for (int step=0; step < 21; step++) {
	double t = step / 20.0 * t_exp;
	double Temp = T_inf + delta_T0 * exp(-t/t_c);
	double Tk = Temp + 273.15;
	double wt = 1.0;
	if (use_weights)
	    wt = pow((Tk/273.15), 1.5) *
				exp(-7310. * (1.0/Tk - 1.0/273.15));
	total_temp += Temp * wt;
	total_wt += wt;
    }
    return total_temp / total_wt;
}

////////////////////////////////////////////////////////////////////////
// return flat field's dn value for a given sample/line pair
////////////////////////////////////////////////////////////////////////
double RadiometryPHX::getFlatFieldDN(RadiometryCalImage *img, int line,int samp)
{
    double newDN = 0.0;
    for (int i = 0; i < _hscale; i++) {
        for (int j = 0; j < _vscale; j++) {
	    newDN = newDN + img->getValue(0, line*_vscale + j + _sl,
					     samp*_hscale + i + _ss);
	}
    }
    return newDN/(_hscale*_vscale);
}

////////////////////////////////////////////////////////////////////////
// Return the responsivity coefficient given the temperature and exposure 
// time.
// This coefficient is divided into the input DN to give a number with the
// units watts/(meter**2,steradian,nm).
//
// RAC is complicated.  First, the responsivity function units are in
// (DN/s)/(W/m^2/ster/um) rather than for SSI, (W/m^2/ster/nm)/(DN/s).
// So, we have to convert um to nm, and invert it.  Also, the FOV changes
// with focus, which affects the steradian term, so we adjust for that as
// well.
////////////////////////////////////////////////////////////////////////

double RadiometryPHX::getResponsivityFactor(int band)
{
    if (_is_rac) {

	// Convert Toptics back to motor counts

	double T = (_Toptics - _rac_temp_term) / _rac_temp_factor;
	double R;
	if (_cover)			// True == closed
	    R = _resp_closed[0] + _resp_closed[1]*T + _resp_closed[2]*T*T;
	else
	    R = _resp_open[0] + _resp_open[1]*T + _resp_open[2]*T*T;
	R *= 1000.0;		// convert from um to nm

	// Scale by IFOV (ratio of steradians)
	double ratio = RAC_FOCUS_TERM(_focus_step) / _rac_ref_ifov;
	R = R * ratio * ratio;

	// Invert R compared to "normal"

	return (_binning_correction * R);
    }

    // else... SSI, or maybe OM
    // use Tbar instead of Tave per Lemmon, 3/11/08
    return (_binning_correction) /
			(_resp[0] + _resp[1]*_Tbar + _resp[2]*_Tbar*_Tbar);
}
double RadiometryPHX::getResponsivity(int band)
{   
    return _exptime * getResponsivityFactor(band);
}



////////////////////////////////////////////////////////////////////////
// Read in the calibration parameters for a given flat field file.
////////////////////////////////////////////////////////////////////////

void RadiometryPHX::read_flat_field_parms(char *sn, char *filter)
{
    char filename[PIG_MAX_FILENAME_SIZE];
    FILE *inClientFile;
    char line[255];
    char responsivity_parms[100];

    // Assign default values
    // THESE ARE A NO-OP.  Replace with real values when we ge them.
    _resp[0] = 1.0;
    _resp[1] = 0.0;
    _resp[2] = 0.0;

    // RAC only
    _resp_open[0] = 9331.0;
    _resp_open[1] = -0.031107;
    _resp_open[2] = -0.00016447;
    _resp_closed[0] = 8043.7;
    _resp_closed[1] = -0.099472;
    _resp_closed[2] = -0.00013100;
    _rac_temp_factor = 0.08293;
    _rac_temp_term = -273.643;
    _rac_focus_ref = 6;
    _rac_focus_Xi = 25.517;
    _rac_focus_delta = .041733;
    _rac_focus_scale_term = 2.944;


    _binning_2x2 = 4.0;
    _binning_3x3 = 2.25;
    _binning_4x1 = 1.3333333333;
    _binning_1x4 = 4.0;
    _binning_4x4SW = 1.7777777778;
    _binning_4x4 = 5.3333333333;

    strcpy(responsivity_parms, "\0");

    if (sn)
        sprintf(filename, "param_files/PHX_SN_%s_flat_fields.parms", sn);
    else
	sprintf(filename, "param_files/PHX_flat_fields.parms");

    // open the file
    inClientFile = PigModelBase::openConfigFile(filename, NULL);

    if (inClientFile == NULL) {
        sprintf(line, 
		"Flat field parameters file %s could not be opened, using default values",
		filename);
	printWarning(line);
    }
    else {

	// This is the ONLY filter line we're looking for:
	if (filter != NULL)
	    sprintf(responsivity_parms, "F_%s_responsivity = %%lf %%lf %%lf", filter);
	else
	    sprintf(responsivity_parms, "responsivity = %%lf %%lf %%lf");

        short int found = 0;
	while (fgets(line, sizeof(line), inClientFile) != NULL) {

	    if (strncasecmp(line, responsivity_parms, 12) == 0) {
	        found = 1;
	        sscanf(line, responsivity_parms,
		       &_resp[0], &_resp[1], &_resp[2]);
	    }
	    if (strncasecmp(line, "responsivity_open", 17) == 0) {
	        found = 1;
	        sscanf(line, "responsivity_open = %lf %lf %lf",
		       &_resp_open[0], &_resp_open[1], &_resp_open[2]);
	    }
	    if (strncasecmp(line, "responsivity_closed", 19) == 0) {
	        found = 1;
	        sscanf(line, "responsivity_closed = %lf %lf %lf",
		       &_resp_closed[0], &_resp_closed[1], &_resp_closed[2]);
	    }

	    if (strncasecmp(line, "rac_temp_factor", 15) == 0)
		sscanf(line, "rac_temp_factor = %lf", &_rac_temp_factor);
	    if (strncasecmp(line, "rac_temp_term", 13) == 0)
		sscanf(line, "rac_temp_factor = %lf", &_rac_temp_term);
	    if (strncasecmp(line, "rac_focus_ref", 13) == 0)
		sscanf(line, "rac_focus_ref = %d", &_rac_focus_ref);
	    if (strncasecmp(line, "rac_focus_Xi", 12) == 0)
		sscanf(line, "rac_focus_Xi = %lf", &_rac_focus_Xi);
	    if (strncasecmp(line, "rac_focus_delta", 15) == 0)
		sscanf(line, "rac_focus_delta = %lf", &_rac_focus_delta);
	    if (strncasecmp(line, "rac_focus_scale_term", 20) == 0)
		sscanf(line, "rac_focus_scale_term = %lf", &_rac_focus_scale_term);

	    if (strncasecmp(line, "binning_2x2", 11) == 0)
		sscanf(line, "binning_2x2 = %lf", &_binning_2x2);
	    if (strncasecmp(line, "binning_3x3", 11) == 0)
		sscanf(line, "binning_3x3 = %lf", &_binning_3x3);
	    if (strncasecmp(line, "binning_4x1", 11) == 0)
		sscanf(line, "binning_4x1 = %lf", &_binning_4x1);
	    if (strncasecmp(line, "binning_1x4", 11) == 0)
		sscanf(line, "binning_1x4 = %lf", &_binning_1x4);
	    if (strncasecmp(line, "binning_4x4SW", 13) == 0)
		sscanf(line, "binning_4x4SW = %lf", &_binning_4x4SW);
	    else if (strncasecmp(line, "binning_4x4", 11) == 0)
		sscanf(line, "binning_4x4 = %lf", &_binning_4x4);

	}

	fclose(inClientFile);
	// report if no data found
	if (!found) {
	    sprintf(line, 
		    "Responsivity parameters were not found in %s, using default values!",
		    filename);
	    printWarning(line);
	}
    }
}

////////////////////////////////////////////////////////////////////////
// Read in the calibration parameters for a given dark current file.
////////////////////////////////////////////////////////////////////////

void RadiometryPHX::read_dark_current_parms(char *sn, char *filter)
{
    char filename[PIG_MAX_FILENAME_SIZE];
    FILE *inClientFile;
    char line[255];

    // Assign default values
    // These are from S/N 111 (flight SSI left) for lack of anything better.
    _dark_b0 = 0.61845885;
    _dark_b1 = 71022.889;
    _dark_b2 = 2345.4564;
    _dark_a0 = -3.7935359;
    _dark_a1 = 1.6284367;
    _dark_a2 = 0.15896048;
    _dark_offset = 20.000000;
    _dark_c0 = 9.0016369e+16;
    _dark_c1 = 10401.257;
    _dark_beta0 = 6.1654121e+13;
    _dark_beta1 = 7996.0854;

    _L_beta1 = 9892.21;
    _L_gamma1_s = -7312.40;	// Mean of the file... not from Lemmon
    _L_gamma2_s = 242853.4;	// ditto
    _L_bias_coef[0] = -4.49345;
    _L_bias_coef[1] = 20.6043;
    _L_bias_coef[2] = 1874.91;

    if (sn)
        sprintf(filename, "param_files/PHX_SN_%s_dark_current.parms", sn);
    else
	sprintf(filename, "param_files/PHX_dark_current.parms");

    // open the file
    inClientFile = PigModelBase::openConfigFile(filename, NULL);

    if (inClientFile == NULL) {
        sprintf(line, 
		"Dark current parameters file %s could not be opened, using default values",
		filename);
	printWarning(line);
    }
    else {

	while (fgets(line, sizeof(line), inClientFile) != NULL) {

	    // pull out the parameters

	    // Shaw parameters

	    if (strncasecmp(line, "b0", 2) == 0)
		sscanf(line, "b0 = %lf", &_dark_b0);
	    if (strncasecmp(line, "b1", 2) == 0)
		sscanf(line, "b1 = %lf", &_dark_b1);
	    if (strncasecmp(line, "b2", 2) == 0)
		sscanf(line, "b2 = %lf", &_dark_b2);

	    if (strncasecmp(line, "a0", 2) == 0)
		sscanf(line, "a0 = %lf", &_dark_a0);
	    if (strncasecmp(line, "a1", 2) == 0)
		sscanf(line, "a1 = %lf", &_dark_a1);
	    if (strncasecmp(line, "a2", 2) == 0)
		sscanf(line, "a2 = %lf", &_dark_a2);
	    if (strncasecmp(line, "offset", 2) == 0)
		sscanf(line, "offset = %lf", &_dark_offset);

	    if (strncasecmp(line, "c0", 2) == 0)
		sscanf(line, "c0 = %lf", &_dark_c0);
	    if (strncasecmp(line, "c1", 2) == 0)
		sscanf(line, "c1 = %lf", &_dark_c1);

	    if (strncasecmp(line, "beta0", 5) == 0)
		sscanf(line, "beta0 = %lf", &_dark_beta0);
	    if (strncasecmp(line, "beta1", 5) == 0)
		sscanf(line, "beta1 = %lf", &_dark_beta1);

	    // Lemmon parameters

	    if (strncasecmp(line, "L_beta1", 7) == 0)
		sscanf(line, "L_beta1 = %lf", &_L_beta1);
	    if (strncasecmp(line, "L_gamma1_s", 10) == 0)
		sscanf(line, "L_gamma1_s = %lf", &_L_gamma1_s);
	    if (strncasecmp(line, "L_gamma2_s", 10) == 0)
		sscanf(line, "L_gamma2_s = %lf", &_L_gamma2_s);
	    if (strncasecmp(line, "L_bias_coef", 11) == 0)
		sscanf(line, "L_bias_coef = %lf %lf %lf",
			&_L_bias_coef[0], &_L_bias_coef[1], &_L_bias_coef[2]);
	}

	fclose(inClientFile);
    }
}

////////////////////////////////////////////////////////////////////////
// Model name for label.  Depends on dark current type.
////////////////////////////////////////////////////////////////////////
const char *const RadiometryPHX::getModelLabelName()
{
    if (!_use_lemmon)
	return "MIPLRAD";

    // must be Lemmon
    if (_use_fast_dark)
	return "MIPLRAD3";

    // default...
    return "MIPLRAD2";

}

