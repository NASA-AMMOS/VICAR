////////////////////////////////////////////////////////////////////////
// RadiometryModel
//
// Base class for Radiometry Models.  Responsible for maintaining calibration
// information for a camera and applying radiometric correction when requested.
//
// J. Maki 25 January 1999
//
////////////////////////////////////////////////////////////////////////

#include <string.h>
#include <float.h>
#include <stdlib.h>
#include "RadiometryModel.h"
#include "PigMission.h"
#include "PigFileModel.h"
#include "zvproto.h"

////////////////////////////////////////////////////////////////////////
// Constructor
////////////////////////////////////////////////////////////////////////

RadiometryModel::RadiometryModel(const char *mission,
			const char *instrument,
			int sl, int ss,
			int el, int es)

{
    _mission = _instrument = NULL;

    if (mission) _mission = strdup(mission);
    if (instrument) _instrument = strdup(instrument);

    _sl = sl;
    _ss = ss;
    _el = el;
    _es = es;

    _dnscale_factor = 1.0;
    _solar_elevation = 5.0; 
    _tau = 0.6;
    _flat_darkening_factor = 1.0;
    _color = FALSE;
    _exptime = 0.0;

    // This is a flag for applying regular or zenith scaled RAD method.
    // TRUE means the radiometry model is for zenith scaled RAD.
    // FALSE means the radiometry model is for regular RAD.
    _do_scaled_rad = FALSE;
    char rad_method[256];
    int count;
    getParam("RAD", rad_method, &count, 1, 0);
    if (strcmp(rad_method, "ZENITH_SCALED_RAD") == 0) {
        _do_scaled_rad = TRUE;
    }

    
}

RadiometryModel::RadiometryModel()
{
    _mission = _instrument = NULL;
    _sl = _ss = _el = _es = 0;
    _dnscale_factor = 1.0;
    _solar_elevation = 5.0;
    _tau = 0.6;
    _flat_darkening_factor = 1.0;
    _do_scaled_rad = TRUE;
}

////////////////////////////////////////////////////////////////////////
// Destructor
////////////////////////////////////////////////////////////////////////

RadiometryModel::~RadiometryModel()
{
    if (_mission) 
        delete _mission;

    if (_instrument) 
        delete _instrument;

}

////////////////////////////////////////////////////////////////////////
// This factory method creates and returns an instance of the
// proper subclass for the given camera.  Cameras are specified
// by the file itself (look at the label to figure it out).
////////////////////////////////////////////////////////////////////////

RadiometryModel *RadiometryModel::create(const char *filename)
{
    PigFileModel *file = PigFileModel::create(filename);
    if (file == NULL)
	return NULL;
    RadiometryModel *rad = create(file);
    delete file;
    return rad;
}

RadiometryModel *RadiometryModel::create(PigFileModel *file)
{
    PigMission *m = PigMission::getMissionObject(file);
    return m->createRadiometryModel(file);
}

////////////////////////////////////////////////////////////////////////
// Set the DN scaling factor for all radiometric models supplied.
// 
// The following parameters should be used with this routine (if DNSCALE not
// present, 100 is used; if DNSCALE_OUT not present, STATIC is used):
//
// PARM DNSCALE TYPE=REAL DEFAULT=100.0
// PARM DNSCALE_OUT TYPE=KEYWORD COUNT=1 VALID=("STATIC", "DYNAMIC",
//      "IDENTITY") DEFAULT=STATIC
//
// If -STATIC is enabled, then the value specified by DNSCALE will be used
// as the DN scaling factor (note the unit scaling factor as well) for all
// radiometric models supplied.
//
// If -DYNAMIC is enabled, then the maximum responsivity value across all
// radiometric models will be used as the DN scaling factor (note the unit
// scaling factor as well).
//
// If -IDENTITY is enabled, then no scaling will be applied.
////////////////////////////////////////////////////////////////////////

void *RadiometryModel::setAllDnscalingFactor(RadiometryModel *radiometric[], 
                                             PigFileModel *files[], int nids)
{
    int count;
    double static_dnscale;
    short int doDynamicDnScaling = FALSE;
    short int doFloatDnScaling = FALSE;
    short int doStaticDnScaling = FALSE;
    int scale_method_counter = 0;
    char scale_method[1024];

    if (radiometric == NULL) {
        return NULL;
    }

    getStaticParam("DNSCALE_OUT", scale_method, &count, 1, 0);

    if (strcmp(scale_method, "DYNAMIC") == 0) {
        doDynamicDnScaling = TRUE;
    } else if (strcmp(scale_method, "IDENTITY") == 0) {
        doFloatDnScaling = TRUE;
    } else {
        doStaticDnScaling = TRUE;
    }

    for (int i = 0; i < nids; i++) {
        if (radiometric[i] == NULL) {
            continue;
        }

        // TODO: should look at the label!!!
        if (radiometric[i]->doScaledRad()) {
            scale_method_counter++;
        }

        if (doDynamicDnScaling) {
            int nb = files[i]->getNB();
            double max_resp = -DBL_MAX;
  
            for (int b = 0; b < nb; b++) {
                double resp = radiometric[i]->getResponsivity(b);

                if (resp >= max_resp) {
                    max_resp = resp;
                }
            }
            
            // Incorporating flat darkening factor
            max_resp = max_resp * radiometric[i]->getFlatDarkeningFactor();

            radiometric[i]->setDnScalingFactor(max_resp);
        } else if (doFloatDnScaling) {
            radiometric[i]->setDnScalingFactor(1.0);
        } else {
            getStaticParam("DNSCALE", &static_dnscale, &count, 1, 0);
            if (count == 0) {
                static_dnscale = 100.0;
            }
            static_dnscale = static_dnscale * 
                                      radiometric[i]->getUnitScalingFactor();

            radiometric[i]->setDnScalingFactor(static_dnscale);
        }
    }

    // If RAD type does match, issue a warning
    if (scale_method_counter != 0 && scale_method_counter % nids != 0) {
        printStaticMsg("Radiometry types don't match.", PigMsgWarning);
    }

    // Go through all radiometric models again to figure out what is the 
    // max dnscaling factor. 
    double max_dnscaling_factor = -DBL_MAX;
    for (int i = 0; i < nids; i++) {
        if (radiometric[i] == NULL) {
            continue;
        }

        double current_dnscaling_factor = radiometric[i]->getDnScalingFactor();
        if (radiometric[i]->getDnScalingFactor() >= max_dnscaling_factor) {
            max_dnscaling_factor = current_dnscaling_factor;
        }
    }

    // We've found the max dnscaling factor, and now go through all 
    // radiometric models one more time to set the found value.
    for (int i = 0; i < nids; i++) {
        if (max_dnscaling_factor == 0.0 || max_dnscaling_factor == -DBL_MAX) {
            max_dnscaling_factor = 1.0;
        }

        if (radiometric[i] != NULL) {
            radiometric[i]->setDnScalingFactor(1.0 / max_dnscaling_factor);
        }
    }
    return NULL;
}

////////////////////////////////////////////////////////////////////////
// Print the fields of this class.  Subclasses should normally call this,
// then add their own fields.
////////////////////////////////////////////////////////////////////////

void RadiometryModel::print()

{
    char msg[256];

    sprintf(msg, "Mission: %s", _mission);
    printInfo(msg);
    sprintf(msg, "Instrument: %s", _instrument);
    printInfo(msg);
    sprintf(msg, "starting line: %d", _sl);
    printInfo(msg);
    sprintf(msg, "ending line: %d", _el);
    printInfo(msg);
    sprintf(msg, "starting sample: %d", _ss);
    printInfo(msg);
    sprintf(msg, "ending sample: %d", _es);
    printInfo(msg);
    sprintf(msg, "dn scaling factor: %f", _dnscale_factor);
    printInfo(msg);
    if (doScaledRad()) {
        sprintf(msg, "Radiometry correction method: Zenith Scaled RAD");
    } else {
        sprintf(msg, "Radiometry correction method: RAD");
    }
    printInfo(msg);
}

////////////////////////////////////////////////////////////////////////
// Access macros to the image data
////////////////////////////////////////////////////////////////////////

// Assumes "flat" and "flat_width" local variables, and _sl/_ss/_el/_es
// member variables
#define FLAT(line, samp) (*((flat) + ((line)+_sl) * flat_width + ((samp)+_ss)))
// Assumes "image_*", and "max_ns" local variables or member variables
#define IMAGE(line, samp) (*((image_int) + (line) * max_ns + (samp)))
#define IMAGEF(line, samp) (*((image_float) + (line) * max_ns + (samp)))

////////////////////////////////////////////////////////////////////////
// Apply the correction to an image.  max_nl and max_ns represent the
// physical size of the buffer, not the logical extent.  The _sl, _ss,
// _el, and _es member variables define where this physical buffer lies
// in the logical image.  So, an _sl of 2 means that the first line of
// the physical buffer corresponds to the second line of the flat field.
////////////////////////////////////////////////////////////////////////

void RadiometryModel::applyCorrectionInternal(void *image,
		int max_nl, int max_ns, int is_float, int band, int hasBeenRad)
{
    float *flat;
    int flat_width, flat_height;
    int i, j;
    double dn;

    short int *image_int = (short int *)image;
    float *image_float = (float *)image;

    // Get values from subclasses

    int use_flat = loadFlatField(flat, flat_width, flat_height);
    double responsivity = getResponsivity(band);
    double dnscale = getDnScalingFactor();
    double dnoff = getDnScalingOffset();

    for (i=0; i < (_el - _sl + 1); i++) {
	for (j=0; j < (_es - _ss + 1); j++) {

	    // if flat field correction

	    if (is_float)
		dn = IMAGEF(i,j);
	    else
		dn = IMAGE(i,j);

	    if (use_flat && i < flat_height && FLAT(i, j) > 0)
		dn = dn / (FLAT(i,j) * responsivity);
	    else
		dn = dn / responsivity;

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

    freeFlatField(flat);

}

////////////////////////////////////////////////////////////////////////
// Returns TRUE if the other model is equivalent to this one
// (i.e. same correction being applied), FALSE otherwise.  Use case
// is for mosaics, to make sure that if we write a rad label, it applies
// to all the inputs.
// Note: does not compare FF or Dark filenames as those may vary
////////////////////////////////////////////////////////////////////////

#define EPS 1e-3
int RadiometryModel::isEquivalent(RadiometryModel *other)
{
    if (other == NULL)
	return FALSE;
    if (fabs(getDnScalingFactor() - other->getDnScalingFactor()) > EPS)
	return FALSE;
    if (fabs(getDnScalingOffset() - other->getDnScalingOffset()) > EPS)
	return FALSE;
    if (strcmp(getUnitLabelName(), other->getUnitLabelName()) != 0)
	return FALSE;
    if (strcmp(getModelLabelName(), other->getModelLabelName()) != 0)
	return FALSE;

    return TRUE;
}

////////////////////////////////////////////////////////////////////////
// Returns a slot to use from the cache of cal files.  Returns any match
// first, then an empty slot, then if the cache is full, returns
// slot 0 as an overflow.
//
// Initially, the name array must be initialized to all NULL.  This routine
// will update the array as needed.  If you ever flush a slot, set its name
// to NULL in the array.
//
// cache_max is the size of the cache, not the current number in the cache.
//
// After calling this, if you're using RadiometryCalFile, you will still need
// to call cache[slot]->loadFile().  This should be a no-op on a cache hit
// (since it also checks the filename), but it will load the file on a cache
// miss or overflow.
////////////////////////////////////////////////////////////////////////

int RadiometryModel::getCacheSlot(char *name,
				  char *cache_names[], int cache_max)
{
    int i;
    static int overflow_slot = -1;

    // Look for a matching cache entry

    for (i=0; i < cache_max; i++) {
	if (cache_names[i] != NULL && (strcmp(cache_names[i], name) == 0)) {
	    return i;				// found it!
	}
    }

    // No match, look for an open slot

    for (i=0; i < cache_max; i++) {
	if (cache_names[i] == NULL) {
	    cache_names[i] = strdup(name);	// found a slot!
	    return i;
	}
    }

    // Nothing found, go back to the beginning and use the next slot.
    // That way it's at least sorta LRU.  If there's more than one cache
    // they share the same overflow counter but that's okay.

    overflow_slot++;
    if (overflow_slot >= cache_max)
        overflow_slot = 0;
    return overflow_slot;
}

////////////////////////////////////////////////////////////////////////
// Tau reference is used to correct the zenith factor. It defaults to
// 0.3, and missions can override the default. This value can also be
// supplied by using POINT_METHOD=TAU_REFERENCE=XXX from command line.
////////////////////////////////////////////////////////////////////////

double RadiometryModel::getTauReference()
{
    char point_method[256], *value;
    int count;
    getParam("POINT_METHOD", point_method, &count, 1, 0);
    if (count != 0) {
        value = parseParamString(point_method, "TAU_REFERENCE");
 
        if (value != NULL) {
            return atof(value);
        }
    }

    return getDefaultTauReference();
}

////////////////////////////////////////////////////////////////////////
// Return radiometric correction type
////////////////////////////////////////////////////////////////////////

const char *const RadiometryModel::getRadiometricType()
{
    if (doScaledRad()) {
        return "Scaled Spectral Radiance";
    }

    return "Spectral Radiance";
}

////////////////////////////////////////////////////////////////////////
// Return the factor used to convert rad to zenith scaled rad.
// The factor is a combination of solar elevation and tau, and will have
// the effect of brightening up images that were taken near subset.
////////////////////////////////////////////////////////////////////////

double RadiometryModel::getZenithFactor()
{
    double tau_ref = getTauReference();
    double mu = sin(_solar_elevation);

    return mu * exp(-(_tau/6/mu) + (tau_ref/6/mu));
}
