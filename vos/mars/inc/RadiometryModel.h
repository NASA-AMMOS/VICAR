////////////////////////////////////////////////////////////////////////
// RadiometryModel
//
// Base class for Radiometry models.  Responsible for maintaining calibration
// information for a camera and applying radiometric correction.
//
// This class derives from PigModelBase even though it's not named Pig...
// because it needs many of the same services and follows the same
// architecture.
//
////////////////////////////////////////////////////////////////////////
#ifndef RADIOMETRYMODEL_H
#define RADIOMETRYMODEL_H

#include "PigModelBase.h"

#define PIG_MAX_FLAT_FIELD_INDEX 5

#define PIG_MAX_VAR_FLAT_FILES 200

class PigFileModel;

class RadiometryModel : public PigModelBase {

  protected:
    char *_mission;		
    char *_instrument;
//!!!! const char *_calpathname;  // eventually we'll add this...
    int _sl, _ss, _el, _es;	// where the physical image buffer sits
				// relative to the flat field file coordinates

    double _dnscale_factor;     // This value should be used if you'd like to 
                                // scale floating point numbers to integer.

    int _do_scaled_rad;         // A flag that determines whether the current
                                // radiometry model is a regular RAD or scaled
                                // RAD model. Note: different algorithms wil be
                                // applied between regular and scaled RAD models.
                                
    float _solar_elevation;
    float _tau;                 // Tau is a measure of atmospheric opacity.
    double _flat_darkening_factor;

    double _exptime;

    int _color;

    // The create() method above should normally be used...
    RadiometryModel();
    RadiometryModel(const char *mission, const char *instrument,
						int sl, int ss,
						int el, int es);

    // Band is 0-based
    virtual void applyCorrectionInternal(void *image, int max_nl, int max_ns,
					int is_float, int band, 
                                        int hasBeenRad);

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
    // After calling this, if you're using RadiometryCalFile, you'll still need
    // to call cache[slot]->loadFile().  This should be a no-op on a cache hit
    // (since it also checks the filename), but it will load the file on a cache
    // miss or overflow.
    ////////////////////////////////////////////////////////////////////////

    virtual int getCacheSlot(char *name, char *cache_names[], int cache_max);

  public:

    static RadiometryModel *create(const char *filename);
    static RadiometryModel *create(PigFileModel *file);

    virtual ~RadiometryModel();

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

    static void *setAllDnscalingFactor(RadiometryModel *radiometric[], 
                                       PigFileModel *files[], int nids);

    // Access functions
    virtual const char *getMissionName() const { return _mission; }
    virtual const char *getInstrument() const { return _instrument; }

    // Apply radiometric correction to an image
    // This is the whole purpose of this class.

    // Band is 0-based
    virtual void applyCorrection(short int *image, int max_nl, int max_ns,
			         int band, int hasBeenRad)
	{ applyCorrectionInternal((void *)image, max_nl, max_ns, FALSE, band, 
                                   hasBeenRad); }
    virtual void applyCorrection(float *image, int max_nl, int max_ns, int band,
                                 int hasBeenRad)
	{ applyCorrectionInternal((void *)image, max_nl, max_ns, TRUE, band, 
                                  hasBeenRad); }

    ////////////////////////////////////////////////////////////////////
    // The following flat field and responsivity functions should be
    // overridden by subclasses to do something appropriate

    // Return a flat-field image.  This is a 2-D array of float which is
    // divided into each corresponding pixel of the input (input/flat).
    // Returns TRUE if the flat-field is present, FALSE if not.

    virtual int loadFlatField(float *&flat, int &width, int &height)
	{ flat = NULL;  width = 0;  return FALSE; }

    // Informs the subclass that we're done with the FF loaded above.

    virtual void freeFlatField(float *flat) { }

    // Return the responsivity of the camera.  This is often used for
    // temperature and exposure compensation but can be anything.  Every
    // pixel is divided by this value.  This is defined as a factor which
    // is divided into the input DN in order to give a number with the
    // units watts/(meter**2,steradian,micron).

    virtual double getResponsivity(int band) { return 1.0; }

    // Get the responsivity factor.  This is generally what appears in
    // the cal file, as modified by temperature or other factors.
    // It does not include exposure time (getResponsivity() does).

    virtual double getResponsivityFactor(int band) { return 1.0; }

    // Return the factor used to convert rad to zenith scaled rad. 
    // The factor is a combination of solar elevation and tau, and will have 
    // the effect of brightening up images that were taken near subset.

    virtual double getZenithFactor();

    // Tau reference is used to correct the zenith factor. It defaults to
    // 0.3, and missions can override the default. This value can also be 
    // supplied by using POINT_METHOD=TAU_REFERENCE=XXX from command line.
    
    virtual double getDefaultTauReference() { return 0.3; }
    virtual double getTauReference();
    virtual double getTau() { return _tau; }

    // This factor compensates for the overall darkening of the image that 
    // usually occurs when applying a flat field (which is generally near 1 at 
    // the center and falls off in the corners due to vignetting). It is 
    // generally the average value of the flat field, but need not be exactly 
    // that. 
    
    virtual double getFlatDarkeningFactor() { return _flat_darkening_factor; }

    // Return the DN scaling factor, which convert between the physical
    // units watts/(meter**2,steradian,micron) and DN's sufficient for
    // a halfword (16-bit unsigned) image file.  This should not normally
    // be overridden by subclasses since the same scaling factor should
    // apply globally to all inputs.
    //
    // The formula is:  true_radiance = offset + (factor * DN)
    //
    // The following parameters should be used with this routine (if DNSCALE
    // not present, 100 is used; if DNSCALE_OUT not present, STATIC is used):
    //
    // PARM DNSCALE REAL DEFAULT=100.0
    // PARM DNSCALE_OUT TYPE=KEYWORD COUNT=1 VALID=("STATIC", "DYNAMIC", 
    //      "IDENTITY") DEFAULT=STATIC
    //
    // Static DN scaling uses the value specified by DNSCALE; dynamic DN scaling
    //  uses the maximum responsivity value; identity means no scaling applied.
    //
    // Note that the returned factor is actually the inverse (1/x) of DNSCALE
    // or the maximum responsivity value.

    virtual double getDnScalingFactor() { return _dnscale_factor; }

    // Setter for the DN scaling factor. See getDnScalingFactor() function for 
    // more information.

    virtual void setDnScalingFactor(double factor)
        { _dnscale_factor = factor; }

    // Return TRUE if -ZENITH_SCALED_RAD is enabled, otherwise, FALSE.
    //
    // The following parameter should be used with this routine (if not present,
    // RAD is used):
    //
    // PARM RAD TYPE=KEYWORD VALID=("RAD", "ZENITH_SCALED_RAD", "NORAD") 
    //      DEFAULT=RAD

    virtual int doScaledRad() { return _do_scaled_rad; } 

    // Converts from default physical units which are
    // watts/(meter**2, steradian,micron) to some
    // other physical units.  For example MER uses
    // nanometers instead of microns, so MER's mission specific
    // subclass would apply unit scaling factor.
    virtual double getUnitScalingFactor() { return 1.0; }
    virtual double getDnScalingOffset() { return 0.0; }		// not used
    virtual const char *const getUnitLabelName() { return "WATT*M**-2*SR**-1*uM**-1"; }

    // Return radiometric correction type   

    virtual const char *const getRadiometricType();

    // Get the names and descriptions of cal files for the label.
    // Base class default is to do nothing.  These all return NULL if not
    // valid.  Note that pointers to internal memory or static strings may
    // be returned, so the values should be used immediately or copied - they
    // may not survive object destruction.

    virtual char *getDarkCurrentFile() { return NULL; }
    virtual char *getDarkCurrentDesc() { return NULL; }
    virtual int getNumFlatFieldFiles() { return 1; }
    virtual char *getFlatFieldFile(int i) { return NULL; }
    virtual char *getFlatFieldDesc(int i) { return NULL; }

    // Returns TRUE if the other model is equivalent to this one
    // (i.e. same correction being applied), FALSE otherwise.  Use case
    // is for mosaics, to make sure that if we write a rad label, it applies
    // to all the inputs.

    virtual int isEquivalent(RadiometryModel *other);

    // Returns TRUE if it's a color model

    virtual int isColor() { return _color; }

    ////////////////////////////////////////////////////////////////////

    // Print the model, for debugging
    virtual void print();

    // Name for model type in label
    virtual const char *const getModelLabelName() { return "MIPLRAD"; }

    virtual const char *const getModelName() { return "RadiometryModel"; }
};

#endif

