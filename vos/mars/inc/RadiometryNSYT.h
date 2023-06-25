////////////////////////////////////////////////////////////////////////
// RadiometryNSYT
//
// subclass of Radiometry model for NSYT cameras.
////////////////////////////////////////////////////////////////////////
#ifndef RADIOMETRYNSYT_H
#define RADIOMETRYNSYT_H

#include <string.h>

#include "RadiometryModel.h"
#include "RadiometryCalImage.h"

#define NSYT_MAX_FLAT_CACHE	4

class RadiometryNSYT: public RadiometryModel {

  protected:

    char _host_id[10];
    char _instrument[32];
    char _filter[10];
    int  _isFFonBoardApplied;
    int  _isOnboardBoostApplied;
    float _temperature;
    int _hscale, _vscale;  // downsampled images factors
    double _resp[3];
    double _color_resp[3][3]; // indices: [RGB][012]
    float _flat_parms[5];
    float _preboost_parms[3];
    char *_ff_file;
    char *_ff_desc;
    char *_mode;

    // static variables to hold cache of flat field buffer & name
    static RadiometryCalImage *_flat_cache[NSYT_MAX_FLAT_CACHE];
    static char *_flat_cache_names[NSYT_MAX_FLAT_CACHE];
    static int _flat_cache_init;

    RadiometryNSYT();

    // Read in the calibration pointing parameters for rad correction

    void read_flat_field_parms(char *filename);

    // return flat field's dn value for a given sample/line pair
    float getFlatFieldDN(RadiometryCalImage *img, int band, int i, int j);

    // Apply radiometric correction to an image
    // NSYT does not have onboard flat capability, so no need to undo that
    // (unlike MSL).  However, it DOES have onboard color balancing, so we
    // have to remove that before applying the radiometric correction (thus
    // it's the same rad correction whether the image is raw or jpeg).
//!!!! TBD: THE ONBOARD RESP CORRECTION IS NOT YET IMPLEMENTED
    // Note: band is 0-based
    virtual void applyCorrectionInternal(void *image, int max_nl, int max_ns,
					int is_float, int band, int hasBeenRad);

  public:

    static RadiometryNSYT *create(PigFileModel *file);

    RadiometryNSYT(const char *mission, const char *host_id, 
		  const char *instrument, const char *filter,
		  const char *isFFonBoardApplied, float *flat_parms,
		  float *preboost_parms, float exptime, float temperature, 
		  int sl, int ss, int el, int es,
		  int hscale, int vscale, const char *mode, int color,
                  float solar_elevation, float tau);

    virtual ~RadiometryNSYT();

    // These are overridden from the base class to return NSYT specific
    // values

    virtual int loadFlatField(const char *mode);

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
    // The (0-based) band number is ignored unless _color is true.

    virtual double getResponsivity(int band);
    virtual double getResponsivityFactor(int band);

    // Converts from default physical units which are
    // watts/(meter**2, steradian,micron) to some
    // other physical units.  For NSYT we convert from
    // microns to nanometers: watts/(meter**2, steradian, nm)
    virtual double getUnitScalingFactor() { return 1000.0; }
    virtual const char *const getUnitLabelName() { return "WATT*M**-2*SR**-1*NM**-1"; }

    // Return flat field filename.  Dark current filename is not used.

    virtual char *getFlatFieldFile(int i) { return _ff_file; }
    virtual char *getFlatFieldDesc(int i) { return _ff_desc; }

    // Print the model, for debugging
    virtual void print();

    virtual const char *const getModelName() { return "RadiometryNSYT"; }
};

#endif

