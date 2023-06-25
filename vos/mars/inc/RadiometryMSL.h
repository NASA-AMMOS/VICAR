////////////////////////////////////////////////////////////////////////
// RadiometryMSL
//
// subclass of Radiometry model for MSL cameras.
////////////////////////////////////////////////////////////////////////
#ifndef RADIOMETRYMSL_H
#define RADIOMETRYMSL_H

#include <string.h>

#include "RadiometryModel.h"
#include "RadiometryCalImage.h"

#define MSL_MAX_FLAT_CACHE	20

class RadiometryMSL: public RadiometryModel {

  protected:

    char _host_id[10];
    char _instrument[32];
    char _filter[10];
    int  _isFFonBoardApplied;
    float _temperature;
    int _hscale, _vscale;  // downsampled images factors
    double _resp[3];
    double _color_resp[3][3]; // indices: [RGB][012]
    float _flat_parms[5];
    char *_ff_file;
    char *_ff_desc;
    char *_mode;

    // static variables to hold cache of flat field buffer & name
    static RadiometryCalImage *_flat_cache[MSL_MAX_FLAT_CACHE];
    static char *_flat_cache_names[MSL_MAX_FLAT_CACHE];
    static int _flat_cache_init;

    RadiometryMSL();

    // Read in the calibration pointing parameters for rad correction

    void read_flat_field_parms(char *filename);

    // return flat field's dn value for a given sample/line pair
    float getFlatFieldDN(RadiometryCalImage *img, int band, int i, int j);

    // Apply radiometric correction to an image
    // Note that since MSL is capable of doing certain 
    // radiometric correction on-board, we need to undo 
    // that correction first and then do our own rad correction
    // based on calibrated flat fields.
//!!!! IS THAT TRUE FOR MSL?
    // Note: band is 0-based
    virtual void applyCorrectionInternal(void *image, int max_nl, int max_ns,
					int is_float, int band, int hasBeenRad);

  public:

    static RadiometryMSL *create(PigFileModel *file);

    RadiometryMSL(const char *mission, const char *host_id, 
		  const char *instrument, const char *filter,
		  const char *isFFonBoardApplied, float *flat_parms,
		  float exptime, float temperature, 
		  int sl, int ss, int el, int es,
		  int hscale, int vscale, const char *mode, int color,
                  float solar_elevation, float tau);

    virtual ~RadiometryMSL();

    // These are overridden from the base class to return MSL specific
    // values

    virtual int loadFlatField(const char *mode);

    // Return the responsivity coefficient given the temperature and exposure 
    // time.
    // This coefficient is divided into the input DN to give a number with the
    // units watts/(meter**2,steradian,micron).
    //
    // As with MER, MSL responsivities are the *inverse* of the way it was 
    // done for MPF and other missions prior to MER.  Since we want to keep 
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
    // other physical units.  For MSL we convert from
    // microns to nanometers: watts/(meter**2, steradian, nm)
    virtual double getUnitScalingFactor() { return 1000.0; }
    virtual const char *const getUnitLabelName() { return "WATT*M**-2*SR**-1*NM**-1"; }

    // Return flat field filename.  Dark current filename is not used.

    virtual char *getFlatFieldFile(int i) { return _ff_file; }
    virtual char *getFlatFieldDesc(int i) { return _ff_desc; }

    // Print the model, for debugging
    virtual void print();

    virtual const char *const getModelName() { return "RadiometryMSL"; }
};

#endif

