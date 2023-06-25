////////////////////////////////////////////////////////////////////////
// RadiometryMER
//
// subclass of Radiometry model for MER cameras.
////////////////////////////////////////////////////////////////////////
#ifndef RADIOMETRYMER_H
#define RADIOMETRYMER_H

#include <string.h>

#include "RadiometryModel.h"

#define FLAT_NL_MER 1024
#define FLAT_NS_MER 1024

class RadiometryMER: public RadiometryModel {

  protected:

    char _host_id[10];
    char _instrument[32];
    char _filter[10];
    int  _isFFonBoardApplied;
    float _temperature;
    int _hscale, _vscale;  // downsampled images factors
    double _resp[3];
    float _flat_parms[5];
    

    // static variables to hold flat field buffer & name
    static float _flat[FLAT_NL_MER][FLAT_NS_MER];
    static char _flat_tag[256];
    static int _flat_valid;

    RadiometryMER();

    // Read in the calibration pointing parameters for rad correction

    void read_flat_field_parms(char *filename);

    // return flat field's dn value for a given sample/line pair
    float getFlatFieldDN(float *flat, int i, int j);

    // Apply radiometric correction to an image
    // Note that since MER is capable of doing certain 
    // radiometric correction on-board, we need to undo 
    // that correction first and then do our own rad correction
    // based on calibrated flat fields.
    // Note: band is 0-based (but unused for MER)
    virtual void applyCorrectionInternal(void *image, int max_nl, int max_ns,
					int is_float, int band, int hasBeenRad);

  public:

    static RadiometryMER *create(PigFileModel *file);

    RadiometryMER(const char *mission, const char *host_id, 
		  const char *instrument, const char *filter,
		  const char *isFFonBoardApplied, float *flat_parms,
		  float exptime, float temperature, 
		  int sl, int ss, int el, int es,
		  int hscale, int vscale, float solar_elevation, float tau);

    virtual ~RadiometryMER();

    // These are overridden from the base class to return MER specific
    // values

    virtual int loadFlatField(float *&flat, int &width, int &height);

    // Return the responsivity coefficient given the temperature and exposure 
    // time.
    // This coefficient is divided into the input DN to give a number with the
    // units watts/(meter**2,steradian,micron).
    //
    // According to Justin Maki, Jim Bell has listed the Pancam responsivities
    // as the *inverse* of the way it was done for MPF and other missions 
    // prior to MER.  Since we want to keep API the same for all missions and
    // and all MER instruments, we are returning here 1/x - an inverse 
    // responsivity. That way, in calculating rad-corrected value we *multiply*
    // by the responsivity and *divide* by the exposure time.
    virtual double getResponsivity(int band);
    virtual double getResponsivityFactor(int band);

    // Converts from default physical units which are
    // watts/(meter**2, steradian,micron) to some
    // other physical units.  For MER we convert from
    // microns to nanometers: watts/(meter**2, steradian, nm)
    virtual double getUnitScalingFactor() { return 1000.0; }
    virtual const char *const getUnitLabelName() { return "WATT*M**-2*SR**-1*NM**-1"; }
    // Print the model, for debugging
    virtual void print();

    virtual const char *const getModelLabelName() { return "MIPLRAD2"; }

    virtual const char *const getModelName() { return "RadiometryMER"; }
};

#endif

