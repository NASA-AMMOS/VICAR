////////////////////////////////////////////////////////////////////////
// RadiometryColdarm
//
// subclass of Radiometry model for Coldarm cameras.
////////////////////////////////////////////////////////////////////////
#ifndef RADIOMETRYCOLDARM_H
#define RADIOMETRYCOLDARM_H

#include <string.h>

#include "RadiometryModel.h"
#include "RadiometryCalImage.h"

#define COLDARM_MAX_FLAT_CACHE	500
#define COLDARM_NUM_SLOTS	5

class RadiometryColdarm: public RadiometryModel {

  protected:

    char _host_id[20];
    char _instrument[32];
    char _filter[20];
    int  _isFFonBoardApplied;
    float _temperature;
    int _hscale, _vscale;  // downsampled images factors
    double _resp[3];
    double _color_resp[3][3]; // indices: [RGB][012]
    float _flat_parms[5];
    char *_ff_file[COLDARM_NUM_SLOTS];
    char *_ff_desc[COLDARM_NUM_SLOTS];
    char *_mode;
    int _nominal_bits;
    int _sample_bits;
    // For ZCAM
    int _zoom_pos;
    int _focus_pos;
    double _tref;
    int _use_zcam_tcomp;
    int _flat_filter_ref;
    int _use_zcam_flat;
    int _onboard_shutter_flag;
    double _pcb_temp;
    int _dc_offset;
    double _bias;
    double _exp_overhead;

    // Pre-cached flats
    int _slot[COLDARM_NUM_SLOTS]; // For zcam, 0=rad, 1=fzoom_canon, 2=fzooom_real
		// (zcam) 3=static bias 4=dynamic bias
		// For others, only 0 is used

    // static variables to hold cache of flat field buffer & name
    static RadiometryCalImage *_flat_cache[COLDARM_MAX_FLAT_CACHE];
    static char *_flat_cache_names[COLDARM_MAX_FLAT_CACHE];
    static int _flat_cache_init;

    RadiometryColdarm();

    // Read in the calibration pointing parameters for rad correction

    void read_flat_field_parms(char *filename);

    // Read in the var_flat file
    char *readVarFlat(const char *fn, int &zoom_value);

    // return flat field's dn value for a given sample/line pair
    float getFlatFieldDN(RadiometryCalImage *img, int band, int i, int j);

    // Apply radiometric correction to an image
    // Note that since Coldarm is capable of doing certain 
    // radiometric correction on-board, we need to undo 
    // that correction first and then do our own rad correction
    // based on calibrated flat fields.
    // Note: band is 0-based
    virtual void applyCorrectionInternal(void *image, int max_nl, int max_ns,
					int is_float, int band, int hasBeenRad);

    // Load the flat field files.  May be more than one for ZCAM
    virtual void loadFlatFields(const char *mode);

    // Load one flat field file, of the given type
    virtual int loadFlatField(const char *mode, const char *type, int *pos,
				int index);

  public:

    static RadiometryColdarm *create(PigFileModel *file);

    RadiometryColdarm(const char *mission, const char *host_id, 
		  const char *instrument, const char *filter,
		  const char *isFFonBoardApplied, float *flat_parms,
		  float exptime, float temperature, 
		  int sl, int ss, int el, int es,
		  int hscale, int vscale, const char *mode, int color,
                  float solar_elevation, float tau,
		  int sample_bits, int zoom_pos, int focus_pos,
		  int onboard_shutter, float pcb_temp, int dc_offset);

    virtual ~RadiometryColdarm();

    // These are overridden from the base class to return Coldarm specific
    // values

    virtual int loadFlatField(const char *mode)
	{ return loadFlatField(mode, NULL, NULL, 0); }

    // Return the responsivity coefficient given the temperature and exposure 
    // time.
    // This coefficient is divided into the input DN to give a number with the
    // units watts/(meter**2,steradian,micron).
    //
    // As with MER/MSL/M20, Coldarm responsivities are the *inverse* of the way it
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
    // other physical units.  For Coldarm we convert from
    // microns to nanometers: watts/(meter**2, steradian, nm)
    virtual double getUnitScalingFactor() { return 1000.0; }
    virtual const char *const getUnitLabelName()
			{ return "WATT*M**-2*SR**-1*NM**-1"; }

    // Return flat field filename.  Dark current filename is not used.

    virtual int getNumFlatFieldFiles()
	{ if (_use_zcam_flat) return 5;
	  return 1; }

    virtual char *getFlatFieldFile(int i) { return _ff_file[i]; }
    virtual char *getFlatFieldDesc(int i) { return _ff_desc[i]; }

    // Print the model, for debugging
    virtual void print();

    virtual const char *const getModelName() { return "RadiometryColdarm"; }
};

#endif

