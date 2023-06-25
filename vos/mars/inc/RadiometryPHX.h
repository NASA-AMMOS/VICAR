////////////////////////////////////////////////////////////////////////
// RadiometryPHX
//
// subclass of Radiometry model for PHX cameras.
//
// Temporary rad model that looks at exposure time only.  Needs updating
// with better information!!!
////////////////////////////////////////////////////////////////////////
#ifndef RADIOMETRYPHX_H
#define RADIOMETRYPHX_H

#include <string.h>

#include "RadiometryModel.h"
#include "RadiometryCalImage.h"

#define FLAT_NL_PHX 1024
#define FLAT_NS_PHX 1024

class RadiometryPHX: public RadiometryModel {

  protected:

    char *_host_id;
    char *_filter;
    char *_serial_number;
    double _Tave;		// average start and end CCD temperature
    double _Ts;			// CCD temperature at start of exposure
    double _Te;			// CCD temperature at end of exposure
    double _Tp;			// temperature of CCD Electronics (PCB)
    double _Toptics;		// temperature of optics (RAC)
    double _Tbar;		// avg temp via Lemmon function (for resp)
    int _video_offset;		// from OFFSET_NUMBER label
    int _hscale, _vscale;  // downsampled images factors
    int _focus_step;		// Focus step (motor count), for RAC
    int _cover;			// 0 = cover open, 1 = closed

    int _is_ssi;		// true iff SSI
    int _is_rac;		// true iff RAC

    int _use_dark;
    int _use_lemmon;		// Lemmon or Shaw model
    int _use_fast_dark;		// Per-pixel exponential in Lemmon, or not
    int _use_weights;		// in Tbar computation for Lemmon
    int _use_binning_correction;
    int _use_dark_binning_correction;
    int _dark_frame_only;	// Dark only, with no image
    int _use_smear;		// Smear correction
    int _dark_done_onboard;	// bias, storage, and smear can be done onboard

    double _binning_correction;

    // Parameters

    double _resp[3];
    double _resp_open[3];
    double _resp_closed[3];
    double _rac_temp_factor;	// counts <-> degrees C
    double _rac_temp_term;
    double _rac_ref_ifov;		// computed.  Not really IFOV due to
					// function reduction

    // These are for computing the RAC IFOV.  See the rac_focus spreadsheet.
    int _rac_focus_ref;		// reference focus motor step for RAC resp
    double _rac_focus_Xi;		// cell C7
    double _rac_focus_delta;		// cell C6
    double _rac_focus_scale_term;	// cell C14 and C15 combined

    // Shaw dark

    double _dark_b0;
    double _dark_b1;
    double _dark_b2;
    double _dark_a0;
    double _dark_a1;
    double _dark_a2;
    double _dark_offset;
    double _dark_c0;
    double _dark_c1;
    double _dark_beta0;
    double _dark_beta1;

    // Lemmon dark

    double _L_beta1;
    double _L_gamma1_s;
    double _L_gamma2_s;
    double _L_bias_coef[3];
 
    // Binning correction

    double _binning_2x2;
    double _binning_3x3;
    double _binning_4x1;
    double _binning_1x4;
    double _binning_4x4SW;
    double _binning_4x4;

    // Cal images

    static RadiometryCalImage *_flat;

    // Shaw dark

    static RadiometryCalImage *_dark_H;
    static RadiometryCalImage *_dark_LowPoints;
    static RadiometryCalImage *_dark_Q0;
    static RadiometryCalImage *_dark_Q1;
    static RadiometryCalImage *_dark_Q2;

    // Lemmon dark

    static RadiometryCalImage *_L_beta0;
    static RadiometryCalImage *_L_gamma0;
    static RadiometryCalImage *_L_gamma1;
    static RadiometryCalImage *_L_gamma2;
    static RadiometryCalImage *_L_bias_column;

    RadiometryPHX();

    // Read in the calibration pointing parameters for rad correction

    void read_flat_field_parms(char *sn, char *filter);
    void read_dark_current_parms(char *sn, char *filter);

    // return flat field's dn value for a given sample/line pair
    double getFlatFieldDN(RadiometryCalImage *img, int i, int j);

    // Apply radiometric correction to an image
    // PHX has no on-board flat fielding but does have dark-current correction.
    // Note: band is 0-based (but unsed for PHX)
    virtual void applyCorrectionInternal(void *image, int max_nl, int max_ns,
					int is_float, int band);

    // Calculate tbar, Lemmon's average temperature.  Used for Lemmon's
    // active_area (with weights, usually) and for responsivity (without
    // weight).
    double calculate_Tbar(double exptime, double Ts, double Te,int use_weights);

  public:

    static RadiometryPHX *create(PigFileModel *file);

    RadiometryPHX(const char *mission, const char *host_id, 
		  const char *instrument, const char *filter,
		  double exptime,
		  double Tave, double Tstart, double Te, double Tp,
		  double Toptics,
		  int video_offset,
		  int sl, int ss, int el, int es,
		  int hscale, int vscale,
		  const char *instrument_mode_id, int focus_step, int cover,
		  int onboard_shutter);

    virtual ~RadiometryPHX();

    // Return the responsivity coefficient given the temperature and exposure 
    // time.
    // This coefficient is divided into the input DN to give a number with the
    // units watts/(meter**2,steradian,micron).
    //
    // According to Justin Maki, Jim Bell has listed the Pancam responsivities
    // as the *inverse* of the way it was done for MPF and other missions 
    // prior to PHX.  Since we want to keep API the same for all missions and
    // and all PHX instruments, we are returning here 1/x - an inverse 
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

    virtual const char *const getModelLabelName();

    virtual const char *const getModelName() { return "RadiometryPHX"; }
};

#endif

