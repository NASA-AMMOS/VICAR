////////////////////////////////////////////////////////////////////////
// PigConfigMgrTempPHX
//
// Class to manage configuration data for Phoenix geometric temperature
// compensation.
//
// This simply means it reads the PHX_SN_xxx_geom_temp.params files and
// returns the values on request.
//
// Static methods are used to maintain a list of already-read files.
// An instance of this class is returned for use.
//
// Note that all data members of the instance are public; it's just too
// much trouble for this application to write lots of accessors.  Make
// sure not to modify the values because the instances are reused often.
////////////////////////////////////////////////////////////////////////
#ifndef PIGCONFIGMGRTEMPPHX_H
#define PIGCONFIGMGRTEMPPHX_H

#include "PigModelBase.h"

#define MAX_PHX_CAMERA_SN 20		// actually there are only 8
					// and only 3 are used (null, SSI-L/R)

class PigConfigMgrTempPHX : public PigModelBase {

protected:

    static int _num_instances;
    static PigConfigMgrTempPHX *_instances[MAX_PHX_CAMERA_SN];
    static char *_instrument_names[MAX_PHX_CAMERA_SN];
    static int _initialized;		// are we initialized?
    static int _use_temp;		// parameter-based disable

    // Only instantiable by the static method in this class
    // The serial_number can be null, which will return a no-correction instance

    PigConfigMgrTempPHX(char *serial_number);

private:
    ~PigConfigMgrTempPHX() { }		// not deleteable!

public:

    ////////////////////////////////////////////////////////////////////////
    // Static factory method  Note that instrument can be NULL which will
    // return an instance suitable for no correction.
    // Note:  filter is not currently used.
    ////////////////////////////////////////////////////////////////////////

    static PigConfigMgrTempPHX *getInstance(const char *mission,
					    const char *host_id,
					    const char *instrument,
					    const char *filter);

    ////////////////////////////////////////////////////////////////////////
    // Data fields are all public for expediency.  DO NOT MODIFY!!!
    ////////////////////////////////////////////////////////////////////////

    double _align_x[3];		// Alignment compensation coefficients
    double _align_y[3];

    double _mrad_per_pixel;	// Convert the above from pixels to angle

    double _scale_x[3];		// Scale compensation coefficients
    double _scale_y[3];

    virtual const char *const getModelName() { return "PigConfigMgrTempPHX"; }

};

#endif

