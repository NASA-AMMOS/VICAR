////////////////////////////////////////////////////////////////////////
// RadiometryM98SSI
//
// Subclass for Mars98 SSI Radiometry Model.  
// Responsible for maintaining calibration information for a camera 
// and applying radiometric correction when requested.
//
// J. Maki 4 February 1999
//
////////////////////////////////////////////////////////////////////////

#include "RadiometryM98SSI.h"

#include "PigFileModel.h"

#include "zvproto.h"
#include "applic.h"                     /* for SUCCESS */

#include <stdlib.h>

float RadiometryM98SSI::_flat[FLAT_NL_M98SSI][FLAT_NS_M98SSI];
char RadiometryM98SSI::_flat_tag[256];
int RadiometryM98SSI::_flat_valid = FALSE;

////////////////////////////////////////////////////////////////////////
// Constructor
////////////////////////////////////////////////////////////////////////

RadiometryM98SSI::RadiometryM98SSI(const char *mission,const char *instrument,
				int filter, float exptime, float temperature,
				int sl, int ss, int el, int es)
		: RadiometryModel(mission, instrument, sl, ss, el, es)

{
    _filter = filter;
    _exptime = exptime;
    _temperature = temperature;
}

RadiometryM98SSI::RadiometryM98SSI()
		: RadiometryModel()
{
    _filter = 0;
    _exptime = 1.0;
    _temperature = 0.0;
}

////////////////////////////////////////////////////////////////////////
// Destructor
////////////////////////////////////////////////////////////////////////

RadiometryM98SSI::~RadiometryM98SSI()
{
}

////////////////////////////////////////////////////////////////////////
// This factory method creates and returns an instance of this
// subclass for the camera associated with the given file (determined
// by the labels).
// It could be a constructor instead, but this allows us to return NULL.
////////////////////////////////////////////////////////////////////////

RadiometryM98SSI *RadiometryM98SSI::create(PigFileModel *file)
{
    int sl, ss, el, es;

    sl = file->getFirstLine(0) - 1;		// convert to 0-based
    ss = file->getFirstLineSample(0) - 1;

    if (sl < 0) sl = 0; // clip if negative
    if (ss < 0) ss = 0;

    el = sl + file->getNL() - 1; // compute ending line/sample
    es = ss + file->getNS() - 1;

    // grab the observation time
    float exptime = file->getExposureDuration(1000) / 1000.;   // cvt to seconds

    // grab the CCD temperature
    float temperature = file->getInstrumentTemperature(0);

    // grab the filter number
    int filter;
    const char *filter_string = file->getFilterNumber();
    if (filter_string == NULL)
	filter = 5;
    else
	filter = atoi(filter_string);

    // now do the actual creation by calling the constructor

    const char *frame = file->getFrameId();
    if (frame && frame[0] == 'R') {
	return new RadiometryM98SSI(file->getMissionName(), "SSI Right", filter,
			exptime, temperature, sl, ss, el, es);
    }
    else {		// Defaults to left (msg prob already issued elsewhere)
        return new RadiometryM98SSI(file->getMissionName(), "SSI Left", filter,
			exptime, temperature, sl, ss, el, es);
    }
    return NULL;		// never reached
}

////////////////////////////////////////////////////////////////////////

void RadiometryM98SSI::print()

{
    char msg[256];

    RadiometryModel::print();

    sprintf(msg, "Filter: %d", _filter);
    printInfo(msg);
    sprintf(msg, "Exposure time: %f", _exptime);
    printInfo(msg);
    sprintf(msg, "Temperature: %f", _temperature);
    printInfo(msg);
}
 
////////////////////////////////////////////////////////////////////////
// Load a flat-field image.  The image is cached in a static class variable
// so we don't re-load it on each image.
////////////////////////////////////////////////////////////////////////

int RadiometryM98SSI::loadFlatField(float *&flat, int &width, int &height)
{
    int i, status;
    int unit, ns, nl;
    char flat_field_name[256], flat_field_path[256];;
    static int inst = 1;		// for zvunit
    char msg[256];

    flat = NULL;
    width = FLAT_NS_M98SSI;
    height = FLAT_NL_M98SSI;

    // Construct the filename

    if (strcmp(_instrument,"SSI Left") == 0) {
	sprintf(flat_field_name,"flat_fields/flat_field_SSI_L%02d_flight.vicar",
				_filter);
    }
    else {			// right eye
	sprintf(flat_field_name,"flat_fields/flat_field_SSI_R%02d_flight.vicar",
				_filter);
    }

    // check and see if we need to load the flat field, which is
    // stored in the static variable _flat

    if (!_flat_valid || strcmp(_flat_tag, flat_field_name) != 0) {

	// We need a new flat field

	_flat_valid = FALSE;		// until it loads properly

	// change the tag name to reflect the active flat field
	strcpy(_flat_tag, flat_field_name);

	// Find the file
	status = -1;
	unit = -1;
	FILE *f = PigModelBase::openConfigFile(flat_field_name,flat_field_path);
	if (f != NULL) {
	    fclose(f);
	    sprintf(msg, "loading flat field file %s", flat_field_path);
	    printInfo(msg);

	    // re-open the flat field image using VICAR

	    zvunit(&unit, "rad_m98_ssi_flat", inst++,
				"U_NAME", flat_field_path, NULL);
	    status = zvopen(unit, "U_FORMAT", "REAL",
				"OPEN_ACT", "", "IO_ACT", "S", NULL);
	}

      	if (status != SUCCESS) {
            sprintf(msg, "Unable to open flat field file %s", flat_field_name);
	    printError(msg);
            printWarning("No flat field correction applied");
	    if (unit != -1)
		zvclose(unit, "clos_act", "free", NULL);
	    return FALSE;
        }

	zvget(unit, "NS", &ns, "NL", &nl, NULL);
	if (nl != FLAT_NL_M98SSI || ns != FLAT_NS_M98SSI) {
	    sprintf(msg, "Flat field file must be %d lines by %d samples",
				FLAT_NL_M98SSI, FLAT_NS_M98SSI);
	    printError(msg);
            printWarning("No flat field correction applied");
	    zvclose(unit, "clos_act", "free", NULL);
	    return FALSE;
	}

    	for (i = 0; i < FLAT_NL_M98SSI; i++) {
	    status = zvread(unit, &_flat[i][0], NULL);
      	    if (status != SUCCESS) {
            	sprintf(msg, "I/O error reading flat field file %s", 
		        flat_field_path);
		printError(msg);
        	printWarning("No flat field correction applied");
	        zvclose(unit, "clos_act", "free", NULL);
		return FALSE;
      	    }
        }

	zvclose(unit, "clos_act", "free", NULL);
	_flat_valid = TRUE;
    }

    flat = (float *)_flat;
    return TRUE;
}

////////////////////////////////////////////////////////////////////////
// Return the responsivity coefficient given the temperature and exposure time
// responsivity = R0+R1*T+R2*T*T.  This coefficient is divided into the input
// DN to give a number with the units watts/(meter**2,steradian,micron).
////////////////////////////////////////////////////////////////////////

double RadiometryM98SSI::getResponsivityFactor(int band)
{
    double responsivity;

    // Coefficients for computing responsivity versus temperature, Left camera

     // Pathfinder values, just for reference
     // static double r_left[12][3] = {
     //	{ 126.86, -0.4214, -0.0006 },
     //	{ 500.0,   0.0,     0.0    },
     //	{ 500.0,   0.0,     0.0    },
     //	{ 500.0,   0.0,     0.0    },
     //	{ 500.0,   0.0,     0.0    },
     //	{ 565.9,  -0.5611, -0.0013 },
     //	{ 875.74,  0.2383, -0.0029 },
     //	{ 1224.4,  2.125,   0.003  },
     //	{ 1094.1,  2.9378,  0.0058 },
     //	{ 485.65,  1.956,   0.0051 },
     //	{ 219.1,   1.648,   0.0053 },
     //	{ 401.3,   2.055,   0.0051 }
     //  };
     //  Coefficients for computing responsivity versus temperature, Right camera
     // Pathfinder, just for reference
     // static double r_right[12][3] = {
     // { 132.86, -0.399,  -0.0007 },
     // { 500.0,   0.0,     0.0    },
     // { 500.0,   0.0,     0.0    },
     // { 500.0,   0.0,     0.0    },
     // { 500.0,   0.0,     0.0    },
     // { 557.84, -0.5751, -0.0014 },
     // { 803.26, -0.2517, -0.0019 },
     // { 7596.9,  9.057,  -0.0235 },
     // { 594.0,  -0.5998, -0.0013 },
     // { 592.78, -0.9146, -0.0021 },
     // { 361.0,  -0.6556, -0.0019 },
     // { 402.8,   2.237,   0.0066 }
     // };

     // Mars98 SSI values for left eye
     // JNM 24 September 1999
     static double r_left[12][3] = {
        { 107.97, -0.37735,-0.0032 },
        { 0.0386, -0.00021, 0.0000 },
        { 0.20300, 0.00061, 0.0000 },
        { 0.08176, 0.00041, 0.0000 },
        { 0.32055, 0.00148, 0.0000 },
        { 390.21, -0.28913,-0.0010 },
        { 711.83,  0.77076, 0.00072},
        { 1246.30, 2.4354,  0.00343},
        { 1032.40, 3.0012,  0.00616},
        { 482.27,  1.9725,  0.00505},
        { 160.66,  1.1840,  0.00361},
        { 383.33,  2.0299,  0.00561}
    };

     // Mars98 SSI values for right eye
     // JNM 24 September 1999
     static double r_right[12][3] = {
        {  71.168,-0.24210,-0.00025},
        { 0.03053,-0.00008, 0.0000 },
        { 0.37185, 0.00212, 0.0000 },
        { 0.32038, 0.00194, 0.0000 },
        { 0.02560, 0.00021, 0.0000 },
        { 379.23, -0.20451,-0.00113},
        { 424.42, -0.01427,-0.00206},
        { 5866.1, 8.2026,  -0.03255},
        { 478.64, -0.60987,-0.00152},
        { 445.49, -0.85335,-0.00299},
        { 198.13, -0.48432,-0.00189},
        { 359.12,  1.8608,  0.00467}
    };

    // compute the responsivity

    if (strcmp(_instrument,"SSI Left") == 0)

	responsivity =  r_left[_filter][0] +
			r_left[_filter][1] * _temperature +
			r_left[_filter][2] * _temperature * _temperature;	

    else
        responsivity =  r_right[_filter][0] +
                        r_right[_filter][1] * _temperature +
                        r_right[_filter][2] * _temperature * _temperature;    

    // precompute the combined responsivity exposure factor

    return responsivity;
}

double RadiometryM98SSI::getResponsivity(int band)
{   
    return _exptime * getResponsivityFactor(band);
}


