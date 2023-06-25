////////////////////////////////////////////////////////////////////////
// RadiometryM98RAC
//
// Subclass for Mars98 RAC Radiometry Model.  
// Responsible for maintaining calibration information for a camera 
// and applying radiometric correction when requested.
//
// J. Maki 9 February 1999
//
////////////////////////////////////////////////////////////////////////

#include "RadiometryM98RAC.h"

#include "PigFileModel.h"

#include "zvproto.h"
#include "applic.h"			/* for SUCCESS */

#include <math.h>

float RadiometryM98RAC::_flat[FLAT_NL_M98RAC][FLAT_NS_M98RAC];
char RadiometryM98RAC::_flat_tag[256];
int RadiometryM98RAC::_flat_valid = FALSE;

////////////////////////////////////////////////////////////////////////
// Constructor
////////////////////////////////////////////////////////////////////////

RadiometryM98RAC::RadiometryM98RAC(const char *mission,const char *instrument,
			int focus, float exptime, float temperature,
				int sl, int ss, int el, int es)
		: RadiometryModel(mission, instrument, sl, ss, el, es)

{
    _focus_setting = focus;
    _exptime = exptime;
    _temperature = temperature;
}

RadiometryM98RAC::RadiometryM98RAC()
		: RadiometryModel()
{
    _focus_setting = 0;
    _exptime = 1.0;
    _temperature = 0.0;
}

////////////////////////////////////////////////////////////////////////
// Destructor
////////////////////////////////////////////////////////////////////////

RadiometryM98RAC::~RadiometryM98RAC()
{
}

////////////////////////////////////////////////////////////////////////
// This factory method creates and returns an instance of this
// subclass for the camera associated with the given file (determined
// by the labels).
// It could be a constructor instead, but this allows us to return NULL.
////////////////////////////////////////////////////////////////////////

RadiometryM98RAC *RadiometryM98RAC::create(PigFileModel *file)
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

    // grab the focus step setting
    int focus_setting = file->getInstrumentFocalLengthCount(302);

    // now do the actual creation by calling the constructor

    return new RadiometryM98RAC(file->getMissionName(), "RAC", focus_setting,
			exptime, temperature, sl, ss, el, es);
}

////////////////////////////////////////////////////////////////////////

void RadiometryM98RAC::print()

{
    char msg[256];

    RadiometryModel::print();

    sprintf(msg, "Focus Setting: %d", _focus_setting);
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

int RadiometryM98RAC::loadFlatField(float *&flat, int &width, int &height)
{
    int i, unit, status;
    int nl, ns;
    char flat_field_name[256], flat_field_path[256];
    int focus_settings[] = {0,87,125,153,177,198,217,234,250,265,279,293,300};
    int focus = 0;
    int focus_index = 0;
    int value=1000;
    char msg[512];
    static int inst = 1;

    flat = NULL;
    width = FLAT_NS_M98RAC;
    height = FLAT_NL_M98RAC;

    // Figure out which flat field file to use.  It's different for each focus
    // setting.

    // Find the closest setting value

    for (i=0; i < sizeof(focus_settings) / sizeof(int); i++) {
	if (fabs((double)(focus_settings[i] - _focus_setting)) < value) {
	    focus_index=i;
	    value = (int)fabs((double)(focus_settings[i] - _focus_setting));
	}
    }

    if (focus_settings[focus_index] != _focus_setting) {
	sprintf(msg, "No flat field data for focus setting %d", _focus_setting);
	printWarning(msg);
    }
    focus = focus_settings[focus_index];
    sprintf(msg, "Using flat field for focus setting %d", focus);
    printInfo(msg);

    // Construct the filename

    sprintf(flat_field_name,"flat_fields/flat_field_RAC_step_%03d_flight.vicar",
		focus);

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

	    zvunit(&unit, "rad_m98_rac_flat", inst++,
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
	if (nl != FLAT_NL_M98RAC || ns != FLAT_NS_M98RAC) {
	    sprintf(msg, "Flat field file must be %d lines by %d samples",
				FLAT_NL_M98RAC, FLAT_NS_M98RAC);
	    printError(msg);
	    printWarning("No flat field correction applied");
	    zvclose(unit, "clos_act", "free", NULL);
	    return FALSE;
	}

    	for (i = 0; i < FLAT_NL_M98RAC; i++) {
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
// !!!! FUNCTION???? !!!!
// This coefficient is divided into the input DN to give a number with the
// units watts/(meter**2,steradian,micron).
//!!!! THIS IS A FICTITIOUS RESPONSIVITY - get the real one and put it here !!!!
////////////////////////////////////////////////////////////////////////

double RadiometryM98RAC::getResponsivityFactor(int band)
{
    double responsivity;

//!!!! we don't have a responsivity factor from the SSI/RAC team
//!!!! so use empirical value to match the RAC image DNs with the
//!!!! SSI DNs.

    responsivity = 4135.0;  // empirical value from ATLO images, jnm 12/1/99

    return responsivity;
}

double RadiometryM98RAC::getResponsivity(int band)
{   
    return _exptime * getResponsivityFactor(band);
}


