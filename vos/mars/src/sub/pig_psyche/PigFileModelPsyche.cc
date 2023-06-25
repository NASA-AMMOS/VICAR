////////////////////////////////////////////////////////////////////////
// PigFileModelPsyche
//
// Psyche-specific File model.  This only needs to handle exceptions to the
// multimission label API.
////////////////////////////////////////////////////////////////////////

#include "PigFileModelPsyche.h"
#include "PigCSReference.h"

#include "zvproto.h"
#include "applic.h"

#include "return_status.h"

////////////////////////////////////////////////////////////////////////
// Constructor
////////////////////////////////////////////////////////////////////////

PigFileModelPsyche::PigFileModelPsyche(const char *filename, 
				 int unit,
				 const char *mission)
		        : PigFileModel(filename, unit, mission)
{
    // overwrites parent's offsets.  For MSL offsets are always 0
    setupOffsets();
}

PigFileModelPsyche::~PigFileModelPsyche()
{
}

////////////////////////////////////////////////////////////////////////
// Initializes the x/y offsets, which are the offsets between the camera
// model L/S coordinates, and the "physical" coordinates in the image.
// Note that these physical coordinates are 0-based, e.g. you have to
// add 1 for VICAR files.
//
// For Psyche x and y offsets are always 0
////////////////////////////////////////////////////////////////////////

void PigFileModelPsyche::setupOffsets()
{
    _x_offset = 0;
    _y_offset = 0;
}

////////////////////////////////////////////////////////////////////////
// These functions return complete Label API property structures.
// OVERRIDES of base class to change the names
////////////////////////////////////////////////////////////////////////

PIG_READ_LABEL_STRUCTURE(LblInstrumentState_typ, PigFileModelPsyche,
	InstrumentState, "InstrumentState", InstrumentState,
	"INSTRUMENT_STATE_PARMS")

////////////////////////////////////////////////////////////////////////
// These functions return basic information from the label.
////////////////////////////////////////////////////////////////////////

float PigFileModelPsyche::getInstrumentTemperature(float def)
{						// InstrumentState
    if (_lblInstrumentState == NULL)
        getLblInstrumentState();
    if (_lblInstrumentState == NULL)
        return def;

    char inst_id[32];
    strcpy(inst_id, "\0");
    strcpy(inst_id, getInstrumentId());
    return getInstrumentTemperatureFromName(def, inst_id);
}

////

float PigFileModelPsyche::getInstrumentTemperature(float def, int use_partner)
{
    if (_lblInstrumentState == NULL)
        getLblInstrumentState();
    if (_lblInstrumentState == NULL)
        return def;

    char inst_id[32];
    char partner_inst_id[32];
    strcpy(inst_id, "\0");
    strcpy(inst_id, getInstrumentId());

    if (!strcmp(inst_id, "IMAGER_1"))
	strcpy(partner_inst_id, "IMAGER_2");
    else if (!strcmp(inst_id, "IMAGER_2"))
	strcpy(partner_inst_id, "IMAGER_1");
    else					// no partners
	strcpy(partner_inst_id, inst_id);

    // Look at the interpolation keywords, use them if they exist.
    // If not, return the standard temperature.

    int have_interp_kwds = FALSE;
    float interp_value = 0.0;

    if (_lblCameraModel == NULL)
        getLblCameraModel();
    if (_lblCameraModel != NULL) {
	if (_lblCameraModel->InterpolationMethod.Valid) {
	    if (strcmp(_lblCameraModel->InterpolationMethod.Value, "ONBOARD")
									== 0) {
	        if (_lblCameraModel->InterpolationValue.Valid) {
		    interp_value = _lblCameraModel->InterpolationValue.Value;
		    have_interp_kwds = TRUE;
		}
	    }
	}
    }

    // Sanity check... same as in m20edrgen

    if (interp_value > 50 || interp_value < -128)
	have_interp_kwds = FALSE;

    if (!have_interp_kwds) {		// Keywords not there
	if (use_partner)
	    return getInstrumentTemperatureFromName(def, partner_inst_id);
	else
	    return getInstrumentTemperatureFromName(def, inst_id);
    }

    float my_temp = getInstrumentTemperatureFromName(def, inst_id);
    float partner_temp = getInstrumentTemperatureFromName(def, partner_inst_id);

#define TEMPERATURE_EPSILON 1e-3
    if ((fabs(interp_value - my_temp) < TEMPERATURE_EPSILON) ||
	(fabs(interp_value - partner_temp) < TEMPERATURE_EPSILON)) {
	if (use_partner)
	    return getInstrumentTemperatureFromName(def, partner_inst_id);
	else
	    return getInstrumentTemperatureFromName(def, inst_id);
    }

    return interp_value;
}


// PRIVATE temperature routine used only by the above

float PigFileModelPsyche::getInstrumentTemperatureFromName(
				float def, const char *inst_id)
{

    if (_lblInstrumentState == NULL)
        getLblInstrumentState();
    if (_lblInstrumentState == NULL)
        return def;

    int index = 0;

    // choose the correct index
    if (!strcmp(inst_id, "IMAGER_1"))
        index = 0;
    else if (!strcmp(inst_id, "IMAGER_2"))
        index = 1;
    else {  //camera has not been recognized, assign default
        index = 0;
    }

    if (_lblInstrumentState->InstrumentTemperature[index].Valid &&
       _lblInstrumentState->InstrumentTemperature[index].Value != 0.0 &&
       _lblInstrumentState->InstrumentTemperature[index].Value < 50.0) {
            return _lblInstrumentState->InstrumentTemperature[index].Value;
    }

    // if we are here, no valid entry has been found, return the default
    //printMsg("WARNING: Unable to find any valid Temperature readings, will use the default value", 
    //         PigMsgWarning);
    return def;
}

////////////////////////////////////////////////////////////////////////
// Get the Rover Motion Counter for the file.  The array must be supplied.
// num_index on input should contain the max size of the supplied array;
// on output it will contain the number of elements actually found (which
// could be bigger than the supplied max size, but only that many will
// actually be returned in the array).  PIG_MAX_RMC_INDEX (from
// PigRoverStateManager.h) can be useful for array dimensioning.
////////////////////////////////////////////////////////////////////////

void PigFileModelPsyche::getRoverMotionCounter(int indices[], int &num_indices)
{   
    int max_indices = num_indices;
    num_indices = 0;

    const LblIdentification_typ *lblIdentification = getLblIdentification();

    //getSite
    if(!lblIdentification->RoverMotionCounter[0].Valid) {
        printWarning("SITE not found in label!  0 assumed");
        indices[0] = 0;
    }
    else {
      indices[0] = lblIdentification->RoverMotionCounter[0].Value;
      num_indices++;
    }

    //getDrive
    if(!lblIdentification->RoverMotionCounter[1].Valid) {
        printWarning("DRIVE not found in label!  0 assumed");
        indices[1] = 0;
    }
    else {
      indices[1] = lblIdentification->RoverMotionCounter[1].Value;
      num_indices++;
    }

    return;
}

////////////////////////////////////////////////////////////////////////
// Returns the nominal number of RMC elements for this mission, or 0
// if none.  A given image may not have all elements specified; this
// call returns the max.
////////////////////////////////////////////////////////////////////////
int PigFileModelPsyche::getRoverMotionCounterCount()
{
    return 2;
}

////////////////////////////////////////////////////////////////////////
// Returns the string names of each RMC element.  String is statically
// allocated so user need not free.  Returns NULL if index out of range.
////////////////////////////////////////////////////////////////////////
char *PigFileModelPsyche::getRoverMotionCounterName(int index)
{
    static char *names[] = {
        "site", "drive"
    };

    if (index < 0 || index >= getRoverMotionCounterCount())
	return NULL;

    return names[index];
}

