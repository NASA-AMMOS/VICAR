////////////////////////////////////////////////////////////////////////
// PigFileModelPHX
//
// PHX-specific File model.  This only needs to handle exceptions to the
// multimission label API.
////////////////////////////////////////////////////////////////////////

#include "PigFileModelPHX.h"
#include "PigCSReference.h"

#include "zvproto.h"
#include "applic.h"

#include "return_status.h"

////////////////////////////////////////////////////////////////////////
// Constructor
////////////////////////////////////////////////////////////////////////

PigFileModelPHX::PigFileModelPHX(const char *filename, 
				 int unit,
				 const char *mission)
		        : PigFileModel(filename, unit, mission)
{
  _phxLblSsiArticulation = NULL;
  _phxLblRacArticulation = NULL;
  _phxLblRaArticulation = NULL;

    // overwrites parent's offsets.  For PHX offsets are always 0
    setupOffsets();
}

PigFileModelPHX::~PigFileModelPHX()
{
  
    if (_phxLblSsiArticulation)
	    delete _phxLblSsiArticulation;
    if (_phxLblRacArticulation)
	    delete _phxLblRacArticulation;
    if (_phxLblRaArticulation)
	    delete _phxLblRaArticulation;
}

////////////////////////////////////////////////////////////////////////
// Initializes the x/y offsets, which are the offsets between the camera
// model L/S coordinates, and the "physical" coordinates in the image.
// Note that these physical coordinates are 0-based, e.g. you have to
// add 1 for VICAR files.
//
// For PHX x and y offsets are always 0
////////////////////////////////////////////////////////////////////////

void PigFileModelPHX::setupOffsets()
{
    _x_offset = 0; 
    _y_offset = 0;		
}
////////////////////////////////////////////////////////////////////////
// These functions return complete Label API property structures.
////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////
// PhxLblSsiArticulation
////////////////////////////////////////////////////////////////////////

const LblArticulation_typ *PigFileModelPHX::getPhxLblSsiArticulation()
{
    if (_phxLblSsiArticulation != NULL)
	return _phxLblSsiArticulation;

    openFile();

    _phxLblSsiArticulation = new LblArticulation_typ;
    if (_phxLblSsiArticulation == NULL) {
	printMsg("Memory error", "Surface Stereo Imager", PigMsgError);
	return NULL;
    }

    int status=PhxLblSsiArticulation(_unit, LBL_READ, 
				     _phxLblSsiArticulation, 1);
    if (RTN_FAILURE(status)) {
	printMsg((char *)LblErrorMessage(), 
		 "PhxLblSsiArticulation", PigMsgError);
	return NULL;
    }

    return _phxLblSsiArticulation;
}
////////////////////////////////////////////////////////////////////////
// PhxLblRacArticulation
////////////////////////////////////////////////////////////////////////

const LblArticulation_typ *PigFileModelPHX::getPhxLblRacArticulation()
{
    if (_phxLblRacArticulation != NULL)
	return _phxLblRacArticulation;

    openFile();

    _phxLblRacArticulation = new LblArticulation_typ;
    if (_phxLblRacArticulation == NULL) {
	printMsg("Memory error", "Robotic Arm Camera Articulation", PigMsgError);
	return NULL;
    }

    int status=PhxLblRacArticulation(_unit, LBL_READ, 
				     _phxLblRacArticulation, 1);
    if (RTN_FAILURE(status)) {
	printMsg((char *)LblErrorMessage(), 
		 "PhxLblRacArticulation", PigMsgError);
	return NULL;
    }

    return _phxLblRacArticulation;
}
////////////////////////////////////////////////////////////////////////
// PhxLblRaArticulation
////////////////////////////////////////////////////////////////////////

const LblArticulation_typ *PigFileModelPHX::getPhxLblRaArticulation()
{
    if (_phxLblRaArticulation != NULL)
	return _phxLblRaArticulation;

    openFile();

    _phxLblRaArticulation = new LblArticulation_typ;
    if (_phxLblRaArticulation == NULL) {
	printMsg("Memory error", "Robotic Arm Articulation", PigMsgError);
	return NULL;
    }

    int status=PhxLblRaArticulation(_unit, LBL_READ, 
				     _phxLblRaArticulation, 1);
    if (RTN_FAILURE(status)) {
	printMsg((char *)LblErrorMessage(), 
		 "PhxLblRaArticulation", PigMsgError);
	return NULL;
    }

    return _phxLblRaArticulation;
}

PIG_READ_LABEL_STRUCTURE(LblInstrumentState_typ, PigFileModelPHX,
        InstrumentState, "InstrumentState", InstrumentState,
        "INSTRUMENT_STATE_PARMS")


////////////////////////////////////////////////////////////////////////
// These functions return basic information from the label.
////////////////////////////////////////////////////////////////////////

// Scalar string item
#define FILE_LABEL_S(Property,fName,Name)				\
	const char * PigFileModelPHX::get##fName()			\
	{								\
	    if (_phxLbl##Property == NULL)				\
		getPhxLbl##Property();					\
	    if (_phxLbl##Property == NULL)				\
		return NULL;						\
	    if (!_phxLbl##Property->Name.Valid)	       	        	\
		return NULL;						\
	    return _phxLbl##Property->Name.Value;			\
	}

// Scalar numeric item
#define FILE_LABEL(Property,fName,Name,Type)				\
	Type PigFileModelPHX::get##fName(Type def)			\
	{								\
	    if (_phxLbl##Property == NULL)				\
		getPhxLbl##Property();					\
	    if (_phxLbl##Property == NULL)				\
		return def;						\
	    if (!_phxLbl##Property->Name.Valid)	       	        	\
		return def;						\
	    return _phxLbl##Property->Name.Value;			\
	}

// Item from numeric array
#define FILE_LABEL2(Property,Name,Type,ItemName)			\
	Type PigFileModelPHX::get##Name(Type def)			\
	{								\
	    if (_lbl##Property == NULL)				        \
		getLbl##Property();					\
	    if (_lbl##Property == NULL)				        \
		return def;						\
	    if (!_lbl##Property->ItemName.Valid)	       		\
		return def;						\
	    return _lbl##Property->ItemName.Value;		        \
	}
// Array item, converted to type "Type".  Type must have ctor for Value's type.
#define FILE_LABEL3(Property,Name,Type)				        \
	Type PigFileModelPHX::get##Name(Type def)			\
	{								\
	    if (_phxLbl##Property == NULL)				\
		getPhxLbl##Property();					\
	    if (_phxLbl##Property == NULL)				\
		return def;						\
	    if (!_phxLbl##Property->Name.Valid)	       		        \
		return def;						\
	    return Type(_phxLbl##Property->Name.Value);       		\
	}

// This macro is specific to the triplet of Value/Valid pair
// ie validation is done for every subfield value.  The type
// should be a triplet such as PigVector or PigPoint.
#define FILE_LABEL4(Property,Name,Type)					\
	Type PigFileModelPHX::get##Name(Type def)			\
	{								\
	    if (_phxLbl##Property == NULL)		       		\
		getPhxLbl##Property();					\
	    if (_phxLbl##Property == NULL)				\
		return def;						\
	    if (!(_phxLbl##Property->Name[0].Valid &&                   \
		  _phxLbl##Property->Name[1].Valid &&                   \
		  _phxLbl##Property->Name[2].Valid ))			\
		return def;						\
	    return Type(_phxLbl##Property->Name[0].Value,               \
		        _phxLbl##Property->Name[1].Value,               \
			_phxLbl##Property->Name[2].Value);	        \
	}
// This macro is specific to the quadruplet of Value/Valid pair
// ie validation is done for every subfield value.  The type
// should be a quadruplet such as PigQuaternion.
#define FILE_LABEL5(Property,Name,Type)					\
	Type PigFileModelPHX::get##Name(Type def)			\
	{								\
	    if (_phxLbl##Property == NULL)				\
		getPhxLbl##Property();					\
	    if (_phxLbl##Property == NULL)				\
		return def;						\
	    if (!(_phxLbl##Property->Name[0].Valid &&                   \
		  _phxLbl##Property->Name[1].Valid &&                   \
		  _phxLbl##Property->Name[2].Valid &&                   \
		  _phxLbl##Property->Name[3].Valid ))			\
		return def;						\
	    return Type(_phxLbl##Property->Name[0].Value,               \
		        _phxLbl##Property->Name[1].Value,               \
		        _phxLbl##Property->Name[2].Value,               \
			_phxLbl##Property->Name[3].Value);	        \
	}
FILE_LABEL   (SsiArticulation, Azimuth, 
			  ArticulationDeviceAngle[0], float)
FILE_LABEL   (SsiArticulation, Elevation, 
			  ArticulationDeviceAngle[1], float)
FILE_LABEL   (RacArticulation, InstrumentFocalLengthCount, 
	       ArticulationDeviceCount[0], int)
FILE_LABEL4   (RaArticulation, ArticulationDevLocation, PigPoint)
FILE_LABEL5   (RaArticulation, ArticulationDevOrient, PigQuaternion)

FILE_LABEL_S (RacArticulation, InstrumentDeploymentState,
		ArticulationDeviceState[0])

// In only a few cases is the lack of value significant...
#define CHECK_FILE_LABEL(Property,fName,Name)				\
	int PigFileModelPHX::check##fName()				\
	{								\
	    if (_phxLbl##Property == NULL)				\
		getPhxLbl##Property();					\
	    if (_phxLbl##Property == NULL)				\
		return FALSE;						\
	    return _phxLbl##Property->Name.Valid;			\
	}

CHECK_FILE_LABEL(SsiArticulation, Azimuth, ArticulationDeviceAngle[0])
CHECK_FILE_LABEL(SsiArticulation, Elevation, ArticulationDeviceAngle[1])


////////////////////////////////////////////////////////////////////////////////
// Electronics temperature
////////////////////////////////////////////////////////////////////////////////
float PigFileModelPHX::getInstrumentElectronicsTemperature(float def)
{
    if (_lblInstrumentState == NULL)
	getLblInstrumentState();
    if (_lblInstrumentState == NULL)
	return def;
    const char *inst = getInstrumentId();
    if (strncasecmp(inst, "SSI", 3) == 0) {		// SSI - PCB_END
	if (_lblInstrumentState->InstrumentTemperature[6].Valid)
	    return _lblInstrumentState->InstrumentTemperature[6].Value;
	return def;
    }
    // For RAC or OM, default to [0] for lack of anything better
    if (_lblInstrumentState->InstrumentTemperature[0].Valid)
	return _lblInstrumentState->InstrumentTemperature[0].Value;

    return def;
}

////////////////////////////////////////////////////////////////////////////////
// Optics temperature
////////////////////////////////////////////////////////////////////////////////
float PigFileModelPHX::getInstrumentOpticsTemperature(float def)
{
    if (_lblInstrumentState == NULL)
	getLblInstrumentState();
    if (_lblInstrumentState == NULL)
	return def;
    const char *inst = getInstrumentId();
    if (strncasecmp(inst, "SSI", 3) == 0) {		// SSI - OPTICAL BENCH
	if (_lblInstrumentState->InstrumentTemperature[2].Valid)
	    return _lblInstrumentState->InstrumentTemperature[2].Value;
	return def;
    }
    if (strncasecmp(inst, "RAC", 3) == 0) {		// RAC - OPTICAL BENCH
	if (_lblInstrumentState->InstrumentTemperature[1].Valid)
	    return _lblInstrumentState->InstrumentTemperature[1].Value;
	return def;
    }
    // Must be OM... default to [0] for lack of anything better
    if (_lblInstrumentState->InstrumentTemperature[0].Valid)
	return _lblInstrumentState->InstrumentTemperature[0].Value;

    return def;
}

////////////////////////////////////////////////////////////////////////////////
// Return the average of the CCD start and end temps
//////////////////////////////////////////////////////////////////////////////
float PigFileModelPHX::getInstrumentTemperature(float def)
{
    return (getInstrumentTemperatureStart(def) +
			getInstrumentTemperatureEnd(def)) / 2.0;
}

//////////////////////////////////////////////////////////////////////////////
// CCD temps.  This is somewhat complicated because the EM has no sensor,
// thus the values are always pegged at 2047.  In that case, we use the
// PCB temperature minus 20 degrees, per an email from Adam Shaw to rgd
// on 3/23/07 titled "Re: Temperature conversions".
// Note that OM simply uses the same values as SSI.  For RAC, there really
// is no start and end temperature so we just return [0] and [1] anyway for
// expediency.
//////////////////////////////////////////////////////////////////////////////
float PigFileModelPHX::getInstrumentTemperatureStart(float def)
{
    if (_lblInstrumentState == NULL)
	getLblInstrumentState();
    if (_lblInstrumentState != NULL) {

        if (_lblInstrumentState->InstrumentTemperatureCount[0].Valid) {
	    if (_lblInstrumentState->InstrumentTemperatureCount[0].Value!=2047){
		if (_lblInstrumentState->InstrumentTemperature[0].Valid) {
		    return _lblInstrumentState->InstrumentTemperature[0].Value;
		}
	    }
	}
	// Something failed above.  Try the PCB temp - 20.

	float pcb = getInstrumentElectronicsTemperature(1000000.0);
	if (pcb < 1000000.0)		// not defaulted
	    return pcb - 20.0;
    }
    return def;			// something failed, use ultimate default
}
//////////////////////////////////////////////////////////////////////////////
float PigFileModelPHX::getInstrumentTemperatureEnd(float def)
{
    if (_lblInstrumentState == NULL)
	getLblInstrumentState();
    if (_lblInstrumentState != NULL) {

        if (_lblInstrumentState->InstrumentTemperatureCount[1].Valid) {
	    if (_lblInstrumentState->InstrumentTemperatureCount[1].Value!=2047){
		if (_lblInstrumentState->InstrumentTemperature[1].Valid) {
		    return _lblInstrumentState->InstrumentTemperature[1].Value;
		}
	    }
	}
	// Something failed above.  Try the PCB temp - 20.

	float pcb = getInstrumentElectronicsTemperature(1000000.0);
	if (pcb < 1000000.0)		// not defaulted
	    return pcb - 20.0;
    }
    return def;			// something failed, use ultimate default
}

////////////////////////////////////////////////////////////////////////
// Get the Rover Motion Counter for the file.  The array must be supplied.
// num_index on input should contain the max size of the supplied array;
// on output it will contain the number of elements actually found (which
// could be bigger than the supplied max size, but only that many will
// actually be returned in the array).  PIG_MAX_SITE_INDEX (from
// PigRoverStateManager.h) can be useful for array dimensioning.
////////////////////////////////////////////////////////////////////////

void PigFileModelPHX::getRoverMotionCounter(int indices[], int &num_indices)
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

