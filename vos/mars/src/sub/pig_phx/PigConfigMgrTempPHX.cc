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

#include "PigConfigMgrTempPHX.h"

#include "PigXerces.h"
#include "PigCameraMapper.h"
#include "PigCameraMapEntry.h"

#include <stdlib.h>
#include <string.h>

int PigConfigMgrTempPHX::_num_instances = 0;
PigConfigMgrTempPHX *PigConfigMgrTempPHX::_instances[MAX_PHX_CAMERA_SN];
char *PigConfigMgrTempPHX::_instrument_names[MAX_PHX_CAMERA_SN];
int PigConfigMgrTempPHX::_initialized = FALSE;
int PigConfigMgrTempPHX::_use_temp = FALSE;

////////////////////////////////////////////////////////////////////////
// Static factory method  Note that instrument can be NULL which will
// return an instance suitable for no correction.
// Note:  filter is not currently used.
////////////////////////////////////////////////////////////////////////

PigConfigMgrTempPHX *PigConfigMgrTempPHX::getInstance(const char *mission,
						      const char *host_id,
						      const char *instrument,
						      const char *filter)
{

    // See if temperature compensation should be applied

    if (!_initialized) {				// read parameter
	_initialized = TRUE;
	_use_temp = TRUE;
	char point_method[256], *value;
	int count;
	getStaticParam("POINT_METHOD", point_method, &count, 1, 0);
	if (count != 0) {
	    value = parseParamString(point_method, "NOTEMP");
	    if (value != NULL) {
	        _use_temp = FALSE;
	    }
	}
    }

    if (!_use_temp)
	instrument = NULL;	// disable it; will return no-correction instnce

    // Search for an existing instance

    for (int i=0; i < _num_instances; i++) {
	if (instrument == NULL && _instrument_names[i] == NULL)
	    return _instances[i];		// null instance
	if (strcasecmp(instrument, _instrument_names[i]) == 0)
	    return _instances[i];		// found match
    }

    // Read camera mapping to get S/N

    PigXerces::initialize();
    PigCameraMapper *map = new PigCameraMapper(mission, host_id);
    char *sn = NULL;
    if (instrument) {
        PigCameraMapEntry *entry = map->findFromID(instrument);
	sn = strdup(entry->getSerialNumber());
    }
    delete map;
    PigXerces::close();

    // No match found, let's create a new slot

    if (_num_instances >= MAX_PHX_CAMERA_SN - 1) {
	printStaticMsg("**ERROR**!!  Out of slots in PigConfigMgrTempPHX!!",
							PigMsgError);
	printStaticMsg("Temperature compensation not used", PigMsgError);
	for (int i=0; i < _num_instances; i++) {
	    if (_instrument_names[i] == NULL)
		return _instances[i];		// null instance
	}
	return new PigConfigMgrTempPHX(NULL);	// wow... rtn new null instance
    }

    if (sn == NULL)
	_instrument_names[_num_instances] = NULL;
    else
        _instrument_names[_num_instances] = strdup(instrument);

    _instances[_num_instances] = new PigConfigMgrTempPHX(sn);	// new instance
    if (sn)
	delete sn;

    return _instances[_num_instances++];
}

////////////////////////////////////////////////////////////////////////
// Constructor.  Actually reads the config file.  The serial_number can
// be NULL, in which case a no-correction instance is returned.  Such
// is also returned on error reading the file.
////////////////////////////////////////////////////////////////////////

PigConfigMgrTempPHX::PigConfigMgrTempPHX(char *serial_number)
{
    char filename[PIG_MAX_FILENAME_SIZE];
    FILE *inClientFile;
    char line[255];

    // Assign default values
    // These are all 0 to reflect no correction.

    _align_x[0] = 0.0;
    _align_x[1] = 0.0;
    _align_x[2] = 0.0;

    _align_y[0] = 0.0;
    _align_y[1] = 0.0;
    _align_y[2] = 0.0;

    _mrad_per_pixel = 0.0;	// unknown if we don't know which camera!

    _scale_x[0] = 0.0;
    _scale_x[1] = 0.0;
    _scale_x[2] = 0.0;

    _scale_y[0] = 0.0;
    _scale_y[1] = 0.0;
    _scale_y[2] = 0.0;

    if (serial_number == NULL)
	return;				// all done!

    sprintf(filename, "param_files/PHX_SN_%s_geom_temp.parms", serial_number);

    // open the file
    inClientFile = PigModelBase::openConfigFile(filename, NULL);

    if (inClientFile == NULL) {
        sprintf(line, 
		"Geometry temperature compenstation parameters file %s could not be opened; disabling temp. compensation",
		filename);
	printWarning(line);
	return;				// all done
    }

    while (fgets(line, sizeof(line), inClientFile) != NULL) {

	// pull out the parameters

	if (strncasecmp(line, "align_x_c0", 10) == 0)
	    sscanf(line, "align_x_c0 = %lf", &_align_x[0]);
	if (strncasecmp(line, "align_x_c1", 10) == 0)
	    sscanf(line, "align_x_c1 = %lf", &_align_x[1]);
	if (strncasecmp(line, "align_x_c2", 10) == 0)
	    sscanf(line, "align_x_c2 = %lf", &_align_x[2]);

	if (strncasecmp(line, "align_y_c0", 10) == 0)
	    sscanf(line, "align_y_c0 = %lf", &_align_y[0]);
	if (strncasecmp(line, "align_y_c1", 10) == 0)
	    sscanf(line, "align_y_c1 = %lf", &_align_y[1]);
	if (strncasecmp(line, "align_y_c2", 10) == 0)
	    sscanf(line, "align_y_c2 = %lf", &_align_y[2]);

	if (strncasecmp(line, "mrad_per_pixel", 14) == 0)
	    sscanf(line, "mrad_per_pixel = %lf", &_mrad_per_pixel);

	if (strncasecmp(line, "scale_x_c0", 10) == 0)
	    sscanf(line, "scale_x_c0 = %lf", &_scale_x[0]);
	if (strncasecmp(line, "scale_x_c1", 10) == 0)
	    sscanf(line, "scale_x_c1 = %lf", &_scale_x[1]);
	if (strncasecmp(line, "scale_x_c2", 10) == 0)
	    sscanf(line, "scale_x_c2 = %lf", &_scale_x[2]);

	if (strncasecmp(line, "scale_y_c0", 10) == 0)
	    sscanf(line, "scale_y_c0 = %lf", &_scale_y[0]);
	if (strncasecmp(line, "scale_y_c1", 10) == 0)
	    sscanf(line, "scale_y_c1 = %lf", &_scale_y[1]);
	if (strncasecmp(line, "scale_y_c2", 10) == 0)
	    sscanf(line, "scale_y_c2 = %lf", &_scale_y[2]);
    }

    fclose(inClientFile);
}

