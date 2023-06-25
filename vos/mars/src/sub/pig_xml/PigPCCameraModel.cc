////////////////////////////////////////////////////////////////////////
// PigPCCameraModel.cc
//
// Data Structure holding a single Camera model info from 
// the pointing correction file.
////////////////////////////////////////////////////////////////////////

#include "PigPCCameraModel.h"
#include "PigXerces.h"

#include <string.h>
#include <stdlib.h>
#include <stdio.h>

////////////////////////////////////////////////////////////////////////
// Constructor
////////////////////////////////////////////////////////////////////////

PigPCCameraModel::PigPCCameraModel(DOMElement *solution)
{
    _cm_type = NULL;
    _cm_ids = NULL;
    _cm_names = NULL;
    _num_cms = 0;
    _ref_name = NULL;
    _ref_size = 0;

    DOMElement *cm = PigXerces::getOneElement(solution, "camera_model");

    if (cm == NULL)
	return;			// bad entry, nothing to do

    _cm_type = PigXerces::getAttributeCstr(cm, "type");

    setParameters(cm);
    setRefParameters(cm);
}

////////////////////////////////////////////////////////////////////////
// Destructor
////////////////////////////////////////////////////////////////////////

PigPCCameraModel::~PigPCCameraModel()
{
    if (_cm_type)
        delete _cm_type;

    if (_cm_names)
        delete _cm_names;

    if (_cm_ids)
        delete _cm_ids;

    if (_ref_name)
        delete _ref_name;
}

////////////////////////////////////////////////////////////////////////
// returns an array of camera model parameters for that camera model id
// in the supplied double[].  Array is unchanged if not found.
////////////////////////////////////////////////////////////////////////

void PigPCCameraModel::getParameter(const char name, double *values) 
{
    int cm_index = 0;
    for (int i=0; i<_num_cms; i++) {
        if (name == _cm_names[i]) 
            values[cm_index++] = _cm_values[i];
    }
}

////////////////////////////////////////////////////////////////////////
// Internal routine to set up camera model info to in-memory database
// "id" better be a single character for this routine!
////////////////////////////////////////////////////////////////////////


void PigPCCameraModel::setParameters(DOMElement *parent)
{

    char cm_ids[PIG_CM_MAX_PARAMS];
    memset(cm_ids, '\0', PIG_CM_MAX_PARAMS);

    DOMNodeList *cm_params=PigXerces::getElementsByTagName(parent, "parameter");
    if (cm_params == NULL) {
	PigModelBase::printStaticMsg("Unable to find 'parameter' in camera_model entry in nav file", PigMsgWarning);
	return;
    }
    for (int i=0; i<cm_params->getLength(); i++) {
	DOMElement *param = PigXerces::nextElement(cm_params, i);

        char *id = PigXerces::getAttribute(param, "id");
	char *value;

        value = PigXerces::getAttribute(param, "value1");
        if (value != NULL && strlen(value) != 0) {
            strcat(cm_ids, id);
            _cm_values[_num_cms++] = atof(value);
        }
	XMLString::release(&value);

        value = PigXerces::getAttribute(param, "value2");
        if (value != NULL && strlen(value) != 0) {
            strcat(cm_ids, id);
            _cm_values[_num_cms++] = atof(value);
        }
	XMLString::release(&value);

        value = PigXerces::getAttribute(param, "value3");
        if (value != NULL && strlen(value) != 0) {
            strcat(cm_ids, id);
            _cm_values[_num_cms++] = atof(value);
        }
	XMLString::release(&value);

        value = PigXerces::getAttribute(param, "value");
        if (value != NULL && strlen(value) != 0) {
            strcat(cm_ids, id);
            _cm_values[_num_cms++] = atof(value);
        }
	XMLString::release(&value);

	XMLString::release(&id);
    }
    _cm_names = strdup(cm_ids);

    int index=0;
    char cm_names[10];
    memset(cm_names, '\0', 10);
    for (int i=0; i<_num_cms; i++) {
        int duplicate = FALSE; 
        for (int j=0; j<index; j++) {
            if (cm_ids[i] == cm_names[j]) {
                duplicate = TRUE;
	        break;
            }
        }
        if (!duplicate) {
	    char ts[2];
	    ts[0] = cm_ids[i];
	    ts[1] = '\0';
	    strcat(cm_names, ts);
            index++;
        }
    }

    _cm_ids = strdup(cm_names);

    _unique_id = NULL;
    DOMElement *image = PigXerces::getOneElement(parent, "image");
    if (image != NULL)
	_unique_id = PigXerces::getAttributeCstr(image, "unique_id");

}

////////////////////////////////////////////////////////////////////////
// Internal routine to set up reference frame of camera model info 
// to in-memory database
////////////////////////////////////////////////////////////////////////

void PigPCCameraModel::setRefParameters(DOMElement *cm)
{
    _ref_size = 0;

    DOMElement *ref_frame = PigXerces::getOneElement(cm, "reference_frame");
    if (ref_frame == NULL)
	return;			// what to do?

    _ref_name = PigXerces::getAttributeCstr(ref_frame, "name");

    for (int i=0; i < PIG_MAX_CS_INDEX; i++) {
	char tmp[10];
	int index;
	snprintf(tmp, 10, "index%d", i+1);
	index = PigXerces::getAttributeInt(ref_frame, tmp, -99999);
	if (index != -99999) {
	    _ref_values[i] = index;
	    _ref_size = i+1;
	}
	else
	    _ref_values[i] = 0;
    }

}

