////////////////////////////////////////////////////////////////////////
// PigPointingParams
//
// Class to hold one set of Parameter info, such as from the
// pointing correction XML file.
////////////////////////////////////////////////////////////////////////

#include "PigPointingParams.h"
#include "PigXerces.h"
#include "PigModelBase.h"

#include <string.h>
#include <stdlib.h>

////////////////////////////////////////////////////////////////////////
// Constructor
//
// Pass in the node containing the <parameter ...> entries.  It can have
// an optional "type" attribute.
// The second parameter is the optional parent containing <image>
// (in order to get unique_id).  Pass NULL if you don't care.
////////////////////////////////////////////////////////////////////////

PigPointingParams::PigPointingParams(DOMElement *node, DOMElement *parent)
{

    _num_params = 0;

    // may be null

    _type = PigXerces::getAttributeCstr(node, "type");

    DOMNodeList *params_lst = PigXerces::getElementsByTagName(node,
								"parameter");
    if (params_lst == NULL) {
	PigModelBase::printStaticMsg("No <parameter> entries found in file", PigMsgWarning);
	_num_params = 0;
    }
    else {
        _num_params = params_lst->getLength();

        for (int i=0; i<_num_params; i++) {
	    DOMElement *param = PigXerces::nextElement(params_lst, i);
            _param_names[i] = PigXerces::getAttributeCstr(param, "id");
            _param_values[i] = PigXerces::getAttributeDouble(param,"value",0.0);
        }
    }

    _unique_id = NULL;
    if (parent != NULL) {
        DOMElement *image = PigXerces::getOneElement(parent, "image");
        if (image != NULL) 
            _unique_id = PigXerces::getAttribute(image, "unique_id");
    }

}

////////////////////////////////////////////////////////////////////////
// Destructor
////////////////////////////////////////////////////////////////////////

PigPointingParams::~PigPointingParams()
{
    if (_type)
        delete _type;

    for (int i=0; i < _num_params; i++) {
        if (_param_names[i]) {
            delete _param_names[i];
        }
    }
}

////////////////////////////////////////////////////////////////////////
// returns parameter for given name
////////////////////////////////////////////////////////////////////////

double PigPointingParams::getParameter(const char *name) 
{
    double temp = 0.0;

    for (int i=0; i<_num_params; i++) {
        if (!strcasecmp(name, _param_names[i])) {
            temp = _param_values[i];
            break;
        }
    }

    return temp;
}

