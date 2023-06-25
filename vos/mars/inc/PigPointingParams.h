////////////////////////////////////////////////////////////////////////
// PigPointingParams
//
// Class to hold one set of Parameter info, such as from the 
// pointing correction XML file.
////////////////////////////////////////////////////////////////////////
#ifndef PIGPOINTINGPARAMS_H
#define PIGPOINTINGPARAMS_H

#include "PigAdjustable.h"	// for PIG_MAX_PARAMS
#include "PigXerces.h"

class PigPointingParams {

  protected:
    char *_param_names[PIG_MAX_PARAMS];
    double _param_values[PIG_MAX_PARAMS];
    int _num_params;
    char *_type;
    char *_unique_id;

  public:

    // Pass in the node containing the <parameter ...> entries.
    // The second parameter is the optional parent containing <image>
    // (in order to get unique_id).  Pass NULL if you don't care.

    PigPointingParams(DOMElement *node, DOMElement *parent);
    virtual ~PigPointingParams();

    // Getters
    // WARNING:  Several of these return pointers into the internals of
    // this class.  Use immediately, or copy the results.

    virtual const char *getType() { return _type; }
    virtual int getPointingParamCount() { return _num_params; }
    virtual char *getPointingParamName(int i) { return _param_names[i]; }
    virtual double getPointingParameter(int i) { return _param_values[i]; }
    virtual double getParameter(const char *name);
    virtual double *getParameters() { return _param_values; }
    virtual char *getUniqueId() { return _unique_id; }

};

#endif
