////////////////////////////////////////////////////////////////////////
// PigPCCameraModel
//
// Class to hold a single Camera Model info from the pointing correction 
// file. No calculations done in this class.
////////////////////////////////////////////////////////////////////////
#ifndef PIGPCCAMERAMODEL_H
#define PIGPCCAMERAMODEL_H

#define PIG_CM_MAX_PARAMS 25
#include "PigXerces.h"
#include "PigMission.h"

#ifndef TRUE
#define TRUE 1
#endif
#ifndef FALSE
#define FALSE 0
#endif

class PigPCCameraModel {

  protected:
    char *_cm_type;		// These could very well be NULL...
    char *_cm_ids;
    char *_cm_names;
    double _cm_values[PIG_CM_MAX_PARAMS];
    int _num_cms;
    char *_ref_name;
    int _ref_size;
    int _ref_values[PIG_MAX_CS_INDEX];
    char *_unique_id;

    virtual void setParameters(DOMElement *cm);
    virtual void setRefParameters(DOMElement *cm);

  public:

    PigPCCameraModel(DOMElement *solution);
    virtual ~PigPCCameraModel();

    // type of CM (CAHV, CAHVOR, or CAHVORE)
    virtual const char *getType() const { return _cm_type; }

    // list of names, can depend on c, a, h, v, etc order
    virtual char *getParameterNames()   { return _cm_names; }
    
    // list of id's, can depend on c, a, h, v, etc order
    virtual char *getParameterIDs()     { return _cm_ids; }

    // Fills in supplied double[] for that param.  Array is unchanged if
    // not found.
    virtual void getParameter(const char id, double *values);

    // returns all params as an array 
    virtual double *getParameters()      { return _cm_values; }

    // returns number of params 
    virtual int getParamCount()          { return _num_cms; }

    // returns reference info
    virtual int getRefSize()             { return _ref_size; }
    virtual int *getRefValues()          { return _ref_values; }
    virtual char *getRefName()           { return _ref_name; }

    virtual char *getUniqueId()		{ return _unique_id; }
};

#endif

