////////////////////////////////////////////////////////////////////////
// PigSurfaceModelParams
//
// Class to hold one definition of a SurfaceModel.  Typically this comes
// from a pointing correction XML file.  Includes a set of pointing
// params, and a CS reference.
////////////////////////////////////////////////////////////////////////
#ifndef PIGSURFACEMODELPARAMS_H
#define PIGSURFACEMODELPARAMS_H

#include "PigPointingParams.h"
#include "PigCSReference.h"
#include "PigXerces.h"
#include "PigSolutionManager.h"

class PigSurfaceModelParams : public PigSolutionItem {

  protected:
    char *_type;
    char *_solution_id;
    PigPointingParams *_pointingParams;
    PigCSReference *_csRef;

  public:

    // The given element should be the <surface_model> element being read.
    PigSurfaceModelParams(PigMission *m, DOMElement *model);

    virtual ~PigSurfaceModelParams();

    // Get methods

    virtual const char *getType() { return _type; }
    virtual const char *getSolutionID() { return _solution_id; }
    virtual PigPointingParams *getPointingParams()  { return _pointingParams; }
    virtual PigCSReference *getCoordSystemParams()  { return _csRef; }

    // PigSolutionItem methods
    virtual const char *getPriority() { return _solution_id; }
    virtual int getSolutionItemType() { return PIG_SOLUTION_TYPE_SURFACE; }

};

#endif
