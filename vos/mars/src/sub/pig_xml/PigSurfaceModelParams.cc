////////////////////////////////////////////////////////////////////////
// PigSurfaceModelParams
//
// Class to hold one definition of a SurfaceModel.  Typically this comes
// from a pointing correction XML file.  Includes a set of pointing
// params, and a CS reference.
////////////////////////////////////////////////////////////////////////

#include "PigSurfaceModelParams.h"
#include "PigXerces.h"

#include <string.h>
#include <stdlib.h>

////////////////////////////////////////////////////////////////////////
// Constructor
//
// The given element should be the <surface_model> element being read.
////////////////////////////////////////////////////////////////////////

PigSurfaceModelParams::PigSurfaceModelParams(PigMission *m, DOMElement *model)
{
    _type = PigXerces::getAttributeCstr(model, "type");
    _solution_id = PigXerces::getAttributeCstr(model, "solution_id");

    _pointingParams = new PigPointingParams(model, NULL);
    _csRef = new PigCSReference(m, model, "reference_frame");
}

////////////////////////////////////////////////////////////////////////
// Destructor
////////////////////////////////////////////////////////////////////////

PigSurfaceModelParams::~PigSurfaceModelParams()
{
    if (_type)
	delete _type;
    if (_solution_id)
	delete _solution_id;

    if (_pointingParams)
        delete _pointingParams;
    if (_csRef)
        delete _csRef;
}

