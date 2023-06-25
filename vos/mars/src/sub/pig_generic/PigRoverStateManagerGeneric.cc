////////////////////////////////////////////////////////////////////////
// PigRoverStateManagerGeneric
//
// Generic-mission version of PigRoverStateManager.  It doesn't have to do
// much beyond what the superclass does, except manage file coord systems.
////////////////////////////////////////////////////////////////////////

#include "PigRoverStateManagerGeneric.h"
#include "PigGenericImage.h"
#include "PigFileModel.h"
#include "PigCSReference.h"
#include "PigCSDefinition.h"

////////////////////////////////////////////////////////////////////////
// Read all coord sys objects in the given file and add them to the
// in-memory database.
//
// There is no spec for CS objects for generic images.  We try to read
// the normal multimission spec though.  But then we also add the Camera
// CS explicitly whether or not there's a file CS.  No quat, no offset.
////////////////////////////////////////////////////////////////////////

void PigRoverStateManagerGeneric::addFileCoordSystems(PigFileModel *file)
{

    // Let the superclass do its thing

    PigRoverStateManager::addFileCoordSystems(file);

    PigQuaternion ident(1.0, 0.0, 0.0, 0.0);
    PigVector zero(0.0, 0.0, 0.0);

    int rmc[2] = {0, 0};

    PigCSReference fixed_ref(_mission, "FIXED", NULL, rmc, 2, NULL);

    PigCSReference camera_ref(_mission, "CAMERA", NULL, rmc, 2, NULL);
    PigCSDefinition camera_def(_mission, &camera_ref, &fixed_ref, zero, ident);

    addSolution(&camera_def);

}

