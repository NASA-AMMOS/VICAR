////////////////////////////////////////////////////////////////////////
// PigRoverStateManagerMPF
//
// MPF-specific version of PigRoverStateManager.  It doesn't have to do
// much beyond what the superclass does, except manage file coord systems.
////////////////////////////////////////////////////////////////////////

#include "PigRoverStateManagerMPF.h"
#include "PigMPF.h"
#include "PigFileModel.h"
#include "PigCSReference.h"
#include "PigCSDefinition.h"

////////////////////////////////////////////////////////////////////////
// Read all coord sys objects in the given file and add them to the
// in-memory database.
//
// This has NOT been tested... adapted from PigMPF::getSite() ... rgd 2020-06-15
//
////////////////////////////////////////////////////////////////////////

void PigRoverStateManagerMPF::addFileCoordSystems(PigFileModel *file)
{

    PigQuaternion quat;
    PigVector     offset;

    // Check for NOSITE
    char nosite_str[16];
    int count;
    getParam("NOSITE", nosite_str, &count, 1, sizeof(nosite_str));
    int nosite = FALSE;
    if (count != 0)
	nosite = TRUE;

    int rmc[2] = { 0, 0 };

    // Lander first - no quat, we assume it's like most missions' rover frame

    PigQuaternion ident(1.0, 0.0, 0.0, 0.0);

    quat = file->getInstLocalLevelQuaternion(ident);
    offset = file->getSrfcFxdLclLvlVector(PigVector(0.0, 0.0, 0.0));

    if (nosite) {           // NOSITE specified, blank out quat and offset
        quat = ident;
        offset = PigVector(0.0, 0.0, 0.0);
    }

    PigCSReference fixed_ref(_mission, "FIXED", NULL, rmc, 2, NULL);

    PigCSReference lander_ref(_mission, "LANDER", NULL, rmc, 2, NULL);
    PigCSDefinition lander_def(_mission, &lander_ref, &fixed_ref, offset, quat);

    PigCSReference ll_ref(_mission, "MLL", NULL, rmc, 2, NULL);
    PigCSDefinition ll_def(_mission, &ll_ref, &fixed_ref, offset, ident);

    // Rover next

    // !!!!Signs per Jean Lorre's explanation.
    quat = PigMPF::eulerAnglesToQuaternion(file->getInstrumentHostHeading(0.0),
                                   file->getInstrumentHostPitch(0.0),
                                   file->getInstrumentHostRoll(0.0));

    offset = file->getInstHostPosition(PigPoint(0.0, 0.0, 0.0));

    if (quat.magnitude() == 0.0)        // invalid quat - all 0
        quat = ident;			// use identity instead

    if (nosite) {           // NOSITE specified, blank out quat and offset
        quat = ident;
        offset = PigVector(0.0, 0.0, 0.0);
    }

    PigCSReference rover_ref(_mission, "ROVER", NULL, rmc, 2, NULL);

    PigCSDefinition rover_def(_mission, &rover_ref, &lander_ref, offset, quat);

    addSolution(&lander_def);
    addSolution(&ll_def);
    addSolution(&rover_def);

}

