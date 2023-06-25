////////////////////////////////////////////////////////////////////////
// mars_setup_coords.cc
//
// Does the setup of coordinate systems.  This includes returning the default
// coordinate system to use, and initializing the Fixed site for the mission.
// The default coordinate system is returned.
//
// For non-rover missions (where the Fixed site is... fixed), this routine is
// still useful in that it handles the COORD parameter for you.
//
// This routine expects the following parameters in the PDF:
//
// PARM COORD TYPE=KEYWORD VALID=("FIXED", "INSTRUMENT", "SITE", "ROVER", +
//            "LOCAL_LEVEL") DEFAULT="FIXED"
// PARM COORD_INDEX TYPE=INTEGER COUNT=0:10 DEFAULT=--
// PARM FIXED_SITE TYPE=INTEGER COUNT=0:1 DEFAULT=--
// PARM SOLUTION_ID TYPE=STRING COUNT=0:1 DEFAULT=--
// PARM DEBUG_RSF TYPE=KEYWORD VALID=DEBUG_RSF COUNT=0:1 DEFAULT=--
//
// COORD is the coordinate system to use (other mission-specific names may
// be included in the valid list, but are not required).  COORD_INDEX is the
// coordinate system index (needed for some rover-based CS's - it's the RMC).
// FIXED_SITE defines which major Site is the FIXED site for this run.
//
// If COORD_INDEX is defaulted (and needed), it is obtained from the
// first entry in the provided File list.
//
// If FIXED_SITE is defaulted (and needed), then the minimum Site number
// (first index of motion counter) found in the provided File list is used.
//
// DEBUG_RSF turns on a statement that prints out the RMC database.  It
// does not actually need to be in the delivered PDF; the app can be copied
// and the value added to the PDF when needed.
////////////////////////////////////////////////////////////////////////

#include "mars_support.h"

#include "PigFileModel.h"
#include "PigMission.h"
#include "PigRoverStateManager.h"

#include "zvproto.h"

PigCoordSystem *mars_setup_coords(const char *mission,
                        int num_files, PigFileModel *files[])
{
    int count;

    // First check if there is any good (successfully opened) file.

    PigFileModel *a_good_file = NULL;
    for (int i = 0; (i < num_files) && !a_good_file; i++)
	a_good_file = files[i];
    if (a_good_file == NULL) {
	zvmessage("Could not find a good file to read!", "");
	zabend();
    }

    PigMission *m = PigMission::getMissionObject(mission);
    if (m == NULL) {
	zvmessage("Could not get mission object!", "");
	zabend();
    }


    // Read all coord systems from the input files and add them to the
    // RSM database...

    PigRoverStateManager *rsm = m->getRoverStateManager();
    if (rsm != NULL) {
    	for (int i=0; i < num_files; i++)
            if (files[i] != NULL)
	        rsm->addFileCoordSystems(files[i]);

    	if (zvptst("DEBUG_RSF")) 	/* Useful for debugging... !!!! */
            rsm->printSolutions();
    }

    // Initialized the Fixed site.  Used to be done here, now it's all
    // in PigMission.
    //!!!! This should be done lazily now, so no reason to force it here.
    //!!!! There aren't any CS objects set up yet!  rgd 2020-05-25
    //!!!! m->getFixedCS();

    // Now set up the default coordinate system

    char cs_name[100];

    zvp("COORD", cs_name, &count);
    if (count == 0)				// shouldn't happen
	strcpy(cs_name, "FIXED");

    int indices[PIG_MAX_CS_INDEX], num_indices;

    // &count is really the "def" slot, but we don't use it...
    zvparm("COORD_INDEX", indices, &num_indices, &count, PIG_MAX_CS_INDEX, 0);

    // If COORD_INDEX is not given, get the RMC from the first (good) file

    if (num_indices == 0) {
	a_good_file->getRoverMotionCounter(indices, num_indices);
    }

    // If there were no RMC entries, it might be a mosaic.  So let's use
    // an alternate getCoordSystem() which will actually handle that case.

    if (num_indices == 0) {
	return m->getCoordSystem(a_good_file, cs_name);
    }

    
    char solution_id[256];
    zvp("SOLUTION_ID", solution_id, &count);
    char *sid = NULL;
    if (count != 0)
	sid = solution_id;

    PigCSReference ident(m, cs_name, sid, indices, num_indices,
					a_good_file->getInstrumentId());

    return m->getCoordSystem(&ident);

}

