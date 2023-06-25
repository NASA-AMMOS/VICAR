////////////////////////////////////////////////////////////////////////
// mars_read_rsf.cc
//
// Read in the Rover State files from standard parameters.  This should be
// done before calling mars_setup_coords, so the reference chain is complete
// before using it during file read.
//
// This method really should be called once per mission, not just once
// overall.  If we mix mission data, the objects created here are not
// really singletons, but are singletons per mission.
//
// For non-rover missions (where RSF's are not applicable), this routine
// is essentially a no-op.
//
// This routine expects the following parameter in the PDF:
//
// PARM RSF TYPE=STRING COUNT=0:100 DEFAULT=--
// PARM DEBUG_RSF TYPE=KEYWORD VALID=DEBUG_RSF COUNT=0:1 DEFAULT=--
//
// RSF is a list of Rover State Files to load (highest priority first).
//
// Each element of the parameter is expanded and may be a colon-separated
// list of filenames (similar to CONFIG_PATH).  It really should have been
// a single string, but oh well...
////////////////////////////////////////////////////////////////////////

#include "mars_support.h"

#include "PigMission.h"
#include "PigRoverStateManager.h"

#include "zvproto.h"
#include "defines.h"

void mars_read_rsf(const char *mission)
{
    PigMission *m = PigMission::getMissionObject(mission);

    // Load the Rover State files, if any

    int num_rsf;
    zvpcnt("RSF", &num_rsf);

    if (num_rsf != 0) {
	PigRoverStateManager *rsm = m->getRoverStateManager();
	for (int i=num_rsf; i > 0; i--) {
	    char rsf_file[256];
	    zvpone("RSF", rsf_file, i, sizeof(rsf_file));

	    char path_list[4095];
	    char *path_element[128];
	    int status;

	    // Much of the pathname expansion below is cribbed from
	    // PigModelBase::openConfigFile()

	    // Expand environment variables (and ~ if the first character)

	    status = zvfilename(rsf_file, path_list, sizeof(path_list)-1);
	    if (status != SUCCESS) {
		zvmessage("Syntax error or undefined variable in RSF path","");
		continue;			// try the next one
	    }

	    // Find the :'s in the string, and replace them with nulls.  Build
	    // up the path_element pointer array.

	    int num_paths = 0;
	    char *p = path_list;

	    path_element[num_paths++] = p;
	    while (p = strchr(p, ':')) {
		*p = '\0';
		if (*p)
		    path_element[num_paths++] = p;
	    }

	    // Now go through the path and call readSolutionFile on each

	    for (int i=0; i < num_paths; i++) {
	        rsm->readSolutionFile(path_element[i], true);
	    }
	}
    }
}

