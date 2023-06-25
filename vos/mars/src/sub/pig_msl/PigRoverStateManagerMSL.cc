////////////////////////////////////////////////////////////////////////
// PigRoverStateManagerMSL
//
// MSL-specific version of PigRoverStateManager.  It doesn't have to do
// much beyond what the superclass does.
////////////////////////////////////////////////////////////////////////

#include "PigRoverStateManagerMSL.h"
#include "PigMSL.h"
#include "PigCSDefinition.h"

#include <sys/types.h>
#include <sys/stat.h>

////////////////////////////////////////////////////////////////////////
// Read in the given file.  This is an override of PigSolutionManager's
// version.  Here, we look to see if the file is a directory and if so,
// look for the MSL RMC database files, passing each one to
// PSM::readSolutionFile.  If not, the original file is passed along to
// the superclass.
//
// If anything goes wrong in the stat() process, we simply silently hand
// the file to the superclass... just in case it can deal with it.
////////////////////////////////////////////////////////////////////////

//!!!! THIS WILL PROBABLY CHANGE FOR MSL  given PLACES

void PigRoverStateManagerMSL::readSolutionFile(char *filename,
						int exclude_telemetry)
{
    struct stat buf;
    char file[PIG_MAX_FILENAME_SIZE];
    char msg[PIG_MAX_FILENAME_SIZE+25];
    FILE *f;

    int status = stat(filename, &buf);

    if (status != 0) {		// Unknown error, pass to superclass
	PigRoverStateManager::readSolutionFile(filename, exclude_telemetry);
	return;
    }

    if ((buf.st_mode & S_IFDIR) == 0) {		// Not a dir, pass to super
	PigRoverStateManager::readSolutionFile(filename, exclude_telemetry);
	return;
    }

    // Okay, now we know we're a directory.  Look for the SVF master file
    // first.

    sprintf(file, "%s/master/%s_Master.svf", filename, _mission->getHostID());
    f = fopen(file, "r");
    if (f != NULL) {
	fclose(f);
	sprintf(msg, "Loading Site Master %s", file);
	printStaticMsg(msg, PigMsgInfo);
	PigRoverStateManager::readSolutionFile(file, exclude_telemetry);
    }

    // Now look for RVF master files, in order.  Keep looking until we get
    // five failures.  This provides some small insurance against incomplete
    // databases or unreadable files.

    int failures = 0;
    int site = 0;

    while (failures < 5) {

	sprintf(file, "%s/master/%s_Site_%d_Master.rvf", filename,
		_mission->getHostID(), site);
	site++;
	f = fopen(file, "r");
	if (f == NULL) {
	    failures++;
	} else {
	    fclose(f);
	    sprintf(msg, "Loading Rover Master %s", file);
	    printStaticMsg(msg, PigMsgInfo);
	    PigRoverStateManager::readSolutionFile(file, exclude_telemetry);
	}
    }
}

