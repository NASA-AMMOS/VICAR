////////////////////////////////////////////////////////////////////////
// PigUtilities
//
// Collections of utilities for use by all Planetary Image Geometry (Pig)
// routines.  These are static functions, not associated with any class.
////////////////////////////////////////////////////////////////////////

#include "PigUtilities.h"
#include "PigModelBase.h"

#include "zvproto.h"
#include "applic.h"			/* for SUCCESS */
#include "getproj.h"
#include <string.h>
#include <cctype>

////////////////////////////////////////////////////////////////////////
// Open the file and return the unit number and mission name for use by
// the various create() functions.  Returns 0 for success (unlike the RTL!)
// The file is returned open only on success.
//
// If the mission is not recognized, "Unknown" is returned to indicate
// it is unknown.  This constitutes success for the return code.  Failure
// means that the file was not accessible, or something similar... not that
// the mission was unrecognized.
// 
// Note that the mission name match what is returned by zgetproj()
////////////////////////////////////////////////////////////////////////
int PigGetMission(const char *filename, int *unit, 
		  char *mission, char *host_id)
{
    int status;
    static int inst=0;
    char point_method[256];
    char *pointMethodValue = NULL;
    int count;

    status = zvunit(unit, "PigGetMission", inst++,
		"u_name", filename, NULL);
    if (status != SUCCESS)
	return 1;

    status = zvopen(*unit, "op", "read", "open_act", "", NULL);
    if (status != SUCCESS)
	return 1;

    int camera, fds;
    zgetproj(*unit, mission, &camera, &fds, &status);

    if (status != 0)
	strcpy(mission, "Unknown");
	
    // check for mission override  on a command line.  This is useful,
    // for example, if we'd like to process data as mission GENERIC.
    // Also might be useful if label is missing or contains incorrect mission
    // information.
    PigModelBase::getStaticParam("POINT_METHOD", point_method, &count, 1, 0);
    if (count == 1)
       pointMethodValue = PigModelBase::parseParamString(point_method, "MISSION");
    if (pointMethodValue) {
        // if multiple name-values have been specified, 
        // take the first value before "," or " " or "/0"
        strcpy(mission, "");
        int size = strlen(pointMethodValue);
        for (count = 0; count < size; count++) {
	  if (pointMethodValue[count] == ',' ||
              pointMethodValue[count] == ' ' ||
              pointMethodValue[count] == '\0')
	    break;
          mission[count] = (char)toupper(pointMethodValue[count]);
        }
	mission[count] = '\0';	 
    }
    // Get the host ID, if it exists.  This is only used for certain
    // missions, but we look for it all the time, so the decision about
    // whether or not to use it can be made by the caller.  No error is 
    // issued if it doesn't exist.
    
    if(host_id != NULL) {
        strcpy(host_id, "");
	zlget(*unit, "property", "INSTRUMENT_HOST_ID", host_id,
	      "err_act", "", "format", "string", "property", 
	      "IDENTIFICATION", NULL);
	//no need to check return status, host_id will be empty or error
    }

    return 0;		// success always if file was opened
}

