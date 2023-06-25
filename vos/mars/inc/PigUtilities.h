////////////////////////////////////////////////////////////////////////
// PigUtilities
//
// Collections of utilities for use by all Planetary Image Geometry (Pig)
// routines.  These are static functions, not associated with any class.
////////////////////////////////////////////////////////////////////////

#ifndef PIGUTILITIES_H
#define PIGUTILITIES_H

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
		  char *mission, char* host_id);

#endif

