////////////////////////////////////////////////////////////////////////
// mars_get_pointing_corrections.cc
//
// Reads in a navigation table (specified by the NAVTABLE parameter)
//
// and this routine will print its own error messages.
////////////////////////////////////////////////////////////////////////

#include "PigPointingCorrections.h"

#include "zvproto.h"

#include <stdio.h>
#include <string.h>

PigPointingCorrections *mars_get_pointing_corrections(PigMission *m)
{
    int count, status;
    char msg[255];
    char val[10];

    int num_nav_entries;
    char navfilename[PIG_MAX_FILENAME_SIZE+1];

    PigPointingCorrections *pc;

    // Check the pointing corrections file

    zvp("NAVTABLE", navfilename, &count);
    if (count <= 0) {
	return NULL;		// Quietly return if no file was given
    }

    FILE *ftable;
    if ((ftable = fopen(navfilename, "r")) == NULL) {
	sprintf(msg, "Error opening file %s", navfilename);
	zvmessage(msg, "");
	return NULL;		// error!!
    }

    if (fgets(msg, sizeof(msg), ftable) == NULL) {
	sprintf(msg, "Empty nav file %s", navfilename);
	zvmessage(msg, "");
	fclose(ftable);
	return NULL;		// error!!
    }

    // Read the file.  Check for XML or old or new format.
    if (strncmp(msg, "<?xml", 5) == 0) { // XML format read
        fclose(ftable);
        pc = new PigPointingCorrections(m);
        pc->readSolutionFile(navfilename, true);
        return pc;
        
    }
    else if (strcmp(msg, "marsnav pointing corrections file, version 1\n") == 0) {
	// Old mpfnav format read.  Assumes only 2 pointing params per line
	zvmessage("Detected old mpfnav format nav file.  Please update!", "");

        return NULL;


        fclose(ftable);
    }
    // unknown format
    	sprintf(msg, "Unknown format of the nav file: %s", navfilename);
	zvmessage(msg, "");
	fclose(ftable);
	return NULL;		// error!!
}

