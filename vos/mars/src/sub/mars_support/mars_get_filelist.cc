////////////////////////////////////////////////////////////////////////
// mars_get_filelist.cc
//
// Retrieves a file list from the given parameter name.  The files are either
// specified in a file list (simple ASCII text) which is the only value in
// the parameter, or the files may be listed directly in the parameter itself.
// If only one value is specified, a file list is assumed.  However, if the
// file starts with LBLSIZE= or PDS_VERSION_ID or ODL_VERSION_ID, or if it
// doesn't exist and outputs is TRUE, then the single parameter is assumed to
// be the actual filename, not the name of a file list.
//
// The filename array must be passed in; the filenames themselves are
// dynamically allocated and must be freed by the caller.
//
// Comments, blank lines, and trailing spaces are ignored in the list file.
// A comment starts with a '#' in column 1.
////////////////////////////////////////////////////////////////////////

#include "mars_support.h"

#include "zvproto.h"

#include <stdio.h>
#include <string.h>
#include <ctype.h>

void mars_get_filelist(const char *param_name, int &nids, char *filenames[],
			const int max_inputs, const int outputs)
{
    int i;
    int status;
    char msg[PIG_MAX_FILENAME_SIZE+1];

    zvpcnt((char *)param_name, &nids);
 
    ////////////////////////////////////////////////////////////////////////
    // Obtain all filenames, either from the file list or the input parameters

    if (nids == 1) {				// file list
	char listoffiles[PIG_MAX_FILENAME_SIZE+1];
	FILE *fd;

	// read input file names contained in an ascii file

        status=zvpone((char *)param_name, listoffiles, 1, sizeof(listoffiles));

        if ((fd = fopen(listoffiles, "r")) == NULL) {		// Can't open
	    if (outputs) {			// that's okay if output
		sprintf(msg,
		   "File list not found for param %s, assuming single filename",
			param_name);
		zvmessage(msg, "");
        	filenames[0] = new char[strlen(listoffiles)+1];
		if (filenames[0] == NULL) {
		    zvmessage("Memory error in mars_get_filelist", "");
		    zabend();
		}
		strcpy(filenames[0], listoffiles);
		nids = 1;
		return;
	    }

            sprintf(msg, "Error opening file list %s\n", listoffiles);
	    zvmessage(msg, "");
            zabend();
        }

        // get list of files

	nids = 0;
        for (i = 0; i < max_inputs; i++) {
            if (fgets(msg, sizeof(msg), fd) == NULL) {
                break;
            }
	    if (i==0 && strncmp(msg, "LBLSIZE=", 8) == 0) {	// an image!
		sprintf(msg,
		   "VICAR file found for param %s, assuming single filename",
			param_name);
		zvmessage(msg, "");
        	filenames[0] = new char[strlen(listoffiles)+1];
		if (filenames[0] == NULL) {
		    zvmessage("Memory error in mars_get_filelist", "");
		    zabend();
		}
		strcpy(filenames[0], listoffiles);
		nids = 1;
		return;
	    }

	    if (i==0 && strncmp(msg, "PDS_VERSION_ID", 14) == 0) { // an image!
		sprintf(msg,
		   "PDS file found for param %s, assuming single filename",
			param_name);
		zvmessage(msg, "");
        	filenames[0] = new char[strlen(listoffiles)+1];
		if (filenames[0] == NULL) {
		    zvmessage("Memory error in mars_get_filelist", "");
		    zabend();
		}
		strcpy(filenames[0], listoffiles);
		nids = 1;
		return;
	    }

	    if (i==0 && strncmp(msg, "ODL_VERSION_ID", 14) == 0) { // an image!
		sprintf(msg,
		   "ODL file found for param %s, assuming single filename",
			param_name);
		zvmessage(msg, "");
        	filenames[0] = new char[strlen(listoffiles)+1];
		if (filenames[0] == NULL) {
		    zvmessage("Memory error in mars_get_filelist", "");
		    zabend();
		}
		strcpy(filenames[0], listoffiles);
		nids = 1;
		return;
	    }

	    if (msg[0] == '#')			// comment
		continue;

	    // Clear non-printing stuff and trailing whitespace
            for (int j=0; j<strlen(msg); j++)
                if ((!isprint(msg[j])) || isspace(msg[j]))
		    msg[j] = '\0';

	    if (strlen(msg) == 0)		// blank line
		continue;

	    filenames[nids] = new char[strlen(msg)+1];
	    if (filenames[nids] == NULL) {
		zvmessage("Memory error in mars_get_filelist", "");
		zabend();
	    }
	    strcpy(filenames[nids], msg);
	    nids++;
        }
        fclose(fd);
        if (nids == 0) {
            zvmessage("No input files found in listoffiles", "");
            zabend();}
        else {
            sprintf(msg,"%d files located from parameter %s", nids, param_name);
            zvmessage(msg, "");
        }
    }
    else {				// Files listed directly in param
	for (i = 0; i < nids; i++) {
	    zvpone((char *)param_name, msg, i+1, PIG_MAX_FILENAME_SIZE);
	    filenames[i] = new char[strlen(msg)+1];
	    if (filenames[i] == NULL) {
		zvmessage("Memory error in mars_get_filelist", "");
		zabend();
	    }
	    strcpy(filenames[i], msg);
	}
    }
 
    return;
}

