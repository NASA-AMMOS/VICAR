////////////////////////////////////////////////////////////////////////
// mars_apply_navtable.cc
//
// Reads in a navigation table (specified by the NAVTABLE parameter)
// and applies the corrections to the pointing objects.  Returns 0 if
// corrections were applied, 1 if NAVTABLE was not specified, or 2 if
// there was an error in the file.  The status return can be ignored if
// desired; the pointing will be unchanged unless the file is read okay,
// and this routine will print its own error messages.
////////////////////////////////////////////////////////////////////////

#include "mars_support.h"

#include "PigPointingModel.h"
#include "PigPointingParams.h"
#include "PigPointingCorrections.h"
#include "PigCameraModel.h"
#include "PigFileModel.h"

#include "zvproto.h"

#include <stdio.h>
#include <string.h>

#define MAX_INPUTS 2000

int mars_apply_navtable(int nids, PigPointingModel *pointing_in[], PigFileModel *files_in[])
{
    int i, j, k;
    int count, status;
    char msg[255];
    int match_method;
    char val[10];
    int isOldFormat = 0;

    int num_nav_entries;
    char navfilename[PIG_MAX_FILENAME_SIZE+1];
    struct NavTable {
	int num_params;
	double original_params[PIG_MAX_PARAMS];
	double corrected_params[PIG_MAX_PARAMS];
    } nav_table[MAX_INPUTS];
    PigPointingCorrections *pc;

    // Check the pointing corrections file

    zvp("NAVTABLE", navfilename, &count);
    if (count <= 0) {
	return 1;		// Quietly return if no file was given
    }
    if (nids == 0)
	return 1;		// or if no input files

    PigMission *m = NULL;
    for (int i=0; i < nids; i++) {
	if (files_in[i] != NULL) {
	    m = PigMission::getMissionObject(files_in[i]->getMissionName());
	    break;
	}
    }
    if (m == NULL) {
	zvmessage("No files provided to mars_apply_navtable, thus no mission object", "");
	return 1;
    }

    zvp("MATCH_METHOD", val, &count);
    if (strcasecmp(val, "loose"))
        match_method = 1;
    else
        match_method = 0;

    double match_tol;
    int def;
    zvparmd("MATCH_TOL", &match_tol, &count, &def, 1, 0);

    char solution_id[256];
    zvp("SOLUTION_ID", solution_id, &count);
    char *sid = NULL;
    if (count != 0) {
        sid = solution_id;
    }

    FILE *ftable;
    if ((ftable = fopen(navfilename, "r")) == NULL) {
	sprintf(msg, "Error opening file %s", navfilename);
	zvmessage(msg, "");
	return 2;		// error!!
    }

    if (fgets(msg, sizeof(msg), ftable) == NULL) {
	sprintf(msg, "Empty nav file %s", navfilename);
	zvmessage(msg, "");
	fclose(ftable);
	return 2;		// error!!
    }

    // Read the file.  Check for XML or old or new format.
    if (strncmp(msg, "<?xml", 5) == 0) { // XML format read
        fclose(ftable);
        num_nav_entries = 0;
        pc = new PigPointingCorrections(m);
        pc->readSolutionFile(navfilename, true);

#if 0   // for debugging
        pc->printSolutions();
        pc->printPriorities();
#endif
        num_nav_entries = pc->getSolutionCount();
    }
    else if (strcmp(msg, "marsnav pointing corrections file, version 1\n") != 0) {
	// Old mpfnav format read.  Assumes only 2 pointing params per line
	zvmessage("Detected old mpfnav format nav file.  Please update!", "");

        isOldFormat = 1;
	num_nav_entries = 0;
	while (num_nav_entries < MAX_INPUTS) {

	    sscanf(msg, "%lf %lf %lf %lf",
			&nav_table[num_nav_entries].original_params[0],
			&nav_table[num_nav_entries].original_params[1],
			&nav_table[num_nav_entries].corrected_params[0],
			&nav_table[num_nav_entries].corrected_params[1]);

	    nav_table[num_nav_entries].num_params = 2;
	    num_nav_entries++;
	    if (fgets(msg, sizeof(msg), ftable) == NULL)
		break;				// end of file
	}

        fclose(ftable);
    }
    else {
	// New marsnav format read.  First int is # of params on the line (n),
	// then n original params, then n new ones.
	// Error handling could be better here!!!!

        isOldFormat = 1;
	num_nav_entries = 0;
	while (num_nav_entries < MAX_INPUTS) {
	    int n;
	    status = fscanf(ftable, "%d", &n);
	    if (status != 1)
		break;					// end of file

	    for (i=0; i < n; i++) {		// read original values
		status = fscanf(ftable, " %lf",
			&nav_table[num_nav_entries].original_params[i]);
		if (status != 1) {
		    sprintf(msg, "Syntax error in nav file '%s', line %d",
				navfilename, num_nav_entries);
		    zvmessage(msg, "");
		    break;
		}
	    }
	    for (i=0; i < n; i++) {		// read corrected values
		status = fscanf(ftable, " %lf",
			&nav_table[num_nav_entries].corrected_params[i]);
		if (status != 1) {
		    sprintf(msg, "Syntax error in nav file '%s', line %d",
				navfilename, num_nav_entries);
		    zvmessage(msg, "");
		    break;
		}
	    }
	    fscanf(ftable, "\n");
	    nav_table[num_nav_entries].num_params = n;
	    num_nav_entries++;
	}

        fclose(ftable);
    }

    sprintf(msg, "%d values read from navigation file", num_nav_entries);
    zvmessage(msg, "");

    if (num_nav_entries != nids) {
	zvmessage("File list and navigation table are different sizes","");
	zvmessage("Images without table entries used as is", "");
    }

    // Adjust the pointing from the navtable

    for (i=0; i < nids; i++) {
        if ((files_in[i] == NULL) || (pointing_in[i] == NULL))
            continue;

        if (isOldFormat) {   // with old format
	    double params[PIG_MAX_PARAMS];
	    int n = pointing_in[i]->getPointingParamCount();
	    pointing_in[i]->getPointingParameters(params, PIG_MAX_PARAMS);

	    // Find a matching navtable entry

	    int found = FALSE;
	    for (j=0; j<num_nav_entries; j++) {
	        if (nav_table[j].num_params = n) {
	     	    for (k=0; k<n; k++) {
		        if (fabs(params[k] - nav_table[j].original_params[k])
		       		> match_tol)
		            break;			// no match
		    }
		    if (k == n) {			// Found a match
		        found = TRUE;
		        for (k=0; k<n; k++)
			    params[k] = nav_table[j].corrected_params[k];
		        break;			// don't check the rest
	 	    }
	        }
	    }
	    if (found) {
	        sprintf(msg,"Navtable correcting image %d", i+1);
	        zvmessage(msg, "");
	        pointing_in[i]->setPointingParameters(params, n);
	    }
        }
        else {  // with XML format
            double params[PIG_MAX_PARAMS];
            int found = FALSE;
            PigPointingParams *param = NULL;

            pointing_in[i]->getPointingParameters(params, PIG_MAX_PARAMS);

            PigCameraModel *camera_model = pointing_in[i]->getCameraModel();
            const char *model_type = camera_model->getModelName();
            int n = pointing_in[i]->getPointingParamCount();

            // Find a matching navtable entry
            if (match_method) { // tight match
                char unique_id[33] = "";
                files_in[i]->getUniqueId(unique_id);
                if (!strcmp(unique_id, "")) {
                    sprintf(msg, "Empty unique id for file %s", files_in[i]->getFilename());
                    zvmessage(msg, "");
                    break;
                }

                param = pc->getPointing(unique_id, sid, model_type);

		// If no match for unique_id, check UniqueId2

		if (param == NULL) {
		    char unique_id2[33] = "";
		    files_in[i]->getUniqueId2(unique_id2);
		    if (strcmp(unique_id, unique_id2) != 0) {
			param = pc->getPointing(unique_id2, sid, model_type);
		    }
		}
            }
            else { // loose match
                pointing_in[i]->getPointingParameters(params, PIG_MAX_PARAMS);

                param = pc->getPointing(params, match_tol, sid, model_type);

            }
            if (param != NULL) {
                found = TRUE;
                for (int k=0; k<param->getPointingParamCount(); k++) 
                    params[k] = param->getPointingParameter(k);
            }
            
            if (found) {
                sprintf(msg, "Navtable correcting image %d", i+1);
                zvmessage(msg, "");
                pointing_in[i]->setPointingParameters(params, n);
            }
        }
    }
    return 0;		// success!
}

