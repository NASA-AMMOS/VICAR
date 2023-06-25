////////////////////////////////////////////////////////////////////////
// mars_read_filelist.cc
//
// Takes a given filelist, opens all the files, and reads/initializes all of
// the given arrays.  This is much of what used to be in mars_setup but it has
// been separated out so the caller can read more than one input file list.
//
// Note that mars_setup does other one-time initialization and should be
// called instead of this in most cases, and before this when reading more
// than one list.  In other words, this should be called ONLY when reading
// more than one list, and after mars_setup.
//
// The caller should call mars_get_filelist() first in order to populate
// nids and filenames.
//
// Note that the def_cs return is used by mars_setup and should generally
// be ignored when calling this routine directly (the default CS's really
// should match after all...).  Likewise, the pointing_corrections and
// solution_id are for mars_setup and should generally be ignored by other
// callers (they can be passed in as NULL if you don't need them).
//
// This routine expects items in the PDF as for mars_setup.
//
// The only input parameters are nids and filenames... but the routine
// expects most other arrays to be set up so they can just be filled in
// as outputs.
////////////////////////////////////////////////////////////////////////

#include "mars_support.h"

#include "PigFileModel.h"
#include "PigCameraModel.h"
#include "RadiometryModel.h"
#include "PigPointingModel.h"
#include "PigPointingCorrections.h"
#include "PigBrtCorrModel.h"

#include "zvproto.h"

#include <stdio.h>
#include <string.h>
#include <ctype.h>

void mars_read_filelist(int nids, char **filenames,		// inputs
	PigFileModel *files[],
	PigCameraModel *cameras[], PigPointingModel *pointings[],
	RadiometryModel *radiometric[], 
	PigBrtCorrModel *brt_corr_models[],
        PigCoordSystem *&def_cs, char *mission, char *instrument, 
        int &homogeneous_inputs, const int max_nl, const int max_ns,
	PigPointingCorrections **pointing_corrections,
	char **solution_id)
{
    int i;
    int status;
    char msg[PIG_MAX_FILENAME_SIZE+1];

    PigFileModel *a_good_file = NULL;

    homogeneous_inputs = TRUE;

    ////////////////////////////////////////////////////////////////////////
    // Open all files

    for (i = 0; i < nids; i++) {

	files[i] = PigFileModel::create(filenames[i]);
	if (files[i] == NULL) {
	    sprintf(msg, "Unable to create file model for input %d", i+1);
	    zvmessage(msg, "");
	    continue;			// try to read next one
	}

        if (a_good_file == NULL) {
	    if (files[i]->getMissionName() != NULL) {
                a_good_file = files[i];
            }
        }

	if ((max_nl != 0 && files[i]->getNL() > max_nl) ||
	    (max_ns != 0 && files[i]->getNS() > max_ns)) {
	    sprintf(msg, "Input image %d exceeds buffer limit size", i+1);
	    zvmessage(msg, "");
	    zabend();
	}
    }
    if (a_good_file == NULL) {
	zvmessage("Could not find a good file to read.", "");
	zabend();
    }

    strcpy(mission, (a_good_file->getMissionName()?a_good_file->getMissionName():""));
    // it's ID in the file model but becomes name in the camera model, sigh
    strcpy(instrument, (a_good_file->getInstrumentId()?a_good_file->getInstrumentId():""));
    


    ////////////////////////////////////////////////////////////////////////
    // Coord system stuff.  Note that the returned CS is ignored, as the
    // default cs is set up by mars_setup.

    def_cs = mars_setup_coords(a_good_file->getMissionName(), nids, files);

    PigMission *m = PigMission::getMissionObject(a_good_file->getMissionName());
    PigPointingCorrections *pc = mars_get_pointing_corrections(m);
    if (pointing_corrections != NULL)
	*pointing_corrections = pc;

    char internal_solution_id[256];
    int count;
    zvp("SOLUTION_ID", internal_solution_id, &count);
    char *sid = NULL;
    if (solution_id != NULL)
	*solution_id = NULL;
    if (count != 0) {
        sid = internal_solution_id;
    } 
    else if (pc != NULL) {
    	// no solution_id found on a command line
	// default to highest priority in the xml nav file if it exists
	sid = pc->getHighestPriority();
    }
    if (solution_id != NULL && sid != NULL)
	*solution_id = strdup(sid);

    ////////////////////////////////////////////////////////////////////////
    // Now loop through and initialize all files.  This must be done after
    // the above coord system setups

    for (i = 0; i < nids; i++) {

	// Clear fields (or set to default) in case label not present

	cameras[i] = NULL;
	pointings[i] = NULL;
	if (radiometric != NULL)
	    radiometric[i] = NULL;

        if (files[i] == NULL)
            continue;

        ////////////////////////////////////////////////////////////////////////
        // Compute initial camera and pointing models

	cameras[i] = PigCameraModel::create(files[i], NULL);
	if (cameras[i] == NULL) {
	    sprintf(msg, "Unable to create camera model for input %d", i+1);
	    zvmessage(msg, "");
	}
        
        int status;
	if (cameras[i] != NULL) {
	    pointings[i] = mars_create_pointing_model(cameras[i], files[i], sid,
								pc, status);
	}

	if (pointings[i] == NULL) {
	    sprintf(msg, "Unable to create pointing model for input %d", i+1);
	    zvmessage(msg, "");
	}
	
	if (status == -1) {
	    //nav file exists but no match has been found
	    sprintf(msg, "No match in the Navtable has been found for input %d", i+1);
	    zvmessage(msg, "");
	    // Point the input camera
	    if (pointings[i] != NULL)
	        pointings[i]->pointCamera(files[i]);
	}
	else if (status == 0) {
	    // no pointing correction file present
	    // Point the input camera
	    if (pointings[i] != NULL)
	        pointings[i]->pointCamera(files[i]);	
	}
	else if (status == 1) {
	    // correction has been applied
	    sprintf(msg, "Pointing Correction has been applied for input %d", i+1);
	    zvmessage(msg, "");
	}

	// Check for varying missions and instruments, for information only

        if (strcasecmp(mission, (files[i]->getMissionName()?files[i]->getMissionName():"")) != 0) {
	    sprintf(msg, "Note: Input list contains more than one mission. First mission %s, current mission %s",
                    mission, files[i]->getMissionName());
	    PigModelBase::printUniqueStaticMsg(msg, PigMsgWarning);
	    homogeneous_inputs = FALSE;
	}
	if (strcasecmp(instrument, (files[i]->getInstrumentId()?files[i]->getInstrumentId():"")) != 0) {
	    sprintf(msg, "Note: Input list contains more than one instrument ID. First ID %s, current ID %s",
                    instrument, files[i]->getInstrumentId());
	    PigModelBase::printUniqueStaticMsg(msg, PigMsgWarning);
	    homogeneous_inputs = FALSE;
	}


        ////////////////////////////////////////////////////////////////////////
        // Create Radiometry Models for each input image, if requested

	if (radiometric != NULL) {
	    if (zvptst("NORAD")) {		// No radiometric correction
		radiometric[i] = NULL;
	    }
	    else {
		radiometric[i] = RadiometryModel::create(files[i]);

		if (radiometric[i] == NULL) {
		    sprintf(msg,
			"Unable to create radiometric model for input %d", i+1);
		     zvmessage(msg, "");
		}
	    }
    	}

        ////////////////////////////////////////////////////////////////////////
        // Create Brightness Correction Models for each image, if requested.
	// Rad is needed by some corrections (HsiLin for example)

	if (brt_corr_models != NULL) {
	    PigBrtCorrModel::createBrtCorrModels(nids, files,
				brt_corr_models, radiometric);
    	}

	files[i]->closeFile();			// don't have too many open
    }
 
    // figure out dnscaling factor
    RadiometryModel::setAllDnscalingFactor(radiometric, files, nids);

    // check for old style, text-based nav file, if it exists, apply its values
    if (pc == NULL) {
    	//check for old format style and if nav file in old format found
	//print warning message and apply pointing corrections
	mars_apply_navtable(nids, pointings, files);
    }

    return;
}
