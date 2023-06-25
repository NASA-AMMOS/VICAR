////////////////////////////////////////////////////////////////////////
// mars_setup.cc
//
// Does the standard processing needed by all multi-input mars* mosaic
// programs (including nav).  This involves reading the file list (or
// input parameters) from INP, and setting up the initial camera/pointing
// and radiometry models for each input.  The caller is responsible for
// allocating all arrays.  This routine also handles calling mars_read_rsf
// and mars_setup_coords for you.  The coordinate system to use is returned
// in the PigCoordSystem parameter.
//
// This routine expects the following parameters be in the PDF:
//
// PARM RAD TYPE=KEYWORD VALID=("RAD", "NORAD") DEFAULT=RAD
// PARM RSF TYPE=STRING COUNT=0:100 DEFAULT=--
// PARM COORD TYPE=KEYWORD VALID=("FIXED", "INSTRUMENT", "SITE", "ROVER", +
//            "LOCAL_LEVEL") DEFAULT="FIXED"
// PARM COORD_INDEX TYPE=INTEGER COUNT=0:10 DEFAULT=--
// PARM FIXED_SITE TYPE=INTEGER COUNT=0:1 DEFAULT=--
// PARM BRTCORR TYPE=STRING COUNT=(0:1) DEFAULT=--
//
// where RAD controls whether or not to radiometrically correct the images.
// If the radiometry array is passed in as NULL (meaning the caller doesn't
// want rad at all), this parameter is not checked.
// See mars_read_rsf below for the meaning of the RSF parameter.
// See mars_setup_coords below for the meaning of the COORD, COORD_INDEX,
// and FIXED_SITE parameters.
// If the brt_corr array is passed in as NULL (meaning the caller doesn't
// want brt corr at all), the BRTCORR parameter is not checked at all and
// may be absent.
////////////////////////////////////////////////////////////////////////

#include "mars_support.h"

#include "PigFileModel.h"
#include "PigCameraModel.h"
#include "RadiometryModel.h"
#include "PigPointingModel.h"
#include "PigSurfaceModel.h"
#include "PigPointingCorrections.h"
#include "PigBrtCorrModel.h"

#include "PigCSReference.h"
#include "PigMission.h"

#include "zvproto.h"

#include <stdio.h>
#include <string.h>
#include <ctype.h>

// legacy API for mars_setup.  Left here for backward compatibility.
// It is missing surface_model and brt_corr_models.
// All new apps should use another mars_setup API

void mars_setup(int &nids, PigFileModel *files[],
	PigCameraModel *cameras[], PigPointingModel *pointings[],
	RadiometryModel *radiometric[], PigCoordSystem *&def_cs,
	char *mission, char *instrument, int &homogeneous_inputs,
	const int max_nl, const int max_ns, const int max_inputs)
{
    PigSurfaceModel *surface_model = NULL;
    mars_setup(nids, files, cameras, pointings, surface_model, radiometric,NULL,
	     def_cs, mission, instrument, homogeneous_inputs, max_nl,
             max_ns, max_inputs);
}

// legacy API for mars_setup.  Left here for backward compatibility.
// It is missing brt_corr_models.
// All new apps should use another mars_setup API

void mars_setup(int &nids, PigFileModel *files[],
	PigCameraModel *cameras[], PigPointingModel *pointings[],
 	PigSurfaceModel *&surface_model,
	RadiometryModel *radiometric[], PigCoordSystem *&def_cs,
	char *mission, char *instrument, int &homogeneous_inputs,
	const int max_nl, const int max_ns, const int max_inputs)
{
    mars_setup(nids, files, cameras, pointings, surface_model, radiometric,NULL,
	     def_cs, mission, instrument, homogeneous_inputs, max_nl,
             max_ns, max_inputs);
}

// The full API

void mars_setup(int &nids, PigFileModel *files[],
	PigCameraModel *cameras[], PigPointingModel *pointings[],
	PigSurfaceModel *&surface_model,
	RadiometryModel *radiometric[], 
	PigBrtCorrModel *brt_corr_models[],
        PigCoordSystem *&def_cs, char *mission, char *instrument, 
        int &homogeneous_inputs, const int max_nl, const int max_ns, 
	const int max_inputs)
{
    int i;
    int status;
    char msg[PIG_MAX_FILENAME_SIZE+1];

    homogeneous_inputs = TRUE;

    ////////////////////////////////////////////////////////////////////////
    // Get the list of all files

    char **filenames = new char *[max_inputs];
    if (filenames == NULL) {
	zvmessage("Memory error in mars_setup, filename array", "");
	zabend();
    }
    mars_get_filelist("INP", nids, filenames, max_inputs, FALSE);

    ////////////////////////////////////////////////////////////////////////
    // Coord system stuff

    // Initialize the RSF's, if necessary.

    // Since we don't have FileModels yet, get the mission directly from the
    // Mission object (which has the same result)

    int a_good_file_idx;
    PigMission *mo = NULL;
    for (a_good_file_idx = 0; a_good_file_idx < nids; a_good_file_idx++) {
	int unit;
	mo = PigMission::getMissionObject(filenames[a_good_file_idx], &unit);
	if (mo != NULL) {
	    zvclose(unit, NULL);		// read_filelist re-opens it
	    break;
	}
    }
    if (mo == NULL) {
	zvmessage("Could not find a good input file to read.", "");
	zabend();
    }

    mars_read_rsf(mo->getMissionName());

    ////////////////////////////////////////////////////////////////////////
    // Read the files

    char *sid = NULL;
    PigPointingCorrections *pointing_corrections;

    mars_read_filelist(nids, filenames, files, cameras, pointings,
		radiometric, brt_corr_models,
		def_cs, mission, instrument,
		homogeneous_inputs, max_nl, max_ns,
		&pointing_corrections, &sid);

    if (def_cs == NULL) {
	zvmessage("No valid default coord sys found, using Fixed", "");
	def_cs = mo->getFixedCS();
    }

    // Make sure we have a valid file
    for (a_good_file_idx = 0; a_good_file_idx < nids; a_good_file_idx++) {
	if (files[a_good_file_idx] != NULL)
	    break;
    }
    if (a_good_file_idx >= nids) {
	zvmessage("Could not find a good input file model.", "");
	zabend();
    }

    ////////////////////////////////////////////////////////////////////////
    // construct surface model

    if (pointing_corrections != NULL) {
    	//look for surface in the nav file
    	PigSurfaceModelParams *smp = pointing_corrections->getSurfaceModel(sid);
	
	//!!!! This is a hack to get around the problem that we can't create
	// proper coordinate system knowing only it's name.  For example
	// specifying on a command line SURF_COORD=ROVER is not enough to create
	// proper Rover Coordinate System.  Because of that we check for command
	// line parameter here, and if it's given we discard NAV file surface model
	// if any and go directly to PigSurfaceModel::create(file).  That one knows
	// how to create proper CS.  For more info see comments in PigMission.cc
	// -ozp
	int cnt=0;
	char surface_coord[20];
        PigModelBase::getStaticParam("SURF_COORD", surface_coord, &cnt, 1, 0);    
    	if ((smp == NULL) || (cnt==1)) {
        	// no surface model definition found in the nav file,
		// or surface model definition has been specified
		// on the command line 
        	//create surface model using label info from the first
		// input file
    		surface_model = PigSurfaceModel::create(files[a_good_file_idx]);
    	}
    	else {  
	    PigMission *m = pointing_corrections->getMission();
            PigCSReference *csRef = smp->getCoordSystemParams();
	    PigCSReference new_csRef(m,
			csRef->getFrameName(),
			sid,
			csRef->getIndices(),
			csRef->getNumIndices(),
			csRef->getInstId());

	    surface_model = PigSurfaceModel::create(
			m->getMissionName(),
	    		instrument,
			smp->getPointingParams()->getType(),
			smp->getPointingParams()->getParameters(),
    			smp->getPointingParams()->getPointingParamCount(),
                	m->getCoordSystem(&new_csRef));
    	}
   }
   else {
       	// no nav file in xml format exists, 
       	//create default one using label info from the first
	// input file
    	surface_model = PigSurfaceModel::create(files[a_good_file_idx]);  
   }	

    for (i=0; i < nids; i++)
	delete filenames[i];
    delete[] filenames;

    zvmessage("All input labels processed", "");

    return;
}
