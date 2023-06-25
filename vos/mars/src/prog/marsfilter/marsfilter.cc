/* marsfilter */
#include "vicmain_c"

#include "mars_support.h"

#include "PigMission.h"
#include "PigFileModel.h"
#include "PigLabelModel.h"
#include "PigCameraModel.h"
#include "PigPointingModel.h"
#include "PigVector.h"
#include "PigCoordSystem.h"
#include "PigCSReference.h"
#include "PigSurfaceModel.h"

#include "FilterShapes.h"

#include <stdlib.h>

/* buffer sizes in main program */
#define MAX_INPUTS 3
#define MAX_NL MARS_MAX_NL
#define MAX_NS MARS_MAX_NS



////////////////////////////////////////////////////////////////////////

void main44()
{
    int i, j, band, line;
    int status, count, def;
	const size_t msgLen = 1024;
    char msg[msgLen];

    int nids;
    char mission[64], instrument[64];

    // Inputs

    PigFileModel *file_models[MAX_INPUTS];
    PigCameraModel *camera_in[MAX_INPUTS];
    PigPointingModel *pointing_in[MAX_INPUTS];
    PigSurfaceModel *surface_model;
    int homogeneous_inputs = TRUE;
    PigCoordSystem *cs, *xyz_cs, *site_cs;
    int xyz_unit[3];
    int in_band[3];
    double params[PIG_MAX_FILTER_PARAMS];

    // Outputs
    int out_unit;
    int nlo, nso;

    // User Parameters

    double *xyz[3];			// input image
    unsigned char *mask;		// output image
    int do_print;
    int use_xyz;		// if false, input is not XYZ; disables volumes

    zvmessage("MARSFILTER version 1", "");

    // Get the input file list, and set up initial camera/pointing models
    // for each input.  Although we accept one or three inputs only, mars_setup
    // does lots of other nice things for us.

    mars_setup(nids, file_models, camera_in, pointing_in, surface_model, NULL,
		cs, mission, instrument, homogeneous_inputs,
                MAX_NL, MAX_NS, MAX_INPUTS);

    PigMission *m = PigMission::getMissionObject(mission);

    // Get coord system for input XYZ file

    PigCSReference *ref;
    file_models[0]->getDerivedImageCS(ref);
    xyz_cs = m->getCoordSystem(ref);

    site_cs = m->getCoordSystem(file_models[0], "SITE");

    snprintf(msg, msgLen, "Interpreting XYZ values using the %s coordinate frame: %s",
		xyz_cs->getFrameName(), ref->getFullName());
    zvmessage(msg, "");

    // Get the coordinate system to use.
    snprintf(msg, msgLen, "Performing filtering using the %s coordinate frame.",
	    cs->getFrameName());
    zvmessage(msg, "");

    // Get parameters

    for (int i=0; i < PIG_MAX_FILTER_PARAMS; i++)
	params[i] = 0.0;
    zvparmd("PARAMS", params, &count, &def, PIG_MAX_FILTER_PARAMS, 0);

    do_print = zvptst("PRINT");
    FilterShape::enablePrint(do_print);
    int do_numbers = zvptst("NUMBERS");
    use_xyz = zvptst("USE_XYZ");
    if (!use_xyz) {
	zvmessage(
	   "WARNING: NO_XYZ has been specified.  Volume masks are disabled","");
    }

    // Open input file(s).  An XYZ data could be either 1 3-band file
    // or 3 single band files

    if (nids == 1) {
      
        // Make sure file is open with U_FORMAT of DOUB to match our buffer.

	// get Unit id
	xyz_unit[0] = file_models[0]->getUnit();

        if (file_models[0]->isFileOpen())
	    file_models[0]->closeFile();
	status = zvopen(xyz_unit[0], "op", "read", "open_act", "sa",
		"io_act", "sa", "u_format", "doub", NULL);
	file_models[0]->setFileOpen(TRUE);

	if (use_xyz && file_models[0]->getNB() != 3) {
	  zvmessage("A single XYZ file must have three bands", "");
	  zabend();
	}

	// Initialize xyz_unit array
	xyz_unit[2] = xyz_unit[1] = xyz_unit[0];
      
        // Initialize band array
	in_band[0] = 1;
	in_band[1] = 2;
	in_band[2] = 3;      
    }
    else if (nids == 3) {
        for (i = 0; i < 3; i++) {

            // make sure that file is open
            if (file_models[i]->isFileOpen())
	        file_models[i]->closeFile();
      
	    // get Unit id
	    xyz_unit[i] = file_models[i]->getUnit();

	    status = zvopen(xyz_unit[i], "op", "read", "open_act", "sa",
		"io_act", "sa", "u_format", "doub", NULL);
	    file_models[i]->setFileOpen(TRUE);

	    if (use_xyz && file_models[i]->getNB() != 1) {
		zvmessage("A three-file XYZ must have one band each (#1)", "");
	        zabend();
	    }

	    // check that all files are the same size
	    if ((file_models[i]->getNL() != file_models[0]->getNL()) ||
		(file_models[i]->getNS() != file_models[0]->getNS())) {
	        zvmessage("Input is of different size than Input #1", "");
	        zabend();
	    }
	    in_band[i] = 1;
        }

    }
    else {
	zvmessage("MARSFILTER requires either 1 3-band file or 3 single band files as input", "");
	zabend();
    }


    // get input image dimensions
    nlo = file_models[0]->getNL();
    nso = file_models[0]->getNS();

    // Allocate memory for input XYZ and output mask.  We read the entire
    // images into memory for convenience.

    for (i=0; i<3; i++) {
	xyz[i] = (double *)malloc(nlo * nso * sizeof(double));
	if (xyz[i] == NULL) {
	    snprintf(msg, msgLen, "Unable to allocate memory for XYZ input %d", i);
	    zvmessage(msg, "");
	    zabend();
	}
	memset(xyz[i], 0, nlo * nso * sizeof(double));
    }
    mask = (unsigned char *)malloc(nlo * nso * sizeof(unsigned char));
    if (mask == NULL) {
	zvmessage("Unable to allocate memory for MASK output", "");
	zabend();
    }
    memset(mask, 0, (nlo * nso * sizeof(unsigned char)));

    // Read in the XYZ file(s)...

    if (use_xyz) {
	for (band = 0; band < 3; band++) {
	    for (line = 0; line < nlo; line++) {
		zvread(xyz_unit[band], (xyz[band]) + (line * nso),
			"BAND", in_band[band], "LINE", line+1, NULL);
	    }
	}
    }

    // Convert coord systems if necessary...  (and it usually will be)

    if (use_xyz && cs != xyz_cs) {
	for (line = 0; line < nlo; line++) {
	    for (int samp = 0; samp < nso; samp++) {
		int index = line * nso + samp;
		PigPoint old_xyz(*(xyz[0]+index), *(xyz[1]+index),
					*(xyz[2]+index));

		// If the point is 0,0,0 (meaning not valid), leave it alone

		if (old_xyz.getX() != 0.0 ||
		    old_xyz.getY() != 0.0 ||
		    old_xyz.getZ() != 0.0) {

		    PigPoint new_xyz = cs->convertPoint(old_xyz, xyz_cs);
		    *(xyz[0]+index) = new_xyz.getX();
		    *(xyz[1]+index) = new_xyz.getY();
		    *(xyz[2]+index) = new_xyz.getZ();
		}
	    }
	}
    }

    ////////////////////////////////////////////////////////////////////
    // Do the work...
    // We actually do it twice... once for the common filter (in the
    // database or "filter" keyword), and once for the specific filter
    // (in the "extra_filter" keyword).  The weird loop logic is to avoid
    // a process function with far too many arguments.
    ////////////////////////////////////////////////////////////////////

    char filter_filename[PIG_MAX_FILENAME_SIZE];
    char actual_filter_filename_common[PIG_MAX_FILENAME_SIZE];
    char actual_filter_filename_extra[PIG_MAX_FILENAME_SIZE];
    strcpy(actual_filter_filename_common, "");
    strcpy(actual_filter_filename_extra, "");

    int use_horizon = FALSE;
    double horizon = 0.0;

    // Get the RMC to use
    int rmc[PIG_MAX_CS_INDEX];
    int num_rmc = 0;
    if (zvptst("RMC"))
	file_models[0]->getRoverMotionCounter(rmc, num_rmc);

    for (int f = 0; f < 2; f++) {

	FilterShape **filters;
	int num_filters;

	if (f == 0) {
	    // Read in the common filter file

	    zvp("FILTER", filter_filename, &count);

	    if (count == 0)		// Default, obtain from the database
		status = FilterShape::readFilters(m, NULL, "rover_filter",
					instrument, filters, num_filters,
					actual_filter_filename_common,
					rmc, num_rmc);
	    else
		status = FilterShape::readFilters(m, filter_filename,
					"rover_filter",
					instrument, filters, num_filters,
					actual_filter_filename_common,
					rmc, num_rmc);

	    if (status != 0)
		zabend();		// msg already printed
	} else {
	    // Read in the extra filter file

	    zvp("EXTRA_FILTER", filter_filename, &count);
	    if (count == 0)
		continue;			// no extra file

	    status = FilterShape::readFilters(m, filter_filename,"extra_filter",
					instrument, filters, num_filters,
					actual_filter_filename_extra,
					rmc, num_rmc);
	    if (status != 0)
		zabend();		// msg already printed
	}


	////////////////////////////////////////////////////////////////////

	// Now process the mask...

	int dn = 255;
	for (int i=0; i < num_filters; i++) {
	    if (do_numbers) {
		dn = 255;
		char *id = filters[i]->getId();
		if (id != NULL && strlen(id) != 0)
		    dn = atoi(id) % 256;
	    }

	    filters[i]->setSourceImage(file_models[0], camera_in[0],
					cs, site_cs, xyz, nlo, nso, params);
	    if (do_print)
	        filters[i]->print();
	    filters[i]->addFilterToMask(mask, nlo, nso, dn);

	    // If horizon, save the horizon value for the label.  If more than
	    // one, this will pick up the last (there shouldn't be).
	    if (filters[i]->getFilterType() == FilterTypeProjectedHorizon) {
	        use_horizon = TRUE;
	        horizon =
			PigRad2Deg(((FilterShapeProjectedHorizon *)filters[i])->
								getElevation());
	    }
	}
    }

    ////////////////////////////////////////////////////////////////////

    // Write out the MASK file...

    // Open output file.
    // OUT is a single 1-banded file.

    zvunit(&out_unit, "OUT", 1, NULL);
    zvopen(out_unit, "op", "write", "u_ns", nso, "u_nl",nlo, "u_nb", 1,
	       "open_act", "sa", "u_org", "bsq",
	       "u_format", "byte", "o_format", "byte", NULL);
    zvplabel(out_unit, 0, 1);

    // write output label
    PigLabelModel *labelModel = m->createLabelModel(out_unit);
    labelModel->setMask(file_models, 1, NULL, actual_filter_filename_common,
		actual_filter_filename_extra, use_horizon, horizon);

    for (line = 0; line < nlo; line++) {
	zvwrit(out_unit, mask + (line * nso), "LINE", line+1, NULL);
    }

    zvclose(out_unit, NULL);

}

