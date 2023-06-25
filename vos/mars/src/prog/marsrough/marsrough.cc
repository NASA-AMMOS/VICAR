/* marsrough */
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
#include <stdlib.h>

extern "C" {
#include "xyz_uvw_to_roughness.h"
}

#include <iostream>
using namespace std;

/* buffer sizes in main program */
#define MAX_INPUTS 3
#define MAX_NL MARS_MAX_NL
#define MAX_NS MARS_MAX_NS

void open_inputs(int nids, PigFileModel *file_models[], int unit[3],
		int band[3], char *type);

////////////////////////////////////////////////////////////////////////

void main44()
{
    int i, j, band, line;
    int status, count, def;
    const size_t msgLen = 256;
    char msg[msgLen];

    int nids, uvw_nids;
    char mission[64], instrument[64];

    // Inputs

    PigFileModel *file_models[MAX_INPUTS], *uvw_file_models[MAX_INPUTS];
    PigCameraModel *camera_in[MAX_INPUTS];
    PigPointingModel *pointing_in[MAX_INPUTS];
    int homogeneous_inputs = TRUE;
    PigCoordSystem *cs, *xyz_cs, *uvw_cs;
    int xyz_unit[3], uvw_unit[3];
    int xyz_band[3], uvw_band[3];

    // Outputs
    int out_unit;
    int out_band;
    int nlo, nso;

    // User Parameters
    XyzUvwToRoughnessParams params;

    double *xyz[3];			// input image
    double *uvw[3];			// input image
    double *rough;			// output image

    zvmessage("MARSROUGH version 1", "");

    // Get the input file list, and set up initial camera/pointing models
    // for each input.  Although we accept one or three inputs only, mars_setup
    // does lots of other nice things for us.

    mars_setup(nids, file_models, camera_in, pointing_in, NULL, cs,
                mission, instrument, homogeneous_inputs,
                MAX_NL, MAX_NS, MAX_INPUTS);

    PigMission *m = PigMission::getMissionObject(mission);

    // For UVW we must do part of what mars_setup does for the INP parameter...

    char **uvw_filenames = new char *[MAX_INPUTS];
    if (uvw_filenames == NULL) {
	zvmessage("Memory error in setup, uvw filename array", "");
	zabend();
    }
    mars_get_filelist("UVW", uvw_nids, uvw_filenames, MAX_INPUTS, FALSE);

    for (i = 0; i < uvw_nids; i++) {
	uvw_file_models[i] = PigFileModel::create(uvw_filenames[i]);
	if (uvw_file_models[i] == NULL) {
	    snprintf(msg, msgLen, "Unable to create file model for UVW input %d", i);
	    zvmessage(msg, "");
	    zabend();
	}
    }

    // Get coord system for input XYZ file

    PigCSReference *ref;
    file_models[0]->getDerivedImageCS(ref);
    xyz_cs = m->getCoordSystem(ref);

    snprintf(msg, msgLen, "Interpreting XYZ values using the %s coordinate frame: %s",
                xyz_cs->getFrameName(), ref->getFullName());
    zvmessage(msg, "");

    // Get coord system for input UVW file

    PigCSReference *ref2;
    uvw_file_models[0]->getDerivedImageCS(ref2);
    uvw_cs = m->getCoordSystem(ref2);

    snprintf(msg, msgLen, "Interpreting UVW values using the %s coordinate frame: %s",
                uvw_cs->getFrameName(), ref2->getFullName());
    zvmessage(msg, "");

    // Get the coordinate system to use.
    snprintf(msg, msgLen, "Generating Roughness using the %s coordinate frame.",
	    cs->getFrameName());
    zvmessage(msg, "");

    // Get parameters

    zvparmd("INNER_RADIUS", &params.inner_radius, &count, &def, 1, 0);
    zvparmd("OUTER_RADIUS", &params.outer_radius, &count, &def, 1, 0);
    if (params.inner_radius >= params.outer_radius) {
	zvmessage("INNER_RADIUS must be less than OUTER_RADIUS!!", "");
	zabend();
    }
    zvparmd("MAX_ROUGH", &params.max_roughness, &count, &def, 1, 0);
    zvparmd("BAD_ROUGH", &params.bad_roughness, &count, &def, 1, 0);
    zvp("MAX_WINDOW", &params.max_window_size, &count);
    zvp("MIN_CLOSE", &params.min_close_points, &count);
    zvparmd("X_CENTER", &params.x_center, &count, &def, 1, 0);
    zvparmd("Y_CENTER", &params.y_center, &count, &def, 1, 0);
    zvparmd("BOX_RADIUS", &params.box_radius, &count, &def, 1, 0);

    // Open input file(s).  An XYZ data could be either 1 3-band file
    // or 3 single band files

    open_inputs(nids, file_models, xyz_unit, xyz_band, "XYZ");

    // Now open the UVW's properly

    open_inputs(uvw_nids, uvw_file_models, uvw_unit, uvw_band, "UVW");

    if ((file_models[0]->getNL() != uvw_file_models[0]->getNL()) ||
        (file_models[0]->getNS() != uvw_file_models[0]->getNS())) {
	zvmessage("XYZ and UVW files are not the same size", "");
	zabend();
    }

    // Open output files.
    // OUT is a single 1-band float file

    zvpcnt("OUT", &count);
    zvunit(&out_unit, "OUT", 1, NULL);
    zvopen(out_unit, "op", "write",
	       "u_ns", file_models[0]->getNS(), "u_nl",file_models[0]->getNL(),
	       "u_nb", 1,
	       "open_act", "sa", "u_org", "bsq",
	       "u_format", "doub", "o_format", "real", NULL);
    zvplabel(out_unit, 0, 1);

    // write output label

    PigLabelModel *labelModel = m->createLabelModel(out_unit);

    // gather all filemodels for handing over to label model
    int nids_all = nids + uvw_nids;
    PigFileModel *file_models_all[MAX_INPUTS*2];
    for (int cnt = 0; cnt < nids; cnt++)
        file_models_all[cnt] = file_models[cnt];
    for (int cnt = 0; cnt < uvw_nids; cnt++)
        file_models_all[nids+cnt] = uvw_file_models[cnt];

    // pick the coordinate system to use.
    labelModel->setRough(file_models_all, nids_all, cs, params.bad_roughness,0);

    count = 0;

    // get input image dimensions
    nlo = file_models[0]->getNL();
    nso = file_models[0]->getNS();

    // Allocate memory for input XYZ and UVW, and output roughness.  The
    // entire images must be in memory for the subroutine.

    for (i=0; i<3; i++) {
	xyz[i] = (double *)malloc(nlo * nso * sizeof(double));
	if (xyz[i] == NULL) {
	    snprintf(msg, msgLen, "Unable to allocate memory for XYZ input %d", i);
	    zvmessage(msg, "");
	    zabend();
	}
	uvw[i] = (double *)malloc(nlo * nso * sizeof(double));
	if (uvw[i] == NULL) {
	    snprintf(msg, msgLen, "Unable to allocate memory for UVW input %d", i);
	    zvmessage(msg, "");
	    zabend();
	}
    }

    rough = (double *)malloc(nlo * nso * sizeof(double));
    if (rough == NULL) {
	zvmessage("Unable to allocate memory for output file", "");
	zabend();
    }

    // Read in the XYZ file(s)...

    for (band = 0; band < 3; band++) {
	for (line = 0; line < nlo; line++) {
	    zvread(xyz_unit[band], (xyz[band]) + (line * nso),
			"BAND", xyz_band[band], "LINE", line+1, NULL);
	}
    }

    // Read in the UVW file(s)...

    for (band = 0; band < 3; band++) {
	for (line = 0; line < nlo; line++) {
	    zvread(uvw_unit[band], (uvw[band]) + (line * nso),
			"BAND", uvw_band[band], "LINE", line+1, NULL);
	}
    }

    // Convert coord systems if necessary...

    if (cs != xyz_cs) {
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

    if (cs != uvw_cs) {
	for (line = 0; line < nlo; line++) {
	    for (int samp = 0; samp < nso; samp++) {
		int index = line * nso + samp;
		PigVector old_uvw(*(uvw[0]+index), *(uvw[1]+index),
					*(uvw[2]+index));

		// If the point is 0,0,0 (meaning not valid), leave it alone

		if (old_uvw.getX() != 0.0 ||
		    old_uvw.getY() != 0.0 ||
		    old_uvw.getZ() != 0.0) {

		    PigVector new_uvw = cs->convertVector(old_uvw, uvw_cs);
		    *(uvw[0]+index) = new_uvw.getX();
		    *(uvw[1]+index) = new_uvw.getY();
		    *(uvw[2]+index) = new_uvw.getZ();
		}
	    }
	}
    }

    // Do the work...

    status = xyz_uvw_to_roughness(&params, xyz, uvw, nlo, nso, rough);

    if (status != 0) {
	snprintf(msg, msgLen, "xyz_uvw_to_roughness failed!! status code=%d", status);
	zvmessage(msg, "");
	zabend();
    }

    // Write out the roughness file(s)...

    for (line = 0; line < nlo; line++) {
	zvwrit(out_unit, rough + (line * nso), "BAND",1, "LINE", line+1, NULL);
    }

    zvclose(out_unit, NULL);

}

////////////////////////////////////////////////////////////////////////
// Open one set of inputs, either XYZ or UVW...
////////////////////////////////////////////////////////////////////////

void open_inputs(int nids, PigFileModel *file_models[], int unit[3],
		int band[3], char *type)
{
    int i;
    const size_t msgLen = 256;
    char msg[msgLen];

    if (nids == 1) {
      
        // Make sure file is open with U_FORMAT of DOUB to match our buffer.

	// get Unit id
	unit[0] = file_models[0]->getUnit();

        if (file_models[0]->isFileOpen())
	    file_models[0]->closeFile();
	zvopen(unit[0], "op", "read", "open_act", "sa",
		"io_act", "sa", "u_format", "doub", NULL);
	file_models[0]->setFileOpen(TRUE);

	if (file_models[0]->getNB() != 3) {
	    snprintf(msg, msgLen, "A single %s file must have three bands", type);
	    zvmessage(msg, "");
	    zabend();
	}

	// Initialize xyz_unit array
	unit[2] = unit[1] = unit[0];
      
        // Initialize band array
	band[0] = 1;
	band[1] = 2;
	band[2] = 3;      
    }
    else if (nids == 3) {
        for (i = 0; i < 3; i++) {

            // make sure that file is open
            if (file_models[i]->isFileOpen())
	        file_models[i]->closeFile();
      
	    // get Unit id
	    unit[i] = file_models[i]->getUnit();

	    zvopen(unit[i], "op", "read", "open_act", "sa",
		"io_act", "sa", "u_format", "doub", NULL);
	    file_models[i]->setFileOpen(TRUE);

	    if (file_models[i]->getNB() != 1) {
		snprintf(msg, msgLen, "A three-file %s must have one band each", type);
		zvmessage(msg, "");
	        zabend();
	    }

	    // check that all files are the same size
	    if ((file_models[i]->getNL() != file_models[0]->getNL()) ||
		(file_models[i]->getNS() != file_models[0]->getNS())) {
	        zvmessage("Input is of different size than Input #1", "");
	        zabend();
	    }
	    band[i] = 1;
        }

    }
    else {
	snprintf(msg, msgLen, "MARSROUGH requires either 1 3-band file or 3 single band files as input for %s", type);
	zvmessage(msg, "");
	zabend();
    }
}

