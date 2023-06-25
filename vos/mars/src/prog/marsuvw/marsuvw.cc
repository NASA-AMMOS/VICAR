/* marsuvw */
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

#include "xyz_to_uvw.h"

#include <iostream>
using namespace std;

/* buffer sizes in main program */
#define MAX_INPUTS 3
// Value of 0 indicates there is no
// upper-bound for input image size.
#define MAX_NS 0
#define MAX_NL 0


////////////////////////////////////////////////////////////////////////

void main44()
{
    int i, band, line;
    int status, count, def;
    const int MSG_LEN = 256;
    char msg[MSG_LEN];

    int nids;
    char mission[64], instrument[64];

    // Inputs

    PigFileModel *file_models[MAX_INPUTS];
    PigCameraModel *camera_in[MAX_INPUTS];
    PigPointingModel *pointing_in[MAX_INPUTS];
    int homogeneous_inputs = TRUE;
    PigCoordSystem *cs, *xyz_cs;
    int xyz_unit[3];
    int in_band[3];

    // Outputs
    int out_unit[3];
    int out_band[3];
    int nlo, nso;

    // User Parameters
    XyzToUvwParams params;

    PigVector cameraPositionV;
    double cameraPosition[3];

    double *xyz[3];			// input image
    double *uvw[3];			// output image

    zvmessage("MARSUVW version 2020-05-21", "");

    // Get the input file list, and set up initial camera/pointing models
    // for each input.  Although we accept one or three inputs only, mars_setup
    // does lots of other nice things for us.

    mars_setup(nids, file_models, camera_in, pointing_in, NULL, cs,
                mission, instrument, homogeneous_inputs,
                MAX_NL, MAX_NS, MAX_INPUTS);

    PigMission *m = PigMission::getMissionObject(mission);

    // Get coord system for input XYZ file

    PigCSReference *ref;
    file_models[0]->getDerivedImageCS(ref);
    xyz_cs = m->getCoordSystem(ref);

    snprintf(msg, MSG_LEN, "Interpreting XYZ values using the %s coordinate frame: %s",
		xyz_cs->getFrameName(), ref->getFullName());
    zvmessage(msg, "");

    // Get the coordinate system to use.
    snprintf(msg, MSG_LEN, "Generating Surface Normals (UVW) using the %s coordinate frame.",
	    cs->getFrameName());
    zvmessage(msg, "");

    // Get camera position

    zvpcnt("CAMERA_CENTER", &count);
    if (pointing_in[0] != NULL) {
        cameraPositionV = pointing_in[0]->getCameraPosition(cs);
        cameraPositionV.getXYZ(cameraPosition);
    }
    else {
	if (count != 3) {
	    zvmessage("Camera model not found in input file; CAMERA_CENTER parameter required", "");
	    zabend();
	}
    }
    if (count == 3) {
	zvparmd("CAMERA_CENTER", cameraPosition, &count, &def, 3, 0);
	snprintf(msg, MSG_LEN, "Camera Center OVERRIDE to %f,%f,%f", cameraPosition[0],
		cameraPosition[1], cameraPosition[2]);
	zvmessage(msg, "");
    }

    // Get parameters

    zvparmd("SEPARATION", &params.max_point_separation, &count, &def, 1, 0);
    zvparmd("ERROR", &params.max_plane_error, &count, &def, 1, 0);
    zvp("MIN_POINTS", &params.min_num_points, &count);
    zvp("RADIUS", &params.window_radius, &count);
    zvparmd("REJECT", &params.rejection_ratio, &count, &def, 1, 0);
    zvparmd("X_CENTER", &params.x_center, &count, &def, 1, 0);
    zvparmd("Y_CENTER", &params.y_center, &count, &def, 1, 0);
    zvparmd("BOX_RADIUS", &params.box_radius, &count, &def, 1, 0);
    zvparmd("FLIP_THRESH", &params.flip_threshold, &count, &def, 1, 0);
    zvparmd("FLIP_DIST", &params.flip_distance, &count, &def, 1, 0);
    params.slope_mode = zvptst("SLOPE");

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

	if (file_models[0]->getNB() != 3) {
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

	    if (file_models[i]->getNB() != 1) {
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
	zvmessage("MARSUVW requires either 1 3-band file or 3 single band files as input", "");
	zabend();
    }


    // Open output files.
    // OUT can be 1 or 3 files, for a single, 3-banded file, or 3 single-band
    // files.

    zvpcnt("OUT", &count);
    if (count == 1) {
	zvunit(&out_unit[0], "OUT", 1, NULL);
	zvopen(out_unit[0], "op", "write",
	       "u_ns", file_models[0]->getNS(), "u_nl",file_models[0]->getNL(),
	       "u_nb", 3,
	       "open_act", "sa", "io_act", "sa", "u_org", "bsq",
	       "u_format", "doub", "o_format", "real", NULL);
	zvplabel(out_unit[0], 0, 1);

	out_unit[2] = out_unit[1] = out_unit[0];
	out_band[0] = 1;
	out_band[1] = 2;
	out_band[2] = 3;

	// write output label
	PigLabelModel *labelModel = m->createLabelModel(out_unit[0]);
	// pick the coordinate system to use.
	labelModel->setUVW(file_models, nids, cs, "UVW_MAP");
    }
    else if (count == 3) {
        char* image_type[3] = {"U_MAP", "V_MAP", "W_MAP"};
	for (i=0; i<3; i++) {
	    zvunit(&out_unit[i], "OUT", i+1, NULL);
	    zvopen(out_unit[i], "op", "write",
	       "u_ns", file_models[0]->getNS(), "u_nl",file_models[0]->getNL(),
	       "u_nb", 1,
	       "open_act", "sa", "io_act", "sa", "u_org", "bsq",
	       "u_format", "doub", "o_format", "real", NULL);
	    zvplabel(out_unit[i], 0, 1);
	    out_band[i] = 1;

	    // write output label
	    PigMission *m = PigMission::getMissionObject(mission);
	    PigLabelModel *labelModel = m->createLabelModel(out_unit[i]);
	    // pick the coordinate system to use.
	    labelModel->setUVW(file_models, nids, cs, image_type[i]);
	}
    }
    else {
	zvmessage("OUT must have 1 or 3 filenames", "");
	zabend();
    }

    count = 0;

    // get input image dimensions
    nlo = file_models[0]->getNL();
    nso = file_models[0]->getNS();

    // Allocate memory for input XYZ and output UVW.  The entire images must
    // be in memory for the subroutine.

    for (i=0; i<3; i++) {
	xyz[i] = (double *)malloc(nlo * nso * sizeof(double));
	if (xyz[i] == NULL) {
	    snprintf(msg, MSG_LEN, "Unable to allocate memory for XYZ input %d", i);
	    zvmessage(msg, "");
	    zabend();
	}
	uvw[i] = (double *)malloc(nlo * nso * sizeof(double));
	if (uvw[i] == NULL) {
	    snprintf(msg, MSG_LEN, "Unable to allocate memory for UVW output %d", i);
	    zvmessage(msg, "");
	    zabend();
	}
    }

    // Read in the XYZ file(s)...

    for (band = 0; band < 3; band++) {
	for (line = 0; line < nlo; line++) {
	    zvread(xyz_unit[band], (xyz[band]) + (line * nso),
			"BAND", in_band[band], "LINE", line+1, NULL);
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

    // Do the work...

    status = xyz_to_uvw(&params, xyz, nlo, nso, cameraPosition, uvw);

    if (status != 0) {
	snprintf(msg, MSG_LEN, "xyz_to_uvw failed!! status code=%d", status);
	zvmessage(msg, "");
	zabend();
    }

    // Write out the UVW file(s)...

    for (band = 0; band < 3; band++) {
	for (line = 0; line < nlo; line++) {

	    zvwrit(out_unit[band], (uvw[band]) + (line * nso),
			"BAND", out_band[band], "LINE", line+1, NULL);
	}
    }

    zvclose(out_unit[0], NULL);
    if (out_unit[1] != out_unit[0])
	zvclose(out_unit[1], NULL);
    if (out_unit[2] != out_unit[0])
	zvclose(out_unit[1], NULL);

}

