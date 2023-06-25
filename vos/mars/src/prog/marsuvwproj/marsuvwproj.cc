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

    PigFileModel *file_models[MAX_INPUTS];
    PigCameraModel *camera_in[MAX_INPUTS];
    PigPointingModel *pointing_in[MAX_INPUTS];
    int homogeneous_inputs = TRUE;
    PigCoordSystem *cs, *uvw_cs;
    int uvw_unit[3];
    int uvw_band[3];

    // Outputs
    int out_unit;
    int out_band;
    int nlo, nso;

    double angle, angle_rad;

    double *uvw[3];			// input image
    double *proj_uvw[3];		// output image

    zvmessage("MARSUVWPROJ version 1", "");

    // Get the input file list, and set up initial camera/pointing models
    // for each input.  Although we accept one or three inputs only, mars_setup
    // does lots of other nice things for us.

    mars_setup(nids, file_models, camera_in, pointing_in, NULL, cs,
                mission, instrument, homogeneous_inputs,
                MAX_NL, MAX_NS, MAX_INPUTS);

    PigMission *m = PigMission::getMissionObject(mission);

    // Get coord system for input UVW file

    PigCSReference *ref;
    file_models[0]->getDerivedImageCS(ref);
    uvw_cs = m->getCoordSystem(ref);

    snprintf(msg, msgLen, "Interpreting UVW values using the %s coordinate frame: %s",
                uvw_cs->getFrameName(), ref->getFullName());
    zvmessage(msg, "");

    // Get the coordinate system to use.
    snprintf(msg, msgLen, "Generating Projected UVW using the %s coordinate frame.",
	    cs->getFrameName());
    zvmessage(msg, "");

    // Get parameters

    zvparmd("ANGLE", &angle, &count, &def, 1, 0);
    angle_rad = PigDeg2Rad(angle);

    // Open input file(s).  An UVW data could be either 1 3-band file
    // or 3 single band files

    open_inputs(nids, file_models, uvw_unit, uvw_band, "UVW");

    // Open output files.
    // OUT is a single 3-band float file
    //!!!! Should be able to output 3 single-band files too

    zvpcnt("OUT", &count);
    zvunit(&out_unit, "OUT", 1, NULL);
    zvopen(out_unit, "op", "write",
	       "u_ns", file_models[0]->getNS(), "u_nl",file_models[0]->getNL(),
	       "u_nb", 3,
	       "open_act", "sa", "u_org", "bsq",
	       "u_format", "doub", "o_format", "real", NULL);
    zvplabel(out_unit, 0, 1);

    // write output label

    PigLabelModel *labelModel = m->createLabelModel(out_unit);
    // pick the coordinate system to use.
    labelModel->setUVW(file_models, nids, cs, "Projected UVW");

    count = 0;

    // get input image dimensions
    nlo = file_models[0]->getNL();
    nso = file_models[0]->getNS();

    // Allocate memory for input UVW, and output UVW.  The
    // entire images must be in memory for the subroutine.

    for (i=0; i<3; i++) {
	uvw[i] = (double *)malloc(nlo * nso * sizeof(double));
	if (uvw[i] == NULL) {
	    snprintf(msg, msgLen, "Unable to allocate memory for UVW input %d", i);
	    zvmessage(msg, "");
	    zabend();
	}

	proj_uvw[i] = (double *)malloc(nlo * nso * sizeof(double));
	if (proj_uvw[i] == NULL) {
	    snprintf(msg, msgLen, "Unable to allocate memory for proj_uvw output %d", i);
	    zvmessage(msg, "");
	    zabend();
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

    // Create rotation quat.  We're rotating by angle around +Z

    PigVector axis(0,0,1);
    PigQuaternion rot(axis, angle_rad);

    // Go through each point...

    for (line = 0; line < nlo; line++) {
	for (int samp = 0; samp < nso; samp++) {
	    int index = line * nso + samp;
	    PigVector orig_uvw(*(uvw[0]+index), *(uvw[1]+index),
					*(uvw[2]+index));

    // Rotate the UVW by the negative of rot.  This aligns the IDD Plane
    // with the X-Z plane

	    PigVector plane_uvw = (~rot) * orig_uvw;

    // Zero the Y component.  This projects it to the X-Z plane

	    plane_uvw.setY(0.0);

    // Unrotate back

	    PigVector new_uvw = rot * plane_uvw;

    // normalize

	    new_uvw.normalize();

    // Save it

	    *(proj_uvw[0]+index) = new_uvw.getX();
	    *(proj_uvw[1]+index) = new_uvw.getY();
	    *(proj_uvw[2]+index) = new_uvw.getZ();

	}
    }

    // Write out the projected uvw file(s)...

    for (int band=0; band < 3; band++) {
        for (line = 0; line < nlo; line++) {
	    zvwrit(out_unit, proj_uvw[band] + (line * nso), "BAND",band+1, "LINE", line+1, NULL);
	}
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
	snprintf(msg, msgLen, "MARSUVWPROJ requires either 1 3-band file or 3 single band files as input for %s", type);
	zvmessage(msg, "");
	zabend();
    }
}

