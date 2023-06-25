/* marsprojfid */
#include <math.h>
#include <iostream>
using namespace std;

#include "vicmain_c"

#include "mars_support.h"
#include "mars_tiepoints.h"

#include "PigMission.h"
#include "PigFileModel.h"
#include "PigLabelModel.h"
#include "PigCameraModel.h"
#include "PigPointingModel.h"
#include "PigSurfaceModel.h"
#include "PigVector.h"
#include "RadiometryModel.h"
#include "PigCoordSystem.h"
#include "mat3.h"

/* buffer sizes in main program */
#define MAX_INPUTS 2000
#define MAX_NS 2048
#define MAX_NL 65536		/* arbitrary; lines are not buffered */

#ifndef MIN
#define MIN(x,y) ((x)<(y) ? (x) : (y))
#endif

#ifndef MAX
#define MAX(x,y) ((x)>(y) ? (x) : (y))
#endif

static PigSurfaceModel *createPrivateSurfaceModel(PigSurfaceModel *old,
			PigCoordSystem *new_cs, PigCoordSystem *old_cs);

void main44()
{
    int i, j;
    int status, count, def;
    const size_t msgLen = 512;
    char msg[msgLen];

    int nids;
    char mission[64], instrument[64];
    char filename[PIG_MAX_FILENAME_SIZE+1];
    int nl, ns, nb;
    double error_value, range;

    // Inputs

    PigFileModel *file_models[MAX_INPUTS];
    PigCameraModel *camera_in[MAX_INPUTS];
    PigPointingModel *pointing_in[MAX_INPUTS];
    int homogeneous_inputs = TRUE;
    PigCoordSystem *cs;
    PigSurfaceModel *surface_model;

    // Outputs

    double out_line, out_samp;
    double image_pos[2];

    // User Parameters

    int long_format;

    char tie_filename[250];
    int n_ties = MARS_MAX_TIEPOINTS;
    TiePoint *tiepoints = new (std::nothrow) TiePoint[MARS_MAX_TIEPOINTS];
    if(tiepoints == NULL){
      zvmessage("Memory allocation for TiePoint array failed","");
      zabend();
    }

    zvmessage("MARSPROJFID version 1", "");

    // Get the input file list, and set up initial camera/pointing models
    // for each input.

    mars_setup(nids, file_models, camera_in, pointing_in, surface_model,
	       NULL, cs, mission, instrument, homogeneous_inputs,
               MAX_NL, MAX_NS, MAX_INPUTS);

    // Check for coord sys override

    if (zvptst("FORCE_COORD")) {
	snprintf(msg, msgLen, "XYZ coordinates forced to %s coordinate frame",
						cs->getFrameName());
    }
    else {
	snprintf(msg, msgLen, "Using coord system from tiepoint file");
	cs = NULL;
    }
    zvmessage(msg, "");

    long_format = zvptst("LONG_FORMAT");

    // Get tiepoint file

    zvp("TIEPOINT", tie_filename, &count);
    mars_load_tiepoints(tie_filename, tiepoints, n_ties, file_models, nids,
		cs, PigMission::getMissionObject(mission));

    // Loop through tiepoints, and do it

    if (long_format)
        zvmessage("  X          Y          Z       tie_line    tie_samp    proj_line   proj_samp  delta_line  delta_samp camera_x  camera_y  camera_z  coord_sys          filename", "");
    else
        zvmessage("  X          Y          Z       tie_line    tie_samp    proj_line   proj_samp  delta_line  delta_samp", "");

    for (int i=0; i < n_ties; i++) {

	if (tiepoints[i].type != TIEPOINT_FIDUCIAL)
	    continue;				// not interested...

	if (!tiepoints[i].active)
	    continue;

	int image = tiepoints[i].left_image;
	if (image < 0 || image >= nids) {
	    snprintf(msg, msgLen, "Warning: tiepoint %d, left_image %d is out of range",
					i, image);
	    zvmessage(msg, "");
	    continue;
	}

        camera_in[image]->XYZtoLS(tiepoints[i].xyz, FALSE,
				&out_line, &out_samp, tiepoints[i].cs);

        // Adjust for file offsets

        out_line -= file_models[image]->getYOffset();
        out_samp -= file_models[image]->getXOffset();

        // Convert to 1-based

        out_line += 1.0;
        out_samp += 1.0;

        // Report results

	if (long_format)
	    snprintf(msg, msgLen, "%lf, %lf, %lf, %lf, %lf, %lf, %lf, %lf, %lf, %lf, %lf, %lf, %s, %s",
			tiepoints[i].xyz.getX(),
			tiepoints[i].xyz.getY(),
			tiepoints[i].xyz.getZ(),
			tiepoints[i].left_line,
			tiepoints[i].left_sample,
			out_line,
			out_samp,
			(tiepoints[i].left_line - out_line),
			(tiepoints[i].left_sample - out_samp),
			camera_in[image]->getCameraPosition().getX(),
			camera_in[image]->getCameraPosition().getY(),
			camera_in[image]->getCameraPosition().getZ(),
			tiepoints[i].cs->getFrameName(),
			file_models[image]->getFilename());
	else
	    snprintf(msg, msgLen, "%lf, %lf, %lf, %lf, %lf, %lf, %lf, %lf, %lf",
			tiepoints[i].xyz.getX(),
			tiepoints[i].xyz.getY(),
			tiepoints[i].xyz.getZ(),
			tiepoints[i].left_line,
			tiepoints[i].left_sample,
			out_line,
			out_samp,
			(tiepoints[i].left_line - out_line),
			(tiepoints[i].left_sample - out_samp));

	zvmessage(msg, "");
    }
    
    delete[] tiepoints;
}

