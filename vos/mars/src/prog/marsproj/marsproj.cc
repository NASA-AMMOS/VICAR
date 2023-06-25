/* marsproj */
#include "vicmain_c"

#include <math.h>
#include <iostream>
using namespace std;

#include "mars_support.h"

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

#include "return_status.h"

#include "taeconf.inp"
#include "parblk.inc"
#include "pgminc.inc"

/* buffer sizes in main program */
#define MAX_INPUTS 1
#define MAX_NS 65536
#define MAX_NL 65536		/* arbitrary; lines are not buffered */

#ifndef MIN
#define MIN(x,y) ((x)<(y) ? (x) : (y))
#endif

#ifndef MAX
#define MAX(x,y) ((x)>(y) ? (x) : (y))
#endif

static PigSurfaceModel *createPrivateSurfaceModel(PigSurfaceModel *old,
			PigCoordSystem *new_cs, PigCoordSystem *old_cs);

extern "C" {
    int q_real(struct PARBLK *p, char *name, int count, double *real, int mode);
    int q_init(struct PARBLK *p, int pool_size, int mode);
}

void main44()
{
    int i, j;
    int status, count, def;
    const size_t msgLen = 150;
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
    double FOV[MAX_INPUTS];				
    int homogeneous_inputs = TRUE;
    PigCoordSystem *cs;
    PigSurfaceModel *surface_model;

    // Outputs

    double out_line, out_samp;
    double image_pos[2];

    // User Parameters

    double xyz_array[3];
    PigPoint xyz;

    zvmessage("MARSPROJ version 1", "");

    // Get the input file list, and set up initial camera/pointing models
    // for each input.  Although we accept one and only one input, mars_setup
    // does lots of other nice things for us.

    mars_setup(nids, file_models, camera_in, pointing_in, surface_model,
	       NULL, cs,mission, instrument, homogeneous_inputs,
               MAX_NL, MAX_NS, MAX_INPUTS);
    
    snprintf(msg, msgLen, "Interpreting XYZ value using the %s coordinate frame",
			cs->getFrameName());
    zvmessage(msg, "");

    if (nids != 1) {
	zvmessage("MARSPROJ requires 1 and only 1 image input", "");
	zabend();
    }

    // Get parameter

    zvparmd("XYZ", &xyz_array, &count, &def, 3, 0);
    xyz.setXYZ(xyz_array);

    snprintf(msg, msgLen, "Translating: (%lf, %lf, %lf)",
					xyz.getX(), xyz.getY(), xyz.getZ());
    zvmessage(msg, "");

				       
    FOV[0] = cos((file_models[0]->getFOV(camera_in[0], 0) +
		  file_models[0]->getFOV(camera_in[0], 1)) / 2);
    if (FOV[0] < 0.0)
      FOV[0] = 0.0;		// Limit to 90 degrees

    double new_fov;
    zvparmd("FOV", &new_fov, &count, &def, 1, 0);
    if (count != 0) {
	snprintf(msg, msgLen, "Overriding FOV from %f to %f\n", PigRad2Deg(acos(FOV[0])), new_fov);
	zvmessage(msg, "");
	FOV[0] = cos(PigDeg2Rad(new_fov));
    }

    PigVector camera_orientation = pointing_in[0]->getCameraOrientation(cs);
    PigPoint camera_position = pointing_in[0]->getCameraPosition(cs);
    PigVector new_look = xyz - camera_position;
    new_look.normalize();
    

    if ((new_look % camera_orientation) < FOV[0]) {
          zvmessage("XYZ point is not within image FOV. Exiting.", "");
	  exit(1);
    } 
    
    camera_in[0]->XYZtoLS(xyz, FALSE, &out_line, &out_samp, cs);

    // Adjust for file offsets

    snprintf(msg, msgLen, "Adjusting for file offset, y=%lf, x=%lf",
		file_models[0]->getYOffset(), file_models[0]->getXOffset());
    zvmessage(msg, "");

    out_line -= file_models[0]->getYOffset();
    out_samp -= file_models[0]->getXOffset();

    // Convert to 1-based

    out_line += 1.0;
    out_samp += 1.0;

    // Report results

    if (file_models[0]->testPixelLocation(out_line, out_samp) != 0) {
        if (zvptst("MISS")) {
	    zvmessage("XYZ point projects outside the image, allowed by -MISS parameter", "");
	}
	else {
            zvmessage("XYZ point is not within image. Exiting.", "");
            exit(1);
	}
    } 
    
    snprintf(msg, msgLen, "LINE = %lf", out_line);
    zvmessage(msg, "");
    snprintf(msg, msgLen, "SAMP = %lf", out_samp);
    zvmessage(msg, "");
    snprintf(msg, msgLen, "%lf %lf", out_line, out_samp);
    zvmessage(msg, "");
      
    // Output to TAE

    image_pos[0] = out_line;
    image_pos[1] = out_samp;
    struct PARBLK par_block;
    q_init(&par_block, P_BYTES, P_ABORT);
    q_real(&par_block, "IMAGE_POS", 2, image_pos, P_ADD);
    zvq_out(&par_block);
    exit(0);
}

