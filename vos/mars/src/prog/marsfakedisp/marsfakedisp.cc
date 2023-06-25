/* marsfakedisp */
#include <math.h>
#include <iostream>
using namespace std;

#include "vicmain_c"

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

/* buffer sizes in main program */
#define MAX_INPUTS 2
#define MAX_NS 4096
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
    int homogeneous_inputs = TRUE;
    char frame_name[256];
    PigCoordSystem *cs, *fixed_cs;
    PigSurfaceModel *surface_model;

    // Outputs

    int out_unit[2], out_band[2];
    int out_nl, out_ns;
    int num_out_files;
    float line_disp[MAX_NS];
    float samp_disp[MAX_NS];

    // User Parameters

    int disp_pyramid, disp_zoom;


    zvmessage("MARSFAKEDISP version 1", "");

    // Get the input file list, and set up initial camera/pointing models
    // for each input.  Although we accept two and only two inputs, mars_setup
    // does lots of other nice things for us.

    mars_setup(nids, file_models, camera_in, pointing_in, surface_model,
	       NULL, cs,mission, instrument, homogeneous_inputs,
               MAX_NL, MAX_NS, MAX_INPUTS);

    snprintf(msg, msgLen, "Generating surface using the %s coordinate frame",
			cs->getFrameName());
    zvmessage(msg, "");

    if (nids != 2) {
	zvmessage("MARSXYZ requires 2 and only 2 image inputs", "");
	zabend();
    }

    fixed_cs = surface_model->getCoordSystem();

    //!!!! THIS IS A COMPLETE AND TOTAL HACK !!!!

    if (fixed_cs != cs)
	surface_model = createPrivateSurfaceModel(surface_model, fixed_cs, cs);

    // Get parameter overrides if any

    zvp("PYRLEVEL", &disp_pyramid, &count);
    disp_zoom = 1 << disp_pyramid;		// convert to zoom factor
    snprintf(msg, msgLen, "Zooming disparity by %d (pyramid level %d)",
				disp_zoom, disp_pyramid);
    zvmessage(msg, "");

    out_nl = (file_models[0]->getNL() / disp_zoom);
    out_ns = (file_models[0]->getNS() / disp_zoom);
    if (out_nl < 1) out_nl = 1;
    if (out_ns < 1) out_ns = 1;

    // Create output files...

    zvpcnt("OUT", &num_out_files);
    int num_bands = 2;			// assume same file, bands 1 and 2
    out_band[0] = 1;
    out_band[1] = 2;
    if (num_out_files > 1) {		// separate files, band 1 for each
	    num_bands = 1;
	    out_band[1] = 1;
    }

    // Open the output file (or line disparity file if two)

    zvunit(&out_unit[0], "OUT", 1, NULL);
    zvopen(out_unit[0], "op", "write",
	"u_ns", out_ns, "u_nl", out_nl, "u_nb", num_bands,
	"open_act", "sa", "u_org", "bsq",
	"u_format", "real", "o_format", "real", NULL);
    zvplabel(out_unit[0], 0, 1);

    // We need mission object to write out output label.
    PigMission *m = PigMission::getMissionObject(mission);
    PigLabelModel *labelModel_0 = NULL;
    if (m)
	labelModel_0 = m->createLabelModel(out_unit[0]);

    // Open the sample disparity file if needed

    if (num_out_files <= 1) {
		out_unit[1] = out_unit[0];		// share the file

	// write output label for 2-banded image
	if (labelModel_0)
	    labelModel_0->setDisparity(file_models, file_models[1], nids, 
                                       "DISPARITY_MAP");

    }
    else {
		zvunit(&out_unit[1], "OUT", 2, NULL);
	zvopen(out_unit[1], "op", "write",
		"u_ns", out_ns, "u_nl", out_nl, "u_nb", num_bands,
		"open_act", "sa", "u_org", "bsq",
		"u_format", "real", "o_format", "real", NULL);
	zvplabel(out_unit[1], 0, 1);

	// Write output label here for two 1-banded images
	if (m) {
	    PigLabelModel *labelModel_1 = NULL;
	    labelModel_1 = m->createLabelModel(out_unit[1]);
	    if (labelModel_0)
			labelModel_0->setDisparity(file_models, file_models[1], 
                                                   nids, "DISPARITY_LINE_MAP");
	    if (labelModel_1)
			labelModel_1->setDisparity(file_models, file_models[1], 
                                                   nids, "DISPARITY_SAMPLE_MAP");
	}
    }

    // Now process the image.  We take each pixel on the left, project to
    // the ground, back into the right, and that's the "disparity" value.
    // We just have to pay attention to the pyramid zoom factor...

    for (j = 0; j < out_nl; j++) {

	double line = j * disp_zoom +
				file_models[0]->getYOffset();	// 0-based

	for (int i = 0; i < out_ns; i++) {

	    double samp = i * disp_zoom +
				file_models[0]->getXOffset();	// 0-based

	    PigPoint origin;
	    PigVector look;
	    PigPoint surf_pt;

	    camera_in[0]->LStoLookVector(line, samp, origin, look, fixed_cs);

	    int hits = surface_model->intersectRay(origin, look, surf_pt);
	    int infinity = (hits <= 0);

	    // Don't have to worry about FOV check for this application...

	    double out_line, out_samp;
	    camera_in[1]->XYZtoLS(surf_pt, infinity, &out_line, &out_samp,
								fixed_cs);

	    // Zoom it, and convert to 1-based

	    line_disp[i] = ((out_line - file_models[1]->getYOffset())
							/ disp_zoom) + 1;
	    samp_disp[i] = ((out_samp - file_models[1]->getXOffset())
							/ disp_zoom) + 1;

	    // Should we trap out-of-bounds numbers here??!!!!

	}

	zvwrit(out_unit[0], line_disp, "LINE", j+1, "BAND", out_band[0], NULL);
	zvwrit(out_unit[1], samp_disp, "LINE", j+1, "BAND", out_band[1], NULL);

    }

    // Close up...

    zvclose(out_unit[0], NULL);
    if (num_out_files > 1)
	zvclose(out_unit[1], NULL);

}

//!!!! THIS IS A COMPLETE AND TOTAL HACK !!!!
//!!!! Create a surface model by interpreting the parameters in a different
//!!!! frame
//!!!! This sort of capability SHOULD be in the base classes...

PigSurfaceModel *createPrivateSurfaceModel(PigSurfaceModel *old,
			PigCoordSystem *new_cs, PigCoordSystem *old_cs)
{
    if (strcmp(old->getModelName(), "TiltedPlane") == 0) {
	double x[6];
	old->getPointingParameters(x, 6);
	PigPoint ground(&x[0]);
	PigVector normal(&x[3]);

printf("orig g.x=%f, g.y=%f, g.z=%f, n.x=%f, n.y=%f, n.z=%f\n", x[0],x[1],x[2],x[3],x[4],x[5]);
	PigPoint ground2 = new_cs->convertPoint(ground, old_cs);
	PigVector normal2 = new_cs->convertVector(normal, old_cs);

	if (zvptst("AVERAGE")) {	// average site and rover frames
	    normal = (normal + normal2) / 2;
	    ground = (ground + ground2) / 2;
	}
	else {
	    normal = normal2;
	    ground = ground2;
	}

	ground.getXYZ(&x[0]);
        normal.getXYZ(&x[3]);
printf("new  g.x=%f, g.y=%f, g.z=%f, n.x=%f, n.y=%f, n.z=%f\n", x[0],x[1],x[2],x[3],x[4],x[5]);
	old->setPointingParameters(x, 6);

	return old;

    } else if ((strcmp(old->getModelName(), "Sphere1") == 0) ||
	       (strcmp(old->getModelName(), "Sphere2") == 0)) {

	// Note that radius doesn't change, so we ignore param[4]

	double x[3];
	old->getPointingParameters(x, 3);
	PigPoint ground(x);

	ground = new_cs->convertPoint(ground, old_cs);
	ground.getXYZ(x);
	old->setPointingParameters(x, 3);

	return old;

    } 

    return old;			// no changes
}

