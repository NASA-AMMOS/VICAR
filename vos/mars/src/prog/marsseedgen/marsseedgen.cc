/* marsxyz */
#include "vicmain_c"

#include "mars_support.h"
#include "mars_tiepoints.h"

#include "PigFileModel.h"
#include "PigCameraModel.h"
#include "PigPointingModel.h"
#include "PigSurfaceModel.h"
#include "PigVector.h"
#include "RadiometryModel.h"
#include "PigCoordSystem.h"

#include <math.h>

/* buffer sizes in main program */
#define MAX_INPUTS 2
#define MAX_NL 65536			/* doesn't matter, not used */
#define MAX_NS MARS_MAX_NS
#define MAX_NSEED 100

////////////////////////////////////////////////////////////////////////

void main44()
{
    int i, j;
    int status, count, def;
    char msg[150];

    int nids;
    char mission[64], instrument[64];

    // Inputs

    PigFileModel *file_models[MAX_INPUTS];
    PigCameraModel *camera_in[MAX_INPUTS];
    PigPointingModel *pointing_in[MAX_INPUTS];
    PigSurfaceModel *surface;
    int homogeneous_inputs = TRUE;
    char frame_name[256];
    PigCoordSystem *coord_sys, *dummy_cs;
    double nl, ns, cl, cs;

    // Outputs

    char out_file[256];
    struct TiePoint tiepoints[MAX_NSEED];
    int n_ties;

    // User Parameters

    int n_seeds;

    zvmessage("MARSSEEDGEN version 1", "");

    // Get the input file list, and set up initial camera/pointing models
    // for each input.  Although we accept two and only two inputs, mars_setup
    // does lots of other nice things for us.

    mars_setup(nids, file_models, camera_in, pointing_in, surface,
	       NULL, dummy_cs, mission, instrument, homogeneous_inputs,
                MAX_NL, MAX_NS, MAX_INPUTS);

    // Get the coordinate system to use.

    coord_sys = surface->getCoordSystem();

    if (nids != 2) {
	zvmessage("MARSSEEDGEN requires 2 and only 2 image inputs", "");
	zabend();
    }

    // Get parameter overrides if any

    zvp("NSEEDS", &n_seeds, &count);
    zvp("OUT", out_file, &count);

    nl = file_models[0]->getNL();
    ns = file_models[0]->getNS();
    cl = nl / 2.0;
    cs = ns / 2.0;

    // Determine seed points for left (first) image.
    //!!!! This should be done much better.  Look at the overlap area,
    //!!!! and scatter things around inside it.  Instead, we simply grab
    //!!!! first the center, then 3/8 to each corner, then 3/8 to
    //!!!! each edge.  3/8 is used instead of 1/2 so that we have a better
    //!!!! chance of being inside the second image, by being closer to the
    //!!!! center.

    n_ties = 0;

    // Center
    if (n_seeds > 0) {
	tiepoints[n_ties].left_sample = cs;
	tiepoints[n_ties++].left_line = cl;
    }
    // 3/8 of the way to each corner
    if (n_seeds > 1) {
	tiepoints[n_ties].left_sample = cs*5.0/8.0;
	tiepoints[n_ties++].left_line = cl*5.0/8.0;
    }
    if (n_seeds > 2) {
	tiepoints[n_ties].left_sample = cs + cs*3.0/8.0;
	tiepoints[n_ties++].left_line = cl + cl*3.0/8.0;
    }
    if (n_seeds > 3) {
	tiepoints[n_ties].left_sample = cs*5.0/8.0;
	tiepoints[n_ties++].left_line = cl + cl*3.0/8.0;
    }
    if (n_seeds > 4) {
	tiepoints[n_ties].left_sample = cs + cs*3.0/8.0;
	tiepoints[n_ties++].left_line = cl*5.0/8.0;
    }
    // 3/8 of the way to each side
    if (n_seeds > 5) {
	tiepoints[n_ties].left_sample = cs*5.0/8.0;
	tiepoints[n_ties++].left_line = cl;
    }
    if (n_seeds > 6) {
	tiepoints[n_ties].left_sample = cs + cs*3.0/8.0;
	tiepoints[n_ties++].left_line = cl;
    }
    if (n_seeds > 7) {
	tiepoints[n_ties].left_sample = cs;
	tiepoints[n_ties++].left_line = cl*5.0/8.0;
    }
    if (n_seeds > 8) {
	tiepoints[n_ties].left_sample = cs;
	tiepoints[n_ties++].left_line = cl + cl*3.0/8.0;
    }
    if (n_seeds > 9)
	zvmessage("Current maximum of 9 seeds used", "");

    // Now find the corresponding point for each seed, and fill in the structure

    for (i=0; i < n_ties; i++) {

	tiepoints[i].left_image = 0;
	tiepoints[i].right_image = 1;
	tiepoints[i].quality = 0.0;
	tiepoints[i].interactive = 0;

	PigPoint origin, surf_pt;
	PigVector look;
	int hits;
	double line, samp;

	camera_in[0]->LStoLookVector(tiepoints[i].left_line,
			tiepoints[i].left_sample, origin, look, coord_sys);
	hits = surface->intersectRay(origin, look, surf_pt);
	camera_in[1]->XYZtoLS(surf_pt, (hits<=0), &line, &samp, coord_sys);

	// Check to make sure projected point is inside right image

	if (file_models[1]->testPixelLocation(line, samp) == 0) {
	    tiepoints[i].right_line = line;
	    tiepoints[i].right_sample = samp;
	    tiepoints[i].corrected_line = line;
	    tiepoints[i].corrected_sample = samp;
	}
	else {			// nope, remove the point
	    tiepoints[i].quality = -10.0;
	}
    }

    // Remove bad points (that didn't overlap the right image)

    mars_remove_bad_points(tiepoints, n_ties, -1.0);

    // Print out tiepoints

    zvmessage("Seed point list (camera coordinates):", "");
    mars_print_tiepoint_list(tiepoints, n_ties);

    // Save tiepoints

    mars_save_tiepoints(out_file, tiepoints, n_ties);
}

