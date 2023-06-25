/* marsc2uvw */
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
#include "PigVector.h"
#include "RadiometryModel.h"
#include "PigCoordSystem.h"
#include "mat3.h"
#include "xvector.h"

#include "return_status.h"

#include "SimpleImage.h"

/* buffer sizes in main program */
#define MAX_INPUTS 2
#define MAX_NS 2048
#define MAX_NL 65536		/* arbitrary; lines are not buffered */

#ifndef MIN
#define MIN(x,y) ((x)<(y) ? (x) : (y))
#endif

#ifndef MAX
#define MAX(x,y) ((x)>(y) ? (x) : (y))
#endif

static int getXYZ(double orig_l, double orig_s, double disp_l, double disp_s,
		PigPoint &xyz,
		PigCameraModel *camera_in[], PigCoordSystem *cs);

////////////////////////////////////////////////////////////////////////


void main44()
{
    int i, j;
    int status, count, def;
	const size_t msgLen = 150;
    char msg[msgLen];

    int nids = 0;
    int nfms = 0;
    char mission[64], instrument[64];
    char filename[PIG_MAX_FILENAME_SIZE+1];
    int nl, ns, nb;

    // Inputs

    // For file models, in addition to creating
    // file model for each input, we also create
    // file models for Disparity Map(s) and coefficient map.
    PigFileModel *file_models[MAX_INPUTS+3];
    PigCameraModel *camera_in[MAX_INPUTS];
    PigPointingModel *pointing_in[MAX_INPUTS];
    int homogeneous_inputs = TRUE;
    int disp_unit_line = 0, disp_unit_samp = 0, disp_band_line = 0, disp_band_samp = 0;
    int coef_unit = 0;
    PigCoordSystem *cs = NULL;

    // Outputs

    int out_unit[3], out_band[3];
    SimpleImage<double> ui, vi, wi;

    // User Parameters

    double vector_distance_l;
    double vector_distance_s;

    // Images

    SimpleImage<double> disp_line;
    SimpleImage<double> disp_samp;
    SimpleImage<double> coef_a;
    SimpleImage<double> coef_b;
    SimpleImage<double> coef_d;
    SimpleImage<double> coef_e;
    SimpleImage<double> coef_g;
    SimpleImage<double> coef_h;

    zvmessage("MARSC2UVW version 1", "");

    // Get the input file list, and set up initial camera/pointing models
    // for each input.  Although we accept two and only two inputs, mars_setup
    // does lots of other nice things for us.

    mars_setup(nids, file_models, camera_in, pointing_in, NULL, cs,
                mission, instrument, homogeneous_inputs,
                MAX_NL, MAX_NS, MAX_INPUTS);

    snprintf(msg, msgLen, "Generating UVW point using the %s coordinate frame",
			cs->getFrameName());
    zvmessage(msg, "");

    if (nids != 2) {
	zvmessage("MARSC2UVW requires 2 and only 2 image inputs", "");
	zabend();
    }

    // Open disparity file(s).
    // DISPAR can be 1 or 2 files, for a single, 2-banded file, or 2
    // single-band files.

    PigMission *m = PigMission::getMissionObject(mission);
    zvpcnt("DISPAR", &count);
    if (count == 1) {
	zvp("DISPAR", filename, &count);
	zvunit(&disp_unit_line, "DISPAR", 1, "u_name", filename, NULL);
	disp_unit_samp = disp_unit_line;
	zvopen(disp_unit_line, "op", "read", "u_format", "doub",
		"open_act", "sa", NULL);
        nfms=nids+1;
        file_models[nids]=m->createFileModel(filename, disp_unit_line);
	zvget(disp_unit_line, "nl", &nl, "ns", &ns, "nb", &nb, NULL);
	if (nb != 2) {
	    zvmessage("A single DISPAR file must have two bands", "");
	    zabend();
	}

	if (nl != file_models[0]->getNL() || ns != file_models[0]->getNS()) {
	    zvmessage("Size of disparity file must match first input", "");
	    zabend();
	}

	disp_band_line = 1;
	disp_band_samp = 2;
    }
    else if (count == 2) {
	zvpone("DISPAR", filename, 1, sizeof(filename)-1);
	zvunit(&disp_unit_line, "DISPAR", 1, "u_name", filename, NULL);
	zvopen(disp_unit_line, "op", "read", "u_format", "doub",
		"open_act", "sa", NULL);
        nfms=nids+2;
        file_models[nids]=m->createFileModel(filename, disp_unit_line);
	zvget(disp_unit_line, "nl", &nl, "ns", &ns, "nb", &nb, NULL);
	if (nb != 1) {
	    zvmessage("A two-file DISPAR must have one band each (#1)", "");
	    zabend();
	}
	if (nl != file_models[0]->getNL() || ns != file_models[0]->getNS()) {
	    zvmessage("Size of disparity file #1 must match first input", "");
	    zabend();
	}
	disp_band_line = 1;

	zvpone("DISPAR", filename, 2, sizeof(filename)-1);
	zvunit(&disp_unit_samp, "DISPAR", 2, "u_name", filename, NULL);
	zvopen(disp_unit_samp, "op", "read", "u_format", "doub",
		"open_act", "sa", NULL);
        file_models[nids+1]=m->createFileModel(filename, disp_unit_samp);
	zvget(disp_unit_samp, "nl", &nl, "ns", &ns, "nb", &nb, NULL);
	if (nb != 1) {
	    zvmessage("A two-file DISPAR must have one band each (#2)", "");
	    zabend();
	}
	if (nl != file_models[0]->getNL() || ns != file_models[0]->getNS()) {
	    zvmessage("Size of disparity file #2 must match first input", "");
	    zabend();
	}
	disp_band_samp = 1;
    }
    else {
	zvmessage("DISPAR must have 1 or 2 files", "");
	zabend();
    }

    // Open coefficients file.

    zvp("COEF", filename, &count);
    zvunit(&coef_unit, "COEF", 1, "u_name", filename, NULL);
    zvopen(coef_unit, "op", "read", "u_format", "doub",
		"open_act", "sa", NULL);
    nfms++;
    file_models[nfms-1]=m->createFileModel(filename, coef_unit);
    zvget(coef_unit, "nl", &nl, "ns", &ns, "nb", &nb, NULL);
    if (nb != 6) {
	zvmessage("COEF file must have six bands", "");
	zabend();
    }

    if (nl != file_models[0]->getNL() || ns != file_models[0]->getNS()) {
	zvmessage("Size of coef file must match first input", "");
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
		"open_act", "sa", "u_org", "bsq",
		"u_format", "doub", "o_format", "real", NULL);
	zvplabel(out_unit[0], 0, 1);
	out_unit[1] = out_unit[0];
	out_unit[2] = out_unit[0];
	out_band[0] = 1;
	out_band[1] = 2;
	out_band[2] = 3;
	
	// write output label
	PigMission *m = PigMission::getMissionObject(mission);
	PigLabelModel *labelModel = m->createLabelModel(out_unit[0]);
	// pick the coordinate system to use.
	labelModel->setUVW(file_models, nfms, cs, "UVW_MAP");
	if (zvptst("WRITE_CM"))
	    labelModel->writeCM(camera_in[0], camera_in[0]->getCoordSystem());
    }
    else if (count == 3) {
      char* image_type[3] = {"U_MAP", "V_MAP", "W_MAP"};
	for (i=0; i<3; i++) {
	    zvunit(&out_unit[i], "OUT", i+1, NULL);
	    zvopen(out_unit[i], "op", "write",
		"u_ns", file_models[0]->getNS(), "u_nl",file_models[0]->getNL(),
		"u_nb", 1,
		"open_act", "sa", "u_org", "bsq",
		"u_format", "doub", "o_format", "real", NULL);
	    zvplabel(out_unit[i], 0, 1);
	    out_band[i] = 1;

	    // write output label
	    PigMission *m = PigMission::getMissionObject(mission);
	    PigLabelModel *labelModel = m->createLabelModel(out_unit[i]);
	    // pick the coordinate system to use.
	    labelModel->setUVW(file_models, nids, cs, image_type[i]);
	    if (zvptst("WRITE_CM"))
	        labelModel->writeCM(camera_in[0],
					camera_in[0]->getCoordSystem());
	}
    }
    else {
	zvmessage("OUT must have 1 or 3 filenames", "");
	zabend();
    }

    // Allocate working images

    disp_line.alloc(nl, ns);
    disp_samp.alloc(nl, ns);
    coef_a.alloc(nl, ns);
    coef_b.alloc(nl, ns);
    coef_d.alloc(nl, ns);
    coef_e.alloc(nl, ns);
    coef_g.alloc(nl, ns);
    coef_h.alloc(nl, ns);

    // Allocate output images

    ui.alloc(nl, ns);
    vi.alloc(nl, ns);
    wi.alloc(nl, ns);

    // Read the correlation and coef data.

    zvmessage("Reading disparity and coef file(s)...", "");
    for (j=0; j < nl; j++) {		// line

	zvread(disp_unit_line, disp_line.linePtr(j),
		"LINE", j+1, "BAND", disp_band_line, NULL);
	zvread(disp_unit_samp, disp_samp.linePtr(j),
		"LINE", j+1, "BAND", disp_band_samp, NULL);

	zvread(coef_unit, coef_a.linePtr(j), "LINE", j+1, "BAND", 1, NULL);
	zvread(coef_unit, coef_b.linePtr(j), "LINE", j+1, "BAND", 2, NULL);
	zvread(coef_unit, coef_d.linePtr(j), "LINE", j+1, "BAND", 3, NULL);
	zvread(coef_unit, coef_e.linePtr(j), "LINE", j+1, "BAND", 4, NULL);
	zvread(coef_unit, coef_g.linePtr(j), "LINE", j+1, "BAND", 5, NULL);
	zvread(coef_unit, coef_h.linePtr(j), "LINE", j+1, "BAND", 6, NULL);
    }

    // Now calculate the UVW's...

    // The goal here is to determine the surface normal of the plane
    // defined by the perspective transform.
    //
    // Any two (non-coincident) vectors in a plane define the normal to
    // the plane.
    //
    // So construct an arbitrary-size vector in the L image (say, straight up).
    // Tail is at the correlation point, head is some distance straight up.
    // Project that into space, you know the 3D vector starts and ends somewhere
    // along those lines.
    //
    // Take those two points (image-space vector endpoints) and transform them
    // to the R image using the affine (perspective) transform.  Project that
    // into space.
    //
    // Since both image-space vectors should be projections of the same 3D
    // vector, the intersections of the two tail and two head projections
    // should define the tail and head of the 3D vector, in 3D space.
    // Normalize.
    //
    // Then do the same with moving, say, straight left in image space.  Now you
    // have two 3D vectors which should lie on the plane defined by the
    // transform.  Cross them to get the surface normal.

    zvparmd("DIST_LINE", &vector_distance_l, &count, &def, 1, 0);
    zvparmd("DIST_SAMP", &vector_distance_s, &count, &def, 1, 0);

    zvmessage("Calculating UVW's...", "");

    for (j=0; j < nl; j++) {

	for (i=0; i < ns; i++) {

	    ui.set(j,i,0.0);			// in case the pt is rejected
	    vi.set(j,i,0.0);
	    wi.set(j,i,0.0);

	    // Skip missing points

	    if ((fabs((double)(disp_samp.get(j,i))) < .0001) &&
		(fabs((double)(disp_line.get(j,i))) < .0001)) {
		continue;
	    }

	    PigPoint vector_tail;

	    // Convert points to camera coordinates

	    double orig_l = j + file_models[0]->getYOffset();
	    double orig_s = i + file_models[0]->getXOffset();

	    // disparity images are 1-based, subtract 1 to convert
	    // to 0-base for camera model
	    double disp_l = disp_line.get(j,i)-1 + file_models[1]->getYOffset();
	    double disp_s = disp_samp.get(j,i)-1 + file_models[1]->getXOffset();

	    status = getXYZ(orig_l, orig_s, disp_l, disp_s,
				vector_tail, camera_in, cs);
////printf("tail: l=%f s=%f l2=%f s2=%f x=%f y=%f z=%f status=%d\n", orig_l, orig_s, disp_l, disp_s, vector_tail.getX(), vector_tail.getY(), vector_tail.getZ(), status);	//!!!!
	    if (status != 0) continue;

	    // Perturb the solution up some distance in image space

	    double orig_l2 = - vector_distance_l;
	    double orig_s2 = 0.0;

	    // Run the transform to get the R side

//!!!! Doing it this way means g and h are ignored, because one of the
//!!!! x or y terms is always 0!!  Meaning we're only looking at a,b,d,e to get
//!!!! the surface normal!!  Can that possibly be correct???!!!!

	    double disp_s2 = orig_s2 * coef_a.get(j,i) +
			     orig_l2 * coef_b.get(j,i) +
			     orig_s2 * orig_l2 * coef_g.get(j,i);
	    double disp_l2 = orig_s2 * coef_d.get(j,i) +
			     orig_l2 * coef_e.get(j,i) +
			     orig_s2 * orig_l2 * coef_h.get(j,i);


	    PigPoint vector_head_up;
	    status = getXYZ(orig_l2, orig_s2, disp_l2, disp_s2,
				vector_head_up, camera_in, cs);
////printf("headu: l=%f s=%f l2=%f s2=%f x=%f y=%f z=%f status=%d\n", orig_l2, orig_s2, disp_l2, disp_s2, vector_head_up.getX(), vector_head_up.getY(), vector_head_up.getZ(), status);	//!!!!
	    if (status != 0) continue;

	    // Perturb the solution left some distance in image space

	    orig_l2 = 0.0;
	    orig_s2 = - vector_distance_s;

	    // Run the transform to get the R side

//!!!! Doing it this way means g and h are ignored, because one of the
//!!!! x or y terms is always 0!!  Meaning we're only looking at a,b,d,e to get
//!!!! the surface normal!!  Can that possibly be correct???!!!!

	    disp_s2 = orig_s2 * coef_a.get(j,i) +
		      orig_l2 * coef_b.get(j,i) +
		      orig_s2 * orig_l2 * coef_g.get(j,i);
	    disp_l2 = orig_s2 * coef_d.get(j,i) +
		      orig_l2 * coef_e.get(j,i) +
		      orig_s2 * orig_l2 * coef_h.get(j,i);

	    PigPoint vector_head_left;
	    status = getXYZ(orig_l2, orig_s2, disp_l2, disp_s2,
				vector_head_left, camera_in, cs);
////printf("headl: l=%f s=%f l2=%f s2=%f x=%f y=%f z=%f status=%d\n", orig_l2, orig_s2, disp_l2, disp_s2, vector_head_left.getX(), vector_head_left.getY(), vector_head_left.getZ(), status);	//!!!!
	    if (status != 0) continue;


	    // Now construct the two 3-space vectors, normalize, and cross
	    // to get the surface normal

	    PigVector vector_up = vector_head_up - vector_tail;
	    PigVector vector_left = vector_head_left - vector_tail;
	    vector_up.normalize();
	    vector_left.normalize();

//!!!! really should be up * left I think:
	    PigVector normal = vector_up * vector_left;
//!!!!	    PigVector normal = vector_left * vector_up;
	    normal.normalize();


	    // Save results away

	    ui.set(j,i, normal.getX());
	    vi.set(j,i, normal.getY());
	    wi.set(j,i, normal.getZ());

	}

    }
    disp_line.free();
    disp_samp.free();
    coef_a.free();
    coef_b.free();
    coef_d.free();
    coef_e.free();
    coef_g.free();
    coef_h.free();

    // Write out the uvw image

    for (j=0; j < nl; j++) {
	zvwrit(out_unit[0], ui.linePtr(j), "LINE",j+1, "BAND",out_band[0],NULL);
	zvwrit(out_unit[1], vi.linePtr(j), "LINE",j+1, "BAND",out_band[1],NULL);
	zvwrit(out_unit[2], wi.linePtr(j), "LINE",j+1, "BAND",out_band[2],NULL);
    }

    zvclose(out_unit[0], NULL);
    if (out_unit[1] != out_unit[0])
	zvclose(out_unit[1], NULL);
    if (out_unit[2] != out_unit[0])
	zvclose(out_unit[2], NULL);
}


////////////////////////////////////////////////////////////////////////
// Get the XYZ point corresponding to the given coordinates in the two images
////////////////////////////////////////////////////////////////////////

static int getXYZ(double orig_l, double orig_s, double disp_l, double disp_s,
		PigPoint &xyz,
		PigCameraModel *camera_in[], PigCoordSystem *cs)
{
    int status;

    // Compute unit vectors for both points

    PigPoint orig_origin, disp_origin;
    PigVector orig_vector, disp_vector;

    camera_in[0]->LStoLookVector(orig_l, orig_s,
					orig_origin, orig_vector, cs);
    camera_in[1]->LStoLookVector(disp_l, disp_s,
					disp_origin, disp_vector, cs);

    // Compute x,y,z and error by finding the intersection of the
    // two rays.  We really compute the closest point between the lines
    // since they'll almost never intersect.

    double error_value;
    status = xvector(orig_origin, disp_origin, orig_vector, disp_vector,
			xyz, &error_value);
#if 0	//!!!!
if (orig_l > 500 && orig_l < 510 && orig_s > 500 && orig_s < 510)	//!!!!
  printf("error_value: %f\n", error_value);	//!!!!
#endif	//!!!!
    if (status != 0) {
	return status;
    }

    return 0;
}

