/* marsxyz */
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

#include "return_status.h"

#include "SimpleImage.h"

/* buffer sizes in main program */
#define MAX_INPUTS 2
#define MAX_NS 0		// no limits
#define MAX_NL 0
#define BAD_DELTA -999999

#ifndef MIN
#define MIN(x,y) ((x)<(y) ? (x) : (y))
#endif

#ifndef MAX
#define MAX(x,y) ((x)>(y) ? (x) : (y))
#endif

static int xvector(const PigPoint cam1, const PigPoint cam2,
				   const PigVector uvw1, const PigVector uvw2,
				   PigPoint &xyz, double *error);
static void dsimq(double A[10], double B[10], int N, int *KS);

void  lcross(double b1[3],
	     double v1[3],
	     double b2[3],
	     double v2[3],
	     double *k1,
	     double p1[3],
	     double *k2,
	     double p2[3],
	     int *colinear);

void compute_box_filter(SimpleImage<double> &input,
			SimpleImage<double> &output,
			int box_height, int box_width, double ignore);

////////////////////////////////////////////////////////////////////////


void main44()
{
    int i, j;
    int status, count, def;
    const int MSG_LEN = 150;
    char msg[MSG_LEN];

    int nids;
    int nfms = 0;
    char mission[64], instrument[64];
    char filename[PIG_MAX_FILENAME_SIZE+1];
    int nl = 0, ns = 0, nb;
    double error_value, range;

    // Inputs

    // For file models, in addition to creating
    // file model for each input, we also create
    // file models for Disparity Map(s).
    PigFileModel *file_models[MAX_INPUTS+2];
    PigCameraModel *camera_in[MAX_INPUTS];
    PigPointingModel *pointing_in[MAX_INPUTS];
    int homogeneous_inputs = TRUE;
    int disp_unit_line = 0, disp_unit_samp = 0, disp_band_line = 0, disp_band_samp = 0;
    PigCoordSystem *cs;

    // Outputs

    int out_unit[3], out_band[3];
    int error_unit;
    SimpleImage<double> xi, yi, zi;
    SimpleImage<double> error_img;

    // User Parameters

    double error_check, abs_error_check;
    double linedisp_check;
    double avglinedisp_check;
    int box_width, box_height;
    double zlimits[2];
    double range_limit;
    double range_factor;
    double spike, spike_per_range;
    int spike_box[2];
    int use_spike, use_spike_per_range;
    int outlier_box[2];
    double outlier_thresh;
    int use_outlier;

    // Images

    SimpleImage<double> disp_line;
    SimpleImage<double> disp_samp;
    SimpleImage<double> avg_line;
    SimpleImage<double> delta_line_disp;
    SimpleImage<double> range_image;
    SimpleImage<double> spike_x;
    SimpleImage<double> spike_y;
    SimpleImage<double> spike_z;

    zvmessage("MARSXYZ version 2020-05-26", "");

    // Get the input file list, and set up initial camera/pointing models
    // for each input.  Although we accept two and only two inputs, mars_setup
    // does lots of other nice things for us.

    mars_setup(nids, file_models, camera_in, pointing_in, NULL, cs,
                mission, instrument, homogeneous_inputs,
                MAX_NL, MAX_NS, MAX_INPUTS);

    snprintf(msg, MSG_LEN, "Generating XYZ point using the %s coordinate frame",
			cs->getFrameName());
    zvmessage(msg, "");

    if (nids != 2) {
	zvmessage("MARSXYZ requires 2 and only 2 image inputs", "");
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

    // Compute and print camera baseline.  Must convert to common CS
    // (to accomodate long-baseline stereo)

    PigPoint point0 = camera_in[0]->getCameraPosition();
    PigPoint point1 = camera_in[1]->getCameraPosition();
    PigCoordSystem *camera_cs0 = camera_in[0]->getCoordSystem();
    PigCoordSystem *camera_cs1 = camera_in[1]->getCoordSystem();

    PigPoint point1prime = camera_cs0->convertPoint(point1, camera_cs1);

    double baseline = (point0 - point1prime).magnitude();
    snprintf(msg, MSG_LEN, "Camera baseline is %lf", baseline);
    zvmessage(msg, "");

    // Open output files.
    // OUT can be 1 or 3 files, for a single, 3-banded file, or 3 single-band
    // files.  ERR_FILE is an error output file.

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
	labelModel->setXYZ(file_models, nfms, cs, "XYZ_MAP", baseline,
			file_models[nids]->getStereoProductId());
	if (zvptst("WRITE_CM"))
	    labelModel->writeCM(camera_in[0], camera_in[0]->getCoordSystem());
    }
    else if (count == 3) {
      char* image_type[3] = {"X_MAP", "Y_MAP", "Z_MAP"};
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
	    labelModel->setXYZ(file_models, nids, cs, image_type[i], baseline,
			file_models[nids]->getStereoProductId());
	    if (zvptst("WRITE_CM"))
	        labelModel->writeCM(camera_in[0],
					camera_in[0]->getCoordSystem());
	}
    }
    else {
	zvmessage("OUT must have 1 or 3 filenames", "");
	zabend();
    }

    error_unit = -1;
    zvpcnt("ERR_FILE", &count);
    if (count != 0) {
	zvp("ERR_FILE", filename, &count);
	zvunit(&error_unit, "ERR_FILE", 1, "u_name", filename, NULL);
	zvopen(error_unit, "op", "write",
		"u_ns", file_models[0]->getNS(), "u_nl",file_models[0]->getNL(),
		"u_nb", 1,
		"open_act", "sa", "u_org", "bsq",
		"u_format", "doub", "o_format", "real", NULL);
	zvplabel(error_unit, 0, 1);
    }

    // Allocate working images

    disp_line.alloc(nl, ns);
    disp_samp.alloc(nl, ns);
    delta_line_disp.alloc(nl, ns);
    range_image.alloc(nl, ns);

    // Allocate output images

    xi.alloc(nl, ns);
    yi.alloc(nl, ns);
    zi.alloc(nl, ns);
    error_img.alloc(nl, ns);

    // Get parameter overrides if any

    zvparmd("ERROR", &error_check, &count, &def, 1, 0);
    zvparmd("ABSERROR", &abs_error_check, &count, &def, 1, 0);
    zvparmd("LINEDISP", &linedisp_check, &count, &def, 1, 0);
    zvparmd("AVGLINEDISP", &avglinedisp_check, &count, &def, 1, 0);
    zvp("BOX_WIDTH", &box_width, &count);
    zvp("BOX_HEIGHT", &box_height, &count);
    if (box_width % 2 != 1 || box_height % 2 != 1) {
	zvmessage("BOX_WIDTH and BOX_HEIGHT must be odd.", "");
	zabend();
    }
    zvparmd("ZLIMITS", zlimits, &count, &def, 2, 0);
    zvparmd("SPIKE", &spike, &count, &def, 1, 0);
    use_spike = (count != 0);
    zvparmd("SPIKE_RANGE", &spike_per_range, &count, &def, 1, 0);
    use_spike_per_range = (count != 0);
    zvp("SPIKE_BOX", spike_box, &count);
    if (count == 1)
        spike_box[1] = spike_box[0];
    if (spike_box[0] % 2 != 1 || spike_box[1] % 2 != 1) {
	zvmessage("SPIKE_BOX width and height must be odd.", "");
	zabend();
    }

    zvparmd("OUTLIER", &outlier_thresh, &count, &def, 1, 0);
    use_outlier = (count != 0);
    zvp("OUTLIER_BOX", outlier_box, &count);
    if (count == 1)
	outlier_box[1] = outlier_box[0];
    if (outlier_box[0] % 2 != 1 || outlier_box[1] % 2 != 1) {
	zvmessage("OUTLIER_BOX width and height must be odd.", "");
	zabend();
    }

    zvparmd("RANGE_LIMIT", &range_limit, &count, &def, 1, 0);

    // Range limit not given, so compute from range factor * camera baseline

    if (count == 0) {
	zvparmd("RANGE_FACTOR", &range_factor, &count, &def, 1, 0);
	range_limit = range_factor * baseline;
    }
    snprintf(msg, MSG_LEN, "Limiting range to %lf", range_limit);
    zvmessage(msg, "");

    // Read the correlation data.  Save it, and calculate the delta line
    // disparity.  Check to make sure it's reasonable, and save it too.

    zvmessage("Reading disparity file(s)...", "");
    for (j=0; j < nl; j++) {		// line

	zvread(disp_unit_line, disp_line.linePtr(j),
		"LINE", j+1, "BAND", disp_band_line, NULL);
	zvread(disp_unit_samp, disp_samp.linePtr(j),
		"LINE", j+1, "BAND", disp_band_samp, NULL);

	// orig_xxx = original (usually left image)
	// disp_xxx = from disparity image (usually right image)

	for (i=0; i < ns; i++) {		// samp

	    if ((fabs((double)(disp_samp.get(j,i))) < .0001) &&
		(fabs((double)(disp_line.get(j,i))) < .0001)) {
		delta_line_disp.set(j,i, BAD_DELTA);	// no data
		continue;
	    }

	    // Convert points to camera coordinates

	    double orig_l = j + file_models[0]->getYOffset();

            // disparity images are 1-based, subtract 1 to convert
            // to 0-base for camera model
	    double disp_l = disp_line.get(j,i)-1 + file_models[1]->getYOffset();

	    // Save line disparity value

	    double delta = (double)disp_l - orig_l;
	    if (delta > linedisp_check || delta < -linedisp_check)
		delta = BAD_DELTA;

	    delta_line_disp.set(j,i, delta);
	}
    }

    // Now compute the box filter

    zvmessage("Computing box filter...", "");

    if (avglinedisp_check > 0.0) {
        compute_box_filter(delta_line_disp, avg_line, box_height, box_width,
						(double)BAD_DELTA);
    }
    delta_line_disp.free();

    // Now calculate the XYZ's...

    zvmessage("Calculating XYZ's...", "");

    int reject_nocorr = 0;
    int reject_linedisp = 0;
    int reject_avglinedisp = 0;
    int reject_nosolution = 0;
    int reject_error = 0;
    int reject_abs_error = 0;
    int reject_zlimit = 0;
    int reject_diverge = 0;
    int reject_range = 0;
    int reject_spike = 0;
    int reject_spike_per_range = 0;
    int reject_outlier = 0;
    int computed_points = 0;

    count = 0;

    zvmessage(
 "left_l  left_s right_l  right_s      x        y         z       error", " ");

    for (j=0; j < nl; j++) {

	for (i=0; i < ns; i++) {

	    xi.set(j,i,0.0);			// in case the pt is rejected
	    yi.set(j,i,0.0);
	    zi.set(j,i,0.0);
	    error_img.set(j,i,0.0);
	    range_image.set(j,i,0.0);

	    // Skip missing points

	    if ((fabs((double)(disp_samp.get(j,i))) < .0001) &&
		(fabs((double)(disp_line.get(j,i))) < .0001)) {
		reject_nocorr++;
		continue;
	    }

	    // Convert points to camera coordinates

	    double orig_l = j + file_models[0]->getYOffset();
	    double orig_s = i + file_models[0]->getXOffset();

            // disparity images are 1-based, subtract 1 to convert
            // to 0-base for camera model
	    double disp_l = disp_line.get(j,i)-1 + file_models[1]->getYOffset();
	    double disp_s = disp_samp.get(j,i)-1 + file_models[1]->getXOffset();

	    // Reject if the line disparity is too great

	    double delta = (double)disp_l - orig_l;

	    if (fabs(delta) > linedisp_check) {
		reject_linedisp++;
		continue;
	    }

	    // Reject if the line disparity is too far from the average

	    if (avglinedisp_check > 0 &&
		  fabs(delta - avg_line.get(j,i)) > avglinedisp_check) {
		reject_avglinedisp++;
		continue;
	    }

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

	    PigPoint xyz;
	    status = xvector(orig_origin, disp_origin, orig_vector, disp_vector,
			xyz, &error_value);
	    if (status != 0) {
		reject_nosolution++;
		continue;		// error
	    }

	    // Save away the error value (so the user can see what was rejected)
	    error_img.set(j,i,error_value);

	    // Reject if vector miss distance is too great in absolute terms
	    // (not adjusted for range).

	    if (error_value > abs_error_check) {
		reject_abs_error++;
		continue;
	    }

	    // Reject if vector miss distance (adjusted for range) is too
	    // great.  Note that range is from reference camera center,
	    // not the coord sys origin.

	    range = (xyz - orig_origin).magnitude();
	    if (error_value / range > error_check) {
		reject_error++;
		continue;
	    }
	    range_image.set(j,i,range);

	    // Limit z values
	    if ((xyz.getZ() < zlimits[0]) || (xyz.getZ() > zlimits[1])) {
		reject_zlimit++;
		continue;
	    }

	    // Reject rays that diverge
	    // In order to determine if they diverge, take the magnitude of
	    // the origin difference and compare to the magnitude of the
	    // difference at the end of the vector.  If the end is greater,
	    // they diverge.  To handle the case of origins closer together
	    // than 1.0 unit, the unit vectors are scaled to half the origin
	    // distance.

	    PigVector diff_origin = orig_origin - disp_origin;
	    double origin_mag = diff_origin.magnitude();
	    double scale = origin_mag / 2.0;
	    PigVector diff_end = (orig_origin + orig_vector*scale) -
				 (disp_origin + disp_vector*scale);
	    if (origin_mag < diff_end.magnitude()) {
		reject_diverge++;
		continue;
	    }

	    if (range > range_limit) {
		reject_range++;
		continue;
	    }

	    // Print out some values
	    if (count++ < 10) {
		snprintf(msg, MSG_LEN, "%6.2f %6.2f %6.2f %6.2f %f %f %f %f",
			orig_l, orig_s, disp_l, disp_s,
			xyz.getX(), xyz.getY(), xyz.getZ(), error_value);
		zvmessage(msg," ");
	    }

	    // Save results away  (error_img saved above)

	    xi.set(j,i, xyz.getX());
	    yi.set(j,i, xyz.getY());
	    zi.set(j,i, xyz.getZ());
	    computed_points++;

	}

    }
    avg_line.free();
    disp_line.free();
    disp_samp.free();

    // Despiking algorithm

    if (use_spike || use_spike_per_range) {

	zvmessage("Despiking...", "");

	// Technically, points should be excluded only if all 3 values are 0.
	// However, practially speaking, it doesn't much matter to exclude
	// all 0 coordinates.  It almost never will be exactly 0.0 in any
	// real-world situation.

        compute_box_filter(xi, spike_x, spike_box[0], spike_box[1], 0.0);
        compute_box_filter(yi, spike_y, spike_box[0], spike_box[1], 0.0);
        compute_box_filter(zi, spike_z, spike_box[0], spike_box[1], 0.0);

	double spike_sq = spike * spike;	// avoid sqrt
	double spike_per_range_sq = spike_per_range * spike_per_range;

	for (j=0; j < nl; j++) {
	    for (i=0; i < ns; i++) {
	
		double x = xi.get(j,i);
		double y = yi.get(j,i);
		double z = zi.get(j,i);
		if (x == 0.0 && y == 0.0 && z == 0.0)
		    continue;			// no point here

		// Compute the distance from this point to the average
		// of the surrounding points.

		PigVector v(x - spike_x.get(j,i),
			    y - spike_y.get(j,i),
			    z - spike_z.get(j,i));
		double dist = v.magnitude_sq();

		if (use_spike) {
		    if (dist > spike_sq) {		// filter it
		        xi.set(j,i, 0.0);
		        yi.set(j,i, 0.0);
		        zi.set(j,i, 0.0);
		        reject_spike++;
		        computed_points--;
			continue;		// don't do both filters!
		    }
		}
		if (use_spike_per_range) {
		    double range = range_image.get(j,i);
		    if (range == 0)
			continue;			// shouldn't happen...
		    double range_sq = range * range;
		    if (dist > spike_per_range_sq * range_sq) {
		        xi.set(j,i, 0.0);
		        yi.set(j,i, 0.0);
		        zi.set(j,i, 0.0);
		        reject_spike_per_range++;
		        computed_points--;
		    }
		}

	    }
	}
	spike_x.free();
	spike_y.free();
	spike_z.free();
    }
    range_image.free();

    // Outlier filter.  Remove points that don't have enough neighbors
    // (and are thus somewhat isolated).  A blob filter would probably be
    // better, but that's coming in a future correlator.

    // Note that unlike the other users of the box filter, in this case
    // we want the box to go off the edge of the image (i.e. border of 0).
    // That makes sure to preserve the statistcs so that a single hard edge
    // really is at 50%.  In order to do this, we simply place a border around
    // the input and output images to the box filter.  The pinning that occurs
    // on the edges is thus ignored.

    if (use_outlier) {

	int obh2 = outlier_box[0];
	int obw2 = outlier_box[1];

	zvmessage("Removing outliers...", "");
	SimpleImage<double> bin_img, outlier_img;
	bin_img.alloc(nl+obh2*2, ns+obw2*2);
	bin_img.zero();

	// Fill up binary image (1 or 0 only) based on existence of pixel

	for (j=0; j < nl; j++) {
	    for (i=0; i < ns; i++) {

		if (xi.get(j,i) != 0 || yi.get(j,i) != 0 || zi.get(j,i) != 0)
		    bin_img.set(j+obh2, i+obw2, 1.0);
		else
		    bin_img.set(j+obh2, i+obw2, 0.0);
	    }
	}

	// Compute a box filter on the binary image (ignoring "ignore")

	compute_box_filter(bin_img, outlier_img,
			outlier_box[0], outlier_box[1], -9e9);

	// If there are not enough neighbors that exist, the average in
	// outlier_img will be less than the threshold.  Eliminate those.

	for (j=0; j < nl; j++) {
	    for (i=0; i < ns; i++) {
		if (bin_img.get(j+obh2, i+obw2) == 0)
		    continue;		// nothing here
		if (outlier_img.get(j+obh2, i+obw2) < outlier_thresh) {
		    xi.set(j,i, 0.0);
		    yi.set(j,i, 0.0);
		    zi.set(j,i, 0.0);
		    reject_outlier++;
		    computed_points--;
		}
	    }
	}
	bin_img.free();
	outlier_img.free();
    }


    // Write out the xyz image

    for (j=0; j < nl; j++) {
	zvwrit(out_unit[0], xi.linePtr(j), "LINE",j+1, "BAND",out_band[0],NULL);
	zvwrit(out_unit[1], yi.linePtr(j), "LINE",j+1, "BAND",out_band[1],NULL);
	zvwrit(out_unit[2], zi.linePtr(j), "LINE",j+1, "BAND",out_band[2],NULL);
    }

    if (error_unit >= 0) {
	for (j=0; j < nl; j++) {
	    zvwrit(error_unit, error_img.linePtr(j), "LINE", j+1, NULL);
	}
    }

    zvclose(out_unit[0], NULL);
    if (out_unit[1] != out_unit[0])
	zvclose(out_unit[1], NULL);
    if (out_unit[2] != out_unit[0])
	zvclose(out_unit[2], NULL);
    if (error_unit >= 0)
	zvclose(error_unit, NULL);

    snprintf(msg, MSG_LEN, "Rejected %d points for missing correlation", reject_nocorr);
    zvmessage(msg, "");
    snprintf(msg, MSG_LEN, "Rejected %d points for excessive line disparity",
							reject_linedisp);
    zvmessage(msg, "");
    snprintf(msg, MSG_LEN, "Rejected %d points for exeeding average line disparity",
							reject_avglinedisp);
    zvmessage(msg, "");
    snprintf(msg, MSG_LEN, "Rejected %d points as not computable", reject_nosolution);
    zvmessage(msg, "");
    snprintf(msg, MSG_LEN, "Rejected %d points for excessive absolute miss distance (abserror)",
							reject_abs_error);
    zvmessage(msg, "");
    snprintf(msg, MSG_LEN, "Rejected %d points for excessive miss distance per range (error)",
							reject_error);
    zvmessage(msg, "");
    snprintf(msg, MSG_LEN, "Rejected %d points for exceeding Z limits", reject_zlimit);
    zvmessage(msg, "");
    snprintf(msg, MSG_LEN, "Rejected %d points due to diverging rays", reject_diverge);
    zvmessage(msg, "");
    snprintf(msg, MSG_LEN, "Rejected %d points for exceeding range limit", reject_range);
    zvmessage(msg, "");
    snprintf(msg, MSG_LEN, "Rejected %d points for exceeding spike value", reject_spike);
    zvmessage(msg, "");
    snprintf(msg, MSG_LEN, "Rejected %d points for exceeding spike value per range", reject_spike_per_range);
    zvmessage(msg, "");
    snprintf(msg, MSG_LEN, "Rejected %d points for being outliers", reject_outlier);
    zvmessage(msg, "");
    snprintf(msg, MSG_LEN, "Computed %d valid XYZ points", computed_points);
    zvmessage(msg, "");
}

/*********************************************************************
c Convert from image coordinates to xyz coordinates given two
c images forming a stereo pair.
c cam1=x,y,z object space position of camera 1 (PigPoint)
c cam2=x,y,z object space position of camera 2 (PigPoint)
c uvw1=direction cosines for left pixel (PigVector)
c uvw2=direction cosines for right pixel (PigVector)
c xyz= xyz object space coord of object (returned PigPoint)
c return value: 0=OK, 1=no solution
***********************************************************************/

static int xvector(const PigPoint cam1, const PigPoint cam2,
		   const PigVector uvw1, const PigVector uvw2,
		   PigPoint &xyz, double *error)
{
    double a[10],b[4],c[10];
    int i, status;

/* compute direction cosines u,v,w for ray1 and ray2 */
    
// They're already unit vectors...
//    uvw1.normalize();
//    uvw2.normalize();
 
/* solve for x,y,z point on ray1 nearest to ray2 */
    PigVector s = uvw1 * uvw2;
    PigVector s1 = s * uvw1;
    PigVector s2 = s * uvw2;

    a[1]=s.getX();
    a[2]=s1.getX();
    a[3]=s2.getX();
    a[4]=s.getY();
    a[5]=s1.getY();
    a[6]=s2.getY();
    a[7]=s.getZ();
    a[8]=s1.getZ();
    a[9]=s2.getZ();
    for (i=1; i < 10; i++)
	c[i]=a[i];
    b[1] = s % cam1;
    b[2] = s1 % cam1;
    b[3] = s2 % cam2;

    dsimq(a, b, 3, &status);
    if (status > 0)
	return status;
    PigPoint xyz1(b[1], b[2], b[3]);
 
/* solve for xx,yy,zz point on ray2 nearest to ray1 */
    b[1] = s % cam2;
    b[2] = s1 % cam1;
    b[3] = s2 % cam2;

    dsimq(c, b, 3, &status);
    if (status > 0)
	return status;
    PigPoint xyz2(b[1], b[2], b[3]);


#if 0
///////////////////////////////////////////////////////////////////
//
// use lcross routine for comparison, added by jnm 15 April 1999

    double b1[3],b2[3],v1[3],v2[3],p1[3],p2[3];
    double k1,k2;
    int colinear;
    k1 =0;
    k2 =0;
    colinear =0;

// pull the points out into simple arrays
   cam1.getXYZ(b1);  // base point #1
   cam2.getXYZ(b2);  // base point #2
   uvw1.getXYZ(v1);  // pointing vector #1
   uvw2.getXYZ(v2);  // pointing vector #2
 
// cout << "Base point 1 is: " << b1[0] << "," <<b1[1] << "," << b1[2] << endl;
// cout << "Base point 2 is: " << b2[0] << "," <<b2[1] << "," << b2[2] << endl;
// cout << "Vector 1 is: " << v1[0] << "," <<v1[1] << "," << v1[2] << endl;
// cout << "Vector 2 is: " << v2[0] << "," <<v2[1] << "," << v2[2] << endl;

   lcross(b1, v1, b2, v2, &k1, p1, &k2, p2, &colinear);

// now put the points p1 and p2 into the xyz1 and xyz1 Point()s.
  
   PigPoint xyz1(p1[0], p1[1], p1[2]);
   PigPoint xyz2(p2[0], p2[1], p2[2]);

// now back to our regularly scheduled program.
//
///////////////////////////////////////////////////////////////////
#endif

/* point inbetween is the closest approach point to both vectors */
    *error = (xyz1-xyz2).magnitude();
 
xyz = (xyz1 + xyz2) / 2.0;

   return 0;
}

/**************************************************************************
C        USAGE
C           CALL DSIMQ(A,B,N,KS)
C        DESCRIPTION OF PARAMETERS
C           A - MATRIX OF COEFFICIENTS STORED COLUMNWISE.  THESE ARE
C               DESTROYED IN THE COMPUTATION.  THE SIZE OF MATRIX A IS
C               N BY N.
C           B - VECTOR OF ORIGINAL CONSTANTS (LENGTH N). THESE ARE
C               REPLACED BY FINAL SOLUTION VALUES, VECTOR X.
C           N - NUMBER OF EQUATIONS AND VARIABLES. N MUST BE .GT. ONE.
C           KS - OUTPUT DIGIT
C                0 FOR A NORMAL SOLUTION
C                1 FOR A SINGULAR SET OF EQUATIONS
*************************************************************************/

static void dsimq(double A[10], double B[10], int N, int *KS)
{
      double BIGA,SAVE,TOL;   
      int JJ,I,IT,J = 0,IJ,I1,I2,IMAX = 0,IQS,JY,IXJ,IX,JX,IXJX,JJX;
      int K,NY,IA,IB,IC;
      TOL=0.0;
      *KS=0;
      JJ=-N;
      for(J=1; J < N+1; J++)                 /*  DO 65 J=1,N */
      {
        JY=J+1;
        JJ=JJ+N+1;
        BIGA=0.0;
        IT=JJ-J;
        for(I=J; I < N+1; I++)                /* DO 30 I=J,N */
        {
          IJ=IT+I;
                                      /* IF(dabs(BIGA)-dabs(A(IJ))) 20,30,30 */
          if(fabs(BIGA)-fabs(A[IJ]) < 0.0){
            BIGA=A[IJ];               /* 20 */
            IMAX=I;
          }
        }                            /* 30 CONTINUE */
        if(fabs(BIGA)-TOL <= 0.0){     /* IF(dabs(BIGA)-TOL) 35,35,40 */
          *KS=1;                          /* 35 */
          return;
        }
        I1=J+N*(J-2);                   /* 40 */
        IT=IMAX-J;
        for(K=J; K < N+1; K++){         /* DO 50 K=J,N */
          I1=I1+N;
          I2=I1+IT;
          SAVE=A[I1];
          A[I1]=A[I2];
          A[I2]=SAVE;
          A[I1]=A[I1]/BIGA;                /* 50 */
        }
        SAVE=B[IMAX];
        B[IMAX]=B[J];
        B[J]=SAVE/BIGA;
        if(J-N == 0) goto seventy;           /* IF(J-N) 55,70,55 */
        IQS=N*(J-1);                      /* 55 */
        for(IX=JY; IX < N+1; IX++){     /* DO 65 IX=JY,N */
          IXJ=IQS+IX;
          IT=J-IX;
          for(JX=JY; JX < N+1; JX++){   /* DO 60 JX=JY,N */
            IXJX=N*(JX-1)+IX;
            JJX=IXJX+IT;
            A[IXJX]=A[IXJX]-(A[IXJ]*A[JJX]); /* 60 */
          }
          B[IX]=B[IX]-(B[J]*A[IXJ]);          /* end of 2nd 65 */
        }
     }                                        /* end of 1st 65 */
  seventy: NY=N-1;
      IT=N*N;
      for(J=1; J < NY+1; J++){              /* DO 80 J=1,NY */
        IA=IT-J;
        IB=N-J;
        IC=N;
        for(K=1; K < J+1; K++){             /* DO 80 K=1,J */
          B[IB]=B[IB]-A[IA]*B[IC];
          IA=IA-N;
          IC=IC-1;                             /* 80 */
        }
      }
} 


/******************************************************************************
********************************   LCROSS   ***********************************
*******************************************************************************

// code by Todd Litwin, added 15 April 1999 by Justin Maki

    This function computes the nearest points, p1 and p2, on lines 1 and
    2, where each line is represented by a base point (b1 or b2) and a
    vector (v1 or v2). If input lines are colinear, then no computation
    will be made. The lines are expected to come close to intersecting
    each other. The equations of the lines are:

        L1 = B1 + k1 V1
        L2 = B2 + k2 V2

    where k1 and k2 are the parameters of the lines. */

#define EPSILON 1.0e-20

void  lcross( double b1[3], /* input base point for line 1 */
	      double v1[3], /* input pointing vector for line 1 */
	      double b2[3], /* input base point for line 2 */
	      double v2[3], /* input pointing vector for line 2 */
	      double *k1,   /* output parameter value for crossover on line 1 */
	      double p1[3], /* output crossover point on line 1 */
	      double *k2,   /* output parameter value for crossover on line 2 */
	      double p2[3], /* output crossover point on line 2 */
	      int *colinear) { /* output flag if lines are colinear */
	      
    double b[3], v1b, v2b, v1v1, v2v2, v1v2, denom;
    static double zero[3] = {0, 0, 0};

    /* Compute the intermediate terms */
    sub3(b1, b2, b);
    v1b  = dot3(v1, b);
    v2b  = dot3(v2, b);
    v1v1 = dot3(v1, v1);
    v2v2 = dot3(v2, v2);
    v1v2 = dot3(v1, v2);

    /* Test for colinearity */
    denom = v1v2 * v1v2 - v1v1 * v2v2;
    if ((denom < EPSILON) && (denom > -EPSILON)) {
        *colinear = TRUE;
        *k1 = 0;
        *k2 = 0;
        copy3(zero, p1);
        copy3(zero, p2);
        return;
        }
    else
        *colinear = FALSE;

    /* Compute the parameter values */
    *k2 = (v1b * v1v2 - v2b * v1v1) / denom;
    *k1 = (*k2 * v1v2 - v1b) / v1v1;

    /* Compute the crossover points */
    scale3(*k1, v1, p1);
    scale3(*k2, v2, p2);
    add3(b1, p1, p1);
    add3(b2, p2, p2);
    }



////////////////////////////////////////////////////////////////////////
// Compute a box filter (simple average of kernel) using sliding sums.
// The output image is allocated here.  "ignore" is a value to ignore
// in the computation of the average.
////////////////////////////////////////////////////////////////////////

void compute_box_filter(SimpleImage<double> &input,
			SimpleImage<double> &output,
			int box_height, int box_width, double ignore)
{
    int i, j;

    int bh2 = box_height / 2;
    int bw2 = box_width / 2;

    int nl = input.getNL();
    int ns = input.getNS();

    output.alloc(nl, ns);

    // Compute initial box in corner

    int init_box_size = 0;
    double init_box_sum = 0.0;

    for (j=0; j < MIN(box_height, nl); j++) {
	for (i=0; i < MIN(box_width, ns); i++) {
	    if (input.get(j,i) != ignore) {
		init_box_sum += input.get(j,i);
		init_box_size++;
	    }
	}
    }

    for (j=0; j < nl; j++) {			// line

	// First and last half-kernels don't move; they're pinned
	// against the edge

	if (j > bh2 && j <= (nl - bh2 - 1)) {

	    // Subtract the old first row and add new last row

	    for (int ii = 0; ii < MIN(box_width, ns); ii++) {
		if (input.get(j-bh2-1,ii) != ignore) {
		    init_box_sum -= input.get(j-bh2-1,ii);
		    init_box_size--;
		}
		if (input.get(j+bh2,ii) != ignore) {
		    init_box_sum += input.get(j+bh2,ii);
		    init_box_size++;
		}
	    }
	}

	// Sample loop.  Start with the initial box and slide it over.

	double box_sum = init_box_sum;
	int box_size = init_box_size;

	for (i=0; i < ns; i++) { 		// samp

	    // First and last half-kernels don't move; they're pinned
	    // against the edge

	    if (i > bw2 && i <= (ns - bw2 - 1)) {

		// Subtract the old first column and add new last column

		int start_j = MAX(j-bh2, 0);
		int end_j = start_j + box_height;
		for (int jj = start_j; jj < MIN(end_j, nl); jj++) {
		    if (input.get(jj,i-bw2-1) != ignore) {
			box_sum -= input.get(jj,i-bw2-1);
			box_size--;
		    }
		    if (input.get(jj,i+bw2) != ignore) {
			box_sum += input.get(jj,i+bw2);
			box_size++;
		    }
		}
	    }

	    // Compute the average and save it

	    if (box_size == 0)
		output.set(j,i, 0.0);
	    else
		output.set(j,i, box_sum / box_size);

	}		// end samp loop
    }		// end line loop

}			// end box filter computation

