/* marsrfilt */
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
#include "PigCSReference.h"
#include "mat3.h"

#include "return_status.h"

#include <math.h>
#include <iostream>
using namespace std;


/* buffer sizes in main program */
#define MAX_INPUTS 3
#define MAX_NL MARS_MAX_NL
#define MAX_NS MARS_MAX_NS

#ifndef MIN
#define MIN(a,b) (((a)<(b))?(a):(b))
#endif

static void lowpass(SimpleImage<float> *img_in,
                    SimpleImage<float> *img_out, int window);
static void range_filt(SimpleImage<float> *img_in, SimpleImage<float> *img_out,
	SimpleImage<float> *avgrng,
	double corr_acc, double baseline, double aspect_ratio,
	double num_sigma, double wfactor, double &average_window,
	double &min_window_out, double &max_window_out,
	double ifov, double prox_min, double prox_max, int min_window);

////////////////////////////////////////////////////////////////////////

void main44()
{
    int i, j;
    int status, count, def;
	const size_t msgLen = 255;
    char msg[msgLen];

    int nids;
    char mission[64], instrument[64];
    int nl, ns, nb;
    int band[3];

    // Inputs

    PigFileModel *file_models[MAX_INPUTS];
    PigCameraModel *camera_in[MAX_INPUTS];
    PigPointingModel *pointing_in[MAX_INPUTS];
    int homogeneous_inputs = TRUE;
    PigCoordSystem *cs, *xyz_cs;
    int xyz_unit[3];
    float point[3];

    // Outputs
    int unit_out;
    int nlo, nso;

    // User Parameters
    PigPoint rangeOrigin;


    zvmessage("MARSRFILT version 1", "");

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

    snprintf(msg, msgLen, "Interpreting XYZ values using the %s coordinate frame: %s",
                xyz_cs->getFrameName(), ref->getFullName());
    zvmessage(msg, "");

    // Get the coordinate system to use.
    snprintf(msg, msgLen, "Generating RANGE using the %s coordinate frame.",
            cs->getFrameName());
    zvmessage(msg, "");

    // Get the range origin point to use
    zvpcnt("origin", &count);
    if (count == 3) {
        zvp("origin", point, &count);
        rangeOrigin.setXYZ(point);
    }
    else {
        if (pointing_in[0] == NULL) {
            zvmessage("No camera model to extract origin; use ORIGIN parameter", "");
            zabend();
        }
        //Use camera position as a default range origin value
        rangeOrigin = pointing_in[0]->getCameraPosition(cs);
    }

    snprintf(msg, msgLen, "Using POINT (%f, %f, %f) as range origin",
            rangeOrigin.getX(), rangeOrigin.getY(),rangeOrigin.getZ());
    zvmessage(msg, "");

    // Open input file(s).  An XYZ data could be either 1 3-band file
    // or 3 single band files
    if (nids == 1) {

        //make sure that file is not open
        if(file_models[0]->isFileOpen())
          file_models[0]->closeFile();

        //open the file
        file_models[0]->openFile();
        file_models[0]->setFileOpen(TRUE);

        //get Unit id
        xyz_unit[0] = file_models[0]->getUnit();

        //check for proper number of bands
        zvget(xyz_unit[0], "nl", &nl, "ns", &ns, "nb", &nb, NULL);
        if (nb != 3) {
          zvmessage("A single XYZ file must have three bands", "");
          zabend();
        }

        //Initialize xyz_unit array
        xyz_unit[2] = xyz_unit[1] = xyz_unit[0];

        //Initialize band array
        band[0] = 1;
        band[1] = 2;
        band[2] = 3;
    }
    else if(nids == 3) {
      int cnt;
      for(cnt = 0; cnt <3; cnt++) {

        //make sure that file is not open
        if(file_models[cnt]->isFileOpen())
          file_models[cnt]->closeFile();

        //open the file
        file_models[cnt]->openFile();
        file_models[cnt]->setFileOpen(TRUE);

        //get Unit id
        xyz_unit[cnt] = file_models[cnt]->getUnit();

        //check for proper number of bands
        zvget(xyz_unit[cnt], "nl", &nl, "ns", &ns, "nb", &nb, NULL);
        if (nb != 1) {
            zvmessage("A three-file XYZ must have one band each (#1)", "");
            zabend();
        }

        //check that all files are the same size
        if (nl != file_models[0]->getNL() || ns != file_models[0]->getNS()) {
            zvmessage("Input is of different size than Input #1", "");
            zabend();
        }
        band[cnt] = 1;
      }

    }
    else {
        zvmessage("MARSRFILT requires either 1 3-band file or 3 single band files", "");
        zabend();
    }

    // Open output files.
    // OUT should be 1 file
    zvpcnt("OUT", &count);
    if (count != 1) {
        zvmessage("OUT must have 1 filename", "");
        zabend();
    }


    count = 0;

    // get input image dimensions
    nlo = file_models[0]->getNL();
    nso = file_models[0]->getNS();
    snprintf(msg, msgLen, "Output # lines & samples=%10d %10d", nlo, nso);
    zvmessage(msg, "");

    zvselpiu(file_models[0]->getUnit());          // transfer labels
    status=zvunit(&unit_out, "OUT", 1, NULL);
    status=zvopen(unit_out, "OP", "WRITE", "U_FORMAT", "REAL",
               "U_NS", nso, "U_NL", nlo, "OPEN_ACT", "AS",
               "U_NB", 3, "U_ORG", "BSQ", "O_FORMAT", "REAL", NULL);

    // Write the parameter output labels

    zvplabel(unit_out, 0, 1);

    // Read the xyz data and compute range, converting CS's if needed as we go

    SimpleImage<float> *xyz = new SimpleImage<float>(3, nlo, nso);
    SimpleImage<float> *xyz_out = new SimpleImage<float>(3, nlo, nso);
    SimpleImage<float> *rng = new SimpleImage<float>(1, nlo, nso);
    SimpleImage<float> *rngf = new SimpleImage<float>(1, nlo, nso);
    SimpleImage<float> *avgrng = new SimpleImage<float>(1, nlo, nso);

    SimpleImage<float> *cast = NULL;
    if (zvptst("RAY")) {
	cast = new SimpleImage<float>(3, nlo, nso);
	cast->zero();
	zvmessage("Using original ray for reconstruction", "");
    }
    else {
	zvmessage("Using camera model for reconstruction", "");
    }

    for (j=0; j < nlo; j++) {               // line

        //read the line from the three bands of the single input file
        //or read the line from the three single band files
        zvread(xyz_unit[0], xyz->linePtr(0,j), "LINE", j+1,
							"BAND", band[0], NULL);
        zvread(xyz_unit[1], xyz->linePtr(1,j), "LINE", j+1,
							"BAND", band[1], NULL);
        zvread(xyz_unit[2], xyz->linePtr(2,j), "LINE", j+1,
							"BAND", band[2], NULL);

	// Convert coord sys's if needed and compute range

        for (i=0; i < nso; i++) {           // samp

            // While you generally shouldn't compare floats for equality,
            // 0.0 is being used as a specific flag value and is
	    // representable exactly...

            PigPoint xyz_pt(xyz->get(0,j,i), xyz->get(1,j,i), xyz->get(2,j,i));

	    double range = 0.0;

            if (xyz_pt.getX() != 0.0 && xyz_pt.getY() != 0.0 &&
				        xyz_pt.getZ() != 0.0) {
                if (cs != xyz_cs) {
                    xyz_pt = cs->convertPoint(xyz_pt, xyz_cs);
                    xyz->set(0, j, i, xyz_pt.getX());
                    xyz->set(1, j, i, xyz_pt.getY());
                    xyz->set(2, j, i, xyz_pt.getZ());
                }
		double diff_x = xyz_pt.getX() - rangeOrigin.getX();
		double diff_y = xyz_pt.getY() - rangeOrigin.getY();
		double diff_z = xyz_pt.getZ() - rangeOrigin.getZ();
		range = sqrt(diff_x*diff_x + diff_y*diff_y + diff_z*diff_z);

		if (cast != NULL) {		// Save the direction vector
		    PigPoint cast_vec(diff_x, diff_y, diff_z);
		    cast_vec.normalize();
		    cast->set(0, j, i, cast_vec.getX());
		    cast->set(1, j, i, cast_vec.getY());
		    cast->set(2, j, i, cast_vec.getZ());
		}
	    }
	    rng->set(0, j, i, range);
	}
    }

    int window;
    zvp("WINDOW", &window, &count);

    // Get the average range using a simple lowpass.  We don't care about
    // edge effects, just have to skip 0's.  Approximate is okay because
    // this is just used to size the actual window.

    lowpass(rng, avgrng, window);

    double corr_acc;
    zvparmd("CORR", &corr_acc, &count, &def, 1, 0);

    double baseline;
    zvparmd("BASELINE", &baseline, &count, &def, 1, 0);
    if (count == 0) {		// get from label
	baseline = file_models[0]->getStereoBaseline(0.0);
	if (baseline == 0.0) {
	    zvmessage("Stereo baseline not found in label, must be provided in parameter", "");
	    zabend();
	}
	snprintf(msg, msgLen, "Stereo baseline from label: %f\n", baseline);
	zvmessage(msg, "");
    } else {
	snprintf(msg, msgLen, "Stereo baseline overridden to %f\n", baseline);
	zvmessage(msg, "");
    }

    double aspect_ratio;
    zvparmd("ASPECT_RATIO", &aspect_ratio, &count, &def, 1, 0);

    double num_sigma;
    zvparmd("NUM_SIGMA", &num_sigma, &count, &def, 1, 0);

    double wfactor;
    zvparmd("WFACTOR", &wfactor, &count, &def, 1, 0);

    int min_window;
    zvp("MIN_WINDOW", &min_window, &count);

    // Get the pixel IFOV

    double ifov = 0.0;
    zvparmd("IFOV", &ifov, &count, &def, 1, 0);
    if (count != 0) {
	snprintf(msg, msgLen, "IFOV overridden to %f", ifov);
	zvmessage(msg, "");
     } else {
        if (camera_in[0] != NULL)
	    ifov = camera_in[0]->getPixelAngle(1);
	if (ifov != 0) {
	    snprintf(msg, msgLen, "IFOV set via camera model to %f", ifov);
	    zvmessage(msg, "");
	}
    }
    if (ifov == 0) {
	zvmessage("IFOV is zero, so proximity filter is disabled", "");
    }

    // Get the proximity filter range

    double prox_min = 0;
    double prox_max = 0;

    zvparmd("PROX_MIN", &prox_min, &count, &def, 1, 0);
    zvparmd("PROX_MAX", &prox_max, &count, &def, 1, 0);

    double average_window = 0.0;
    double min_window_out = 0.0;
    double max_window_out = 0.0;
    range_filt(rng, rngf, avgrng, corr_acc, baseline, aspect_ratio,
		num_sigma, wfactor, average_window,
		min_window_out, max_window_out,
		ifov, prox_min, prox_max, min_window);

    // Now do optional spike filtering on the result

    if (zvptst("SPIKE")) {

	int spike_window;
	zvp("SPIKE_WINDOW", &spike_window, &count);

	double spike_factor;
	zvparmd("SPIKE_FACTOR", &spike_factor, &count, &def, 1, 0);

	// Compute the spike per range squared:

	double spike_per_rangesq = spike_factor * ifov * corr_acc / baseline;

	int num_spike = 0;

	lowpass(rngf, avgrng, spike_window);

	for (j=0; j < nlo; j++) {
	    for (i=0; i < nso; i++) {
		double range = rngf->get(0, j, i);

		if (range == 0.0)
		    continue;

		double diff = fabs(range - avgrng->get(0, j, i));
		if (diff > spike_per_rangesq * range * range) {
		    rngf->set(0, j, i, 0.0);
		    num_spike++;
		}
	    }
	}

	snprintf(msg, msgLen, "%d spikes removed", num_spike);
	zvmessage(msg ,"");
    }

    // Convert back to XYZ

    for (j=0; j < nlo; j++) {
	for (i=0; i < nso; i++) {
	    xyz_out->set(0, j, i, 0.0);
	    xyz_out->set(1, j, i, 0.0);
	    xyz_out->set(2, j, i, 0.0);

	    double range = rngf->get(0, j, i);

	    if (range == 0.0)
		continue;		// invalid point

	    double img_l = j + file_models[0]->getYOffset();
	    double img_s = i + file_models[0]->getXOffset();

	    // Get the view vector, either from the cmod or the saved dir

	    PigVector img_vector;
	    if (cast != NULL) {		// Use the saved direction
		img_vector.setXYZ(cast->get(0,j,i),
				  cast->get(1,j,i),
				  cast->get(2,j,i));
	    } else {
                // Compute the XYZ point.  We get the view ray direction and
                // then go a distance "range" along it.  Although the C point
	        // changes for CAHVORE, we use the same constant origin that
	        // we used to make the range.

                PigPoint img_origin;
                camera_in[0]->LStoLookVector(img_l, img_s,
                            img_origin, img_vector, cs);

                img_vector.normalize();         // make sure it's a unit vector
	    }

	    PigVector new_vector = img_vector * range + rangeOrigin;

	    xyz_out->set(0, j, i, new_vector.getX());
	    xyz_out->set(1, j, i, new_vector.getY());
	    xyz_out->set(2, j, i, new_vector.getZ());
	}
    }

    // Write the output labels

    PigLabelModel *labelModel = m->createLabelModel(unit_out);
    labelModel->setRFilt(file_models, nids, cs,
			average_window, min_window_out, max_window_out);
    // Now write the output

    for (int b=0; b < 3; b++) {
        for (int j=0; j < nlo; j++) {
	    zvwrit(unit_out, xyz_out->linePtr(b,j), "BAND",b+1,"LINE",j+1,NULL);
	}
    }

    zvclose(unit_out, NULL);
}

////////////////////////////////////////////////////////////////////////
// Low-pass filters the given image, copying it to a new location as we go.
// Simple box filter; pixel is replaced with average of nxn surrounding pixels,
// ignoring zero pixels.
// window must be odd.
////////////////////////////////////////////////////////////////////////

static void lowpass(SimpleImage<float> *img_in,
                    SimpleImage<float> *img_out, int window)
{
    int i, j, k;
    int nl = img_in->getNL();
    int ns = img_in->getNS();

    img_out->zero();

    SimpleImage<float> *sum_img = new SimpleImage<float>(nl, ns);
    SimpleImage<int> *count_img = new SimpleImage<int>(nl, ns);
    count_img->zero();

    int nw = window/2;                  // distance either side of center

    // Create image containing horizontal sums for each pixel.  It's okay
    // if we go off the edge for this case, just truncate the box.

    for (j=0; j < nl; j++) {
        for (i=0; i < ns; i++) {
	    int start = -nw;
	    if (i+start < 0) start = -i;
	    int end = nw;
	    if (i+nw >= ns) end = ns - i - 1;
            for (k=start; k <= end; k++) {
		float dn = img_in->get(j,i+k);
		if (dn != 0.0) {
		    sum_img->addto(j, i, dn);
		    count_img->addto(j, i, 1);
                }
            }
        }
    }

    for (j=0; j < nl; j++) {
        for (i=0; i < ns; i++) {
	    int start = -nw;
	    if (j+start < 0) start = -j;
	    int end = nw;
	    if (j+nw >= nl) end = nl - j - 1;

	    double sum = 0.0;
	    int count = 0;
	    for (k = start; k <= end; k++) {
		sum += sum_img->get(j+k, i);
		count += count_img->get(j+k, i);
	    }
	    if (count == 0)
		img_out->set(j, i, 0.0);
	    else
		img_out->set(j, i, sum / count);
	}
    }
    delete sum_img;
    delete count_img;

}

////////////////////////////////////////////////////////////////////////
// Low-pass filters the given image using a Gaussian kernel.  Divides the
// kernel into octants to reduce bias along the edges.
////////////////////////////////////////////////////////////////////////

static void range_filt(SimpleImage<float> *img_in, SimpleImage<float> *img_out,
	SimpleImage<float> *avgrng,
	double corr_acc, double baseline, double aspect_ratio,
	double num_sigma, double wfactor, double &average_window,
	double &min_window_out, double &max_window_out,
	double ifov, double prox_min, double prox_max, int min_window)
{
    int nl = img_in->getNL();
    int ns = img_in->getNS();

    int min_half_win = (min_window-1)/2;
    if (min_window == 0) min_half_win = 0;

    int omp_on = zvptst("OMP_ON");

    img_out->zero();

    double window_total = 0.0;
    int window_count = 0;
    min_window_out = 0.0;
    max_window_out = 0.0;

#pragma omp parallel for schedule(dynamic) if (omp_on)
    for (int j=0; j < nl; j++) {
        for (int i=0; i < ns; i++) {
	    double central_rng = img_in->get(j,i);
	    if (central_rng == 0.0) {
		continue;			// img_out already zeroed
	    }

// int DEBUG = (i==200);		//!!!!

	    // First, figure out the window size.
	    // The basic equation is w = r*c/b where r is range, c is
	    // correlation accuracy (typically 0.25-0.5), and b is the stereo
	    // camera baseline.  This is derived from the ratio of range
	    // error (r^2*c*i)/b where i is ifov, and crossrange pixel size,
	    // which is r*i.  This ratio (w) says how big the error is
	    // compared to the size of a pixel... in other words, the error
	    // size in pixels.  We use this as the nominal *diameter* of the
	    // window (thus, cut in half to get the nominal window radius).
	    // We multiply by wfactor to get the actual size of the (half)
	    // window in pixels.  Then divide by num_sigma to get the factor
	    // for the Gaussian.  We get r from the avgrng image, which is a
	    // simple low-pass filter of the range image.

	    double nom_win_width = avgrng->get(j,i) * corr_acc / baseline / 2;

// window_total and window_count could use reduction(+) but that won't work
// for min and max.  Could do min and max per thread and then min and max
// the result, but probably not worth it.
#pragma omp crtitical(npts)
	    {
	        window_total += nom_win_width;
	        window_count++;
	        if (min_window_out == 0.0)
		    min_window_out = nom_win_width;
	        if (nom_win_width < min_window_out)
		    min_window_out = nom_win_width;
	        if (nom_win_width > max_window_out)
		    max_window_out = nom_win_width;
	    }

	    // Get the actual window width, and allow for  ellipsoidal gaussians

	    double nsw_float = nom_win_width * wfactor;
	    double nlw_float = nsw_float * aspect_ratio;

	    // Finally get the width of the gaussian itself

	    double gauss_width_s = nsw_float / num_sigma;
	    double gauss_width_l = nlw_float / num_sigma;

	    int nsw = (int)(nsw_float + 0.5);
	    int nlw = (int)(nlw_float + 0.5);

	    // Set to minimum if given

	    if (min_half_win != 0) {
		if (nlw < min_half_win)
		    nlw = min_half_win;
		if (nsw < min_half_win)
		    nsw = min_half_win;
	    }

	    // If there's (still) no window, keep the central pixel and bail

	    if (nlw < 1 || nsw < 1) {
	        img_out->set(j,i,central_rng);
		continue;
	    }

	    // Figure out ranges for proximity filter
	    // First, the actual range error:  r^2 * ifov * c / b

	    double rng_err = central_rng*central_rng * corr_acc * ifov / baseline;

	    // Now compute the limits for no truncation (min) and full
	    // truncation (max)

	    double prox_min_rng = rng_err * prox_min;
	    double prox_max_rng = rng_err * prox_max;

//if (DEBUG) printf("line=%d rng=%f rng_err=%f pminr=%f pmaxr=%f\n", j, central_rng,rng_err, prox_min_rng, prox_max_rng);	//!!!!
// if (DEBUG) printf("gw=%f gw=%f nlw=%d nsw=%d\n", gauss_width_l, gauss_width_s, nlw, nsw);	//!!!!
	    // Compute coefficients for plane fit.  The matrix is sums of:
	    // [  xx  xy  x  ] [ A ]   [ xz ]
	    // [  xy  yy  y  ] [ B ] = [ yz ]
	    // [  x   y   n  ] [ C ]   [ z  ]
	    // However, since we want a *weighted* fit, each of the values
	    // being summed is multiplied by the appropriate weight, and n
	    // is actually the total weight.  (in other words, it's the sum
	    // of wt * x *x , not of (wt*x) * (wt*x) ).

	    double sumx2 = 0.0;
	    double sumy2 = 0.0;
	    double sumxy = 0.0;
	    double sumxz = 0.0;
	    double sumyz = 0.0;
	    double sumx  = 0.0;
	    double sumy  = 0.0;
	    double sumz  = 0.0;
	    double total_wt = 0.0;

	    // Weight is e ^ (-(x^2/2Gx^2 + y^2/2Gy^2)) where x and y are
	    // the distances from the center and Gx, Gy are the gaussian
	    // widths along each dimension.

	    // We modify the weight by a proximity filter.  If the actual
	    // point is within +/- prox_min_rng of the central point, we
	    // use the full weight.  If it's outside of +/- prox_max_rng,
	    // the weight is zero.  In between, we use a linear ramp to
	    // adjust the weight (so we don't create a discontinuity).

            for (int jj=-nlw; jj <= nlw; jj++) {
                for (int ii=-nsw; ii <= nsw; ii++) {

		    if (j+jj < 0 || j+jj >= nl || i+ii < 0 || i+ii >= ns)
			continue;			// went off edge

		    float dn = img_in->get(j+jj,i+ii);
		    if (dn == 0.0)
			continue;

		    double exponent = (jj*jj)/(gauss_width_l*gauss_width_l) +
				      (ii*ii)/(gauss_width_s*gauss_width_s);
		    double wt = exp(-exponent/2);

		    double prox_wt = 1.0;

		    if (prox_max_rng != 0) {	// 0 means disabled
			// Outside the max
			if (dn <= central_rng - prox_max_rng ||
			    dn >= central_rng + prox_max_rng) {

//if (DEBUG) printf("prox out jj=%d ii=%d\n",jj,ii);	//!!!!
			    continue;		// pixel is disabled
			// Inside the ring, on the low side
			} else if (dn < central_rng - prox_min_rng) {
			    prox_wt = 1.0 -
				      ((central_rng - prox_min_rng) - dn) /
				       (prox_max_rng - prox_min_rng);
			    wt *= prox_wt;
//if (DEBUG) printf("prox low: jj=%d ii=%d, dn=%f, %f\n", jj,ii,dn,prox_wt);//!!!!
			// Inside the ring, on the high side
			} else if (dn > central_rng + prox_min_rng) {
			    prox_wt = 1.0 -
				      ((dn - (central_rng + prox_min_rng)) /
				       (prox_max_rng - prox_min_rng));
			    wt *= prox_wt;
//if (DEBUG) printf("prox hi: jj=%d ii=%d, dn=%f, %f\n", jj,ii,dn,prox_wt);//!!!!
			}
			// else we're within min, so no modification
		    }

		    // Accumulate the sums...
		    // Somewhat surprisingly (to me), we weight each summed
		    // term, not the x,y,z values directly...

		    register double x = ii;	// presumably the assignment
		    register double y = jj;	// is just optimized away...
		    register double z = dn;

		    sumx2 += wt * x*x;
		    sumy2 += wt * y*y;
		    sumxy += wt * x*y;
		    sumxz += wt * x*z;
		    sumyz += wt * y*z;
		    sumx += wt * x;
		    sumy += wt * y;
		    sumz += wt * z;

		    total_wt += wt;
		}
	    }

	    // Now fit a plane to these points using Cramer's rule.  Much
	    // of this is commented out because we only actually need c,
	    // see below.

	    double m[3][3];
	    // double m1[3][3];
	    // double m2[3][3];
	    double m3[3][3];

	    m[0][0] = sumx2;	 m[0][1] = sumxy;	 m[0][2] = sumx;
	    m[1][0] = sumxy;	 m[1][1] = sumy2;	 m[1][2] = sumy;
	    m[2][0] = sumx;	 m[2][1] = sumy;	 m[2][2] = total_wt;

	    // memcpy(m1, m, sizeof(m));
	    // memcpy(m2, m, sizeof(m));
	    memcpy(m3, m, sizeof(m));

	    // m1[0][0] = sumxz;
	    // m2[0][1] = sumxz;
	    m3[0][2] = sumxz;
	    // m1[1][0] = sumyz;
	    // m2[1][1] = sumyz;
	    m3[1][2] = sumyz;
	    // m1[2][0] = sumz;
	    // m2[2][1] = sumz;
	    m3[2][2] = sumz;

	    // Get the determinants

	    double d = det33(m);
	    // double d1 = det33(m1);
	    // double d2 = det33(m2);
	    double d3 = det33(m3);

	    if (d == 0.0)
		continue;		// bail on this point

	    // Plane coefficients.  z = ax + by + c

	    // double a = d1/d;
	    // double b = d2/d;
	    double c = d3/d;

	    // Now that we have a plane fit, use it to predict the value
	    // at the center (x=0 y=0).  This devolves to z = c so we comment
	    // out the a and b determinations above, as well as m1, m2, d1, d2.

	    img_out->set(j,i,c);
        }
    }
    average_window = 0.0;
    if (window_count != 0)
	average_window = window_total / window_count;

}

