#include "nsyt_roughness.h"
#include "nsyt_utils.h"
#include "mars_support.h"

#ifndef MIN
#define MIN(a, b) ((a) < (b) ? (a) : (b))
#endif
#ifndef MAX
#define MAX(a, b) ((a) > (b) ? (a) : (b))
#endif
#ifndef SQ
#define SQ(x) ((x)*(x))
#endif

#define EPSILON 1e-6

class WindowStruct
{
  public:
    SimpleImage<PigPoint> xyz;	// XYZ image
    double radius_sq;	   // radius squared
    PigVector normal;	   // computed normal
    PigPoint pt_center;	   // Center point for the roughness patch
    int num_close_points;  // # of points w/in radius (size of rz_coords)
    double *rz_coords;     // pointer to deviations of points w/in radius
			   // (height above local plane)
			   // Although +Z is down, these values are projected
			   // along n, which points up. Therefore up is positive
    int max_window;        // max size to expand window
    int use_hill;	   // TRUE if doing hill rough; false for standard
    double projected_z;	   // projected zix value iff use_hill is true

    WindowStruct(SimpleImage<PigPoint> xyz_in,
		 double radius_in, int max_window_in, int use_hill_in)
    {
	xyz = xyz_in;
        setRadius(radius_in);
	max_window = max_window_in;
        rz_coords = new double[SQ(2*max_window+1)];
        num_close_points = 0;
	use_hill = use_hill_in;
	projected_z = 0.0;
    };

    ~WindowStruct()
    {
	delete rz_coords;
    }

    void setRadius(double r)
    {
	radius_sq = SQ(r);
    }
};

////////////////////////////////////////////////////////////////////////
// Process a single point.  Checks to see if it is within the radius
// and saves it if so.  Note that we gather all points regardless of hill
// algorithm, because that's taken care of later.
////////////////////////////////////////////////////////////////////////

// conforms to MarsWindowFunc signature
extern "C" int process_point(void *p, int xi, int yi)
{
    WindowStruct *wp = (WindowStruct *)p;

    int found_a_point = 0;
    double radius_sq;           // square of planar dist from center to pt
    double dtemp;           	// temporary variable
    double d;			// distance along normal

    PigPoint pt = wp->xyz.get(yi,xi);
    // Skip this point if it's invalid (all zeroes)
    if (pt.getX() == 0.0 && pt.getY() == 0.0 && pt.getZ() == 0.0)
	return 0;

    // Compute the radial distance from the center point.  This is just
    // the magnitude of the cross product between the vector (pt-center) and
    // the normal.

    PigVector pdiff = pt - wp->pt_center;
    radius_sq = (pdiff * wp->normal).magnitude_sq();

    if (radius_sq <= wp->radius_sq) {

        /* compute the distance to the point along the normal (dot product) */

        wp->rz_coords[wp->num_close_points++] = pt % wp->normal;
        found_a_point = 1;
    }

    return found_a_point;
}

////////////////////////////////////////////////////////////////////////
// Computes the actual roughness, given an array of deviations from the
// local surface plane.
////////////////////////////////////////////////////////////////////////

static double compute_roughness(const RoughnessParams &params,
                                  WindowStruct *wp)
{
    int i;
    double d;
    double mean;
    double variance;
    double d_max = -1e12;
    double d_min = 1e12;
    double delta_overall;

    assert(wp->num_close_points > 1);

    mean = 0.;

    // Hill roughness uses the instrument plane as the "mean".  We consider
    // only points above this plane for determining the roughness.

    if (wp->use_hill)
	mean = wp->projected_z;
    else {
	for (i = 0; i < wp->num_close_points; i++) {
	    mean = mean + wp->rz_coords[i];
	}
	mean = mean/wp->num_close_points;
    }

    // Gather the stdev.  This is slightly complicated in the hill form
    // because we want to consider only points above the plane.  Note that
    // we actually get variance (stdev squared) to avoid a sqrt.

    // Although +Z is down, rz_coords is projected along the normal, which
    // points up.  Therefore, up is positive in rz_coords.
    variance = 0.;
    if (wp->use_hill) {			// look only at "up" excursions
	int n = 0;
	for (i = 0; i < wp->num_close_points; i++) {
	    if (wp->rz_coords[i] > mean) {
		variance += SQ(wp->rz_coords[i] - mean);
		n++;
	    }
	}
	if (n < 2)
	    return 0.0;		// Nothing is above the plane, so it's 0 rough

        variance = variance/(n - 1);
    }
    else {			// The usual stdev considering all points
        for (i = 0; i < wp->num_close_points; i++) {
	    variance += SQ(wp->rz_coords[i]-mean);
        }

        variance = variance/(wp->num_close_points - 1);
    }

    double filter_sq = SQ(params.filter_scale);
    for (i = 0; i < wp->num_close_points; i++) {
	d = wp->rz_coords[i];
	if (wp->use_hill && d <= mean)	// projected along n so + is up
	    continue;

	// Skip points that are too many stdev's away.  To avoid the sqrt
	// (turning variance into stdev) we square everything else instead.

	double diff = d - mean;
	if (SQ(diff) > filter_sq * variance) {
	    continue;
	}

	if (d > d_max) {
	    d_max = d;
	}
	if (d < d_min) {
	    d_min = d;
	}
    }

    if (wp->use_hill)
	delta_overall = fabs(d_max - mean);
    else
	delta_overall = fabs(d_max - d_min);

    return delta_overall;
}

////////////////////////////////////////////////////////////////////////
// Compute roughness for one point
////////////////////////////////////////////////////////////////////////

double compute_one_roughness(const RoughnessParams &params,
                           SimpleImage<PigPoint> xyz,  /* xyz images */
                           SimpleImage<double> uvw[3], /* uvw images */
			   SimpleImage<double> *zix,    /* z image, or null */
			   int line, int samp,
			   WindowStruct *wp)
{
    int xi, yi;                   /* loop variables for window */
    int found_a_point;                     /* found any points in window? */
    int x0, y0, x1, y1;                    /* window boundaries */
    int lim;                               /* loop limit */
    int window_size;                       /* current window size */

    int num_rows = xyz.getNL();
    int num_cols = xyz.getNS();

    // get the center point
    PigPoint pt_center = xyz.get(line,samp);

    // skip this point if it's invalid (all zeroes)
    if (pt_center.getX()==0.0 && pt_center.getY()==0.0 && pt_center.getZ()==0.0) {
	return params.bad_roughness;
    }

    if (fabs(pt_center.getX() - params.x_center) > params.box_radius ||
        fabs(pt_center.getY() - params.y_center) > params.box_radius) {

        return params.bad_roughness;
    }

    wp->pt_center = pt_center;

    // If we're doing "hill" style roughness, then compute the Z level
    // of the plane of the feet.  The Z is given in the zix image;
    // that's combined with the center x and y.  Note that this might
    // be a different Z than the XYZ image has!  We then take the dot
    // product of that vector with the UVW vector; that gives us the
    // projection of that point in the direction of the normal.  That
    // is then directly comparable to "d" (stored in wp->rz_coords)
    // in the roughness code.

    if (wp->use_hill) {

	// Point is (pt_center.x, pt_center.y, zz); normal is wp->n
	PigPoint inst_center = pt_center;
	inst_center.setZ(zix->get(line, samp));
	wp->projected_z = inst_center % wp->normal;		// dot product
    }


    // since we don't know how big a window we need to search in order
    // to find points within the desired radius, start with a small
    // window and keep getting larger until none of the points in the
    // window are within the radius

    wp->num_close_points = 0;

    mars_grow_window(process_point, (void *)wp,
		wp->max_window, samp, line, num_cols, num_rows);

    // If we got enough points, compute an answer

    if (wp->num_close_points < params.min_close_points)
	return params.bad_roughness;

    return compute_roughness(params, wp);

}

/***********************************************************************
 * Compute the roughess for the given shape.  This computes the actual
 * numeric roughness value for one band, not the status band.  It can
 * compute either the standard roughness or the "Hill" roughness.  Hill
 * requires the zix image, which is not needed in the standard case.
 ***********************************************************************/

void computeEitherRoughness(const RoughnessParams &params,
		SimpleImage<PigPoint> xyz,
		SimpleImage<double> uix[3],
		SimpleImage<double> *zix,
		InstrumentShape &shape,
		SimpleImage<double> &rough,
		double clock_range[2], double clock_step,
		int use_hill)
{
    int max_window;

    if (params.max_window_size > 0)
      max_window = params.max_window_size;
    else
      max_window = MIN(rough.getNL(), rough.getNS()) / 8;

    zvmessage("Creating point cloud\n", "");
    const PointCloud2D<double> cloud = xyz_to_point_cloud(xyz);
    KDTree kd_tree(2, cloud,
              nanoflann::KDTreeSingleIndexAdaptorParams(params.leaf_max_size));
    kd_tree.buildIndex();
    zvmessage("Done with point cloud\n", "");

    const int nl = rough.getNL();
    const int ns = rough.getNS();

    int omp_on = zvptst("OMP_ON");

#pragma omp parallel for schedule(dynamic) if (omp_on)
    for (int line = 0; line < nl; line++) {

        if (line%100 == 0) printf("Processing line %d\n", line);

        // Ordinarily we'd set up WindowStruct and, more importantly, the
        // allocated rz_coords outside the loop.  However, doing so inside
        // the loop allows for the loop to be parallelized - threads do not
        // share the same rz_coords then.  As a compromise, we parallelize
        // per row only, so the allocations aren't done TOO often.
        // Note: radius is set later, for each call

        WindowStruct *wp = new WindowStruct(xyz, 0.0, max_window, use_hill);

        for (int samp = 0; samp < ns; samp++) {

            rough.set(line, samp, params.bad_roughness);

	    PigPoint xyz_pt = xyz.get(line,samp);
            const double raw_x = xyz_pt.getX();
            const double raw_y = xyz_pt.getY();
	    if (raw_x == 0.0 && raw_y == 0.0 && xyz_pt.getZ() == 0.0)
	        continue;			// no value here

	    // Get the normal for the center point.  The single
	    // instrument normal at the center point is used as the normal
	    // for all patches, across all clocks.

	    wp->normal.setXYZ(uix[0].get(line, samp), uix[1].get(line, samp), uix[2].get(line, samp));

	    // skip this point if it's invalid (all zeroes)
	    if (wp->normal.getX() == 0.0 && wp->normal.getY() == 0.0 && wp->normal.getZ() == 0.0)
		continue;

            double rval = -9999.0;  // lower than any valid, but not the bad val
            
            // Convert radial and cross-radial offsets to x and y offsets
            // if inst is WTS. If it is not WTS, then offset_x and offset_y
            // are 0.0.
            double offset_x = params.iparams.getOffsetX(raw_x, raw_y, params.inst);
            double offset_y = params.iparams.getOffsetY(raw_x, raw_y, params.inst);
            
	    // Clock loop.  All clock positions have to be good in order for
	    // us to report a valid point... because if any clock is unknown,
	    // we can't honestly say this is a potentially valid position,
	    // because it might clock into that hole.  So "all_good" keeps
	    // track of this.
	    // The epsilon makes sure we get the last iteration

	    int iter = 0;
	    int all_good = true;
	    for (double clock = clock_range[0];
			clock <= clock_range[1]+EPSILON && all_good;
			clock += clock_step) {

		const auto new_circles = params.iparams.rotateInstrument(
			shape.circles, raw_x, raw_y, params.inst, clock);

                //apply offsets if inst is WTS
                const auto circles = params.iparams.applyWTSOffsets(new_circles,
                    offset_x, offset_y, params.inst);

		// (this is where we diverge from compute_tilt in nsyttilt)
		// Now loop over all the circles in the given shape

          	FOR_ITERATOR(circle_it, circles) {
            	    const auto circle = *circle_it;
		    const double x = raw_x + circle.x;
		    const double y = raw_y + circle.y;

		    // Find the point closest to the center of the circle

            	    const auto index = closest_index(kd_tree, x, y,
                                           params.point_epsilon);

		    if (index < 0) {
			all_good = false;
			rval = params.bad_roughness;
			break;
		    }
		    int ll, ss;
		    cloud.get_ls(index, ll, ss);

		    // Calculate roughness on this circle

		    wp->setRadius(circle.radius);
		    double tmp_rval = compute_one_roughness(params,
						xyz, uix, zix, ll, ss, wp);

		    // If we find a rougher circle, use that as the result
		    // Once we get a bad rough, the result is bad, period.

		    if (rval != params.bad_roughness &&
			tmp_rval != params.bad_roughness && tmp_rval > rval) {
			rval = tmp_rval;
		    }
		    if (tmp_rval == params.bad_roughness) {
			rval = params.bad_roughness;
			all_good = FALSE;
			break;
		    }
		}			// end for circles
		if (!all_good)
		    break;
	    }				// end clock loop

	    if (!all_good)
		rval = params.bad_roughness; // just in case (should already be)

	    rough.set(line, samp, rval);
	}
	delete wp;
    }

}

//**********************************************************************
// Compute the standard roughess for the given shape.  This is the peak-to-peak
// variation from the surface plane across the image.  Unlike "hill" roughness
// it does not need the zix input image.
//**********************************************************************

void computeStdRoughness(const RoughnessParams &params,
		SimpleImage<PigPoint> xyz,
		SimpleImage<double> uvw[3],
		InstrumentShape &shape,
		SimpleImage<double> &rough,
		double clock_range[2], double clock_step)
{
    computeEitherRoughness(params, xyz, uvw, NULL, shape, rough,
			   clock_range, clock_step, FALSE);
}

//**********************************************************************
// Compute the "Z-up" roughess for the given shape.  This is like normal
// roughness except it looks only at upwards (negative, since Z is down)
// excursions from the plane defined by the UIx (instrument normal) and
// ZIx (instrument Z).  The Z has already been modified by the sinkage
// (settling) value in params.
//**********************************************************************

void computeHillRoughness(const RoughnessParams &params,
		SimpleImage<PigPoint> xyz,
		SimpleImage<double> uix[3],
		SimpleImage<double> zix,
		InstrumentShape &shape,
		SimpleImage<double> &rough,
		double clock_range[2], double clock_step)
{
    computeEitherRoughness(params, xyz, uix, &zix, shape, rough,
			   clock_range, clock_step, TRUE);
}

