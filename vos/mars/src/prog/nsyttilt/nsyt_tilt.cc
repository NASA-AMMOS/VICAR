#include "nsyt_tilt.h"

#include "combinations.hpp"
#include "nsyt_utils.h"
#include "PigVector.h"
#include "PigCoordSystem.h"
#include "mars_support.h"

#include <cassert>
#include <cfloat>
#include <vector>

#define EPSILON 1e-6
#define SQ(x) ((x)*(x))

////////////////////////////////////////////////////////////////////////
// Implement a cache for foot values, since we calculate the same points
// over and over again, and it depends only on radius.  A bit tricky due
// to parallelism.
////////////////////////////////////////////////////////////////////////

class FootCache {
  private:
    const int _nl, _ns;
    const double _radius;
    SimpleImage<unsigned char> _cache_valid;
    SimpleImage<PigPoint> _cache_value;

  public:
    FootCache(int nl, int ns, double radius) : _nl(nl),_ns(ns),_radius(radius) {
	_cache_valid.alloc(nl,ns);
	_cache_valid.zero();
	_cache_value.alloc(nl,ns);
    }
    int getCache(int line, int samp, PigPoint &value, double radius)
    {
	int cache_hit = FALSE;
	if (radius != _radius)
	    return FALSE;
#pragma omp critical (nsyt_foot_cache)
	{
	    if (_cache_valid.get(line,samp)) {
		value = _cache_value.get(line,samp);
		cache_hit = TRUE;
	    }
	}
// end critical
	return cache_hit;
    }
    void setCache(int line, int samp, PigPoint &value, double radius)
    {
	if (radius != _radius)
	    return;
#pragma omp critical (nsyt_foot_cache)
	{
	    _cache_value.set(line, samp, value);
	    _cache_valid.set(line, samp, TRUE);
	}
// end critical
    }
};


static const PigPoint UP_VECTOR(0, 0, -1);

////////////////////////////////////////////////////////////////////////
// Calculate the Z at the given X,Y value (the center of the footprint)
// for a set of points plus a normal.  This is done by using the plane
// equation for a surface normal plus a point on the plane (where the point
// is the center of the foot):
// u(x-x0) + v(y-y0) + w(z-z0) = 0
// and solving for z:
// z = z0 - u(x-x0)/w - v(y-y0)/w
// We do this for each foot independently (remember with HP3 there are
// 4 feet).  We then find the lowest Z (max since +z is down) because that's
// the worst case for any of the feet.  Roughness is calculated as excursions
// *above* this point (not below) so it's important to use the worst-case.
// For the 3-foot case the three calculated Z's should be close to each other.
//
// Returns 1 on success, 0 on failure
////////////////////////////////////////////////////////////////////////

static int calculate_z(const std::vector<PigPoint> points, double x, double y,
		PigVector normal, double &output_z)
{
    output_z = 1e38;

    for (unsigned int i=0; i < points.size(); i++) {

	// z = z0 - u(x-x0)/w - v(y-y0)/w
	// where x0/y0/z0 is points[i] and uvw is normal.getX/Y/Z
	double z = points[i].getZ()
		- normal.getX()*(x-points[i].getX())/normal.getZ()
		- normal.getY()*(y-points[i].getY())/normal.getZ();

	if (z < output_z)
	    output_z = z;	// new lowest value
    }

    if (output_z > 1e37) return 0;		// no points
    return 1;					// success

}

////////////////////////////////////////////////////////////////////////
// Calculate the normal for a set of 3 points
////////////////////////////////////////////////////////////////////////

static void calculate_normal(const PigPoint &a,
                             const PigPoint &b,
                             const PigPoint &c,
                             PigPoint &normal)
{
    normal = (a - c) * (b - c);
    normal.normalize();

    // The orders of the points are arbitrary so we need to make sure
    // the normal points up from the terrain.
    if ((normal.getZ() > 0) != (UP_VECTOR.getZ() > 0))
    {
        normal *= -1;
    }
}

////////////////////////////////////////////////////////////////////////
// Compute normals including sinkage and all combinations of feet.
// All combinations of feet is handled by the caller using
// for_each_combination().  That's trivial for seis/wts but hp3 has 4
// feet so it becomes important there.  Each call to ComputeNormals contains
// only 3 feet.  Sinkage is handled here using "bits" to cover all 8 cases
// (sink or no sink for each of the 3 feet).
// We accumulate the "average" surface normal for each foot combination.
// This is so we can pass the normal out to the roughness program.  We
// pick the average because with 4 feet there is no one surface normal, and
// I can't think of anything better to use.  (for seis and wts it's an average
// of 1 value).  Note that we do this ONLY for the no-sinkage case (bits == 0).
////////////////////////////////////////////////////////////////////////

struct ComputeNormals
{
    std::vector<PigPoint> normals;
    PigPoint avg_normal;
    int n_avgs;

    // Remember, +Z is down, so a positive sinkage lowers the location.
    ComputeNormals(const double possible_sinkage)
        : possible_sinkage_vector_(0., 0., possible_sinkage)
    {
	avg_normal.setXYZ(0,0,0);
	n_avgs = 0;
    }

    // This is called against each combination (choose 3) of instrument feet.
    bool operator()(const PigPoint *const first, const PigPoint *const last)
    {
        if (first != last)
        {
            assert(first + 3 == last);

            // We use bitfields to try all possible permuations of feet
            // sinkage (000, 001, 010, 011, et cetera).

	    // if sinkage is 0 then we don't bother
	    int max_bits = 8;
	    if (possible_sinkage_vector_.getZ() == 0.0)
		max_bits = 1;

            for (int bits = 0; bits < max_bits; bits++)
            {
                PigPoint triangle[3] = {
                    first[0],
                    first[1],
                    first[2]
                };

                for (int position = 0; position < 3; position++)
                {
                    if (bits & (1 << position))
                    {
                        triangle[position] += possible_sinkage_vector_;
                    }
                }

		PigPoint normal;
                calculate_normal(triangle[0],
                                 triangle[1],
                                 triangle[2],
                                 normal);
		normals.push_back(normal);

		// Gather average normal across feet, for no sinkage case
		if (bits == 0) {
		    avg_normal += normal;
		    n_avgs++;
		}
            }
        }
        return false;
    }

private:

    const PigPoint possible_sinkage_vector_;
};

////////////////////////////////////////////////////////////////////////
// Return pair (min, max) tilt in radians, for delta tilt
// Returns 1 on success, 0 on failure
////////////////////////////////////////////////////////////////////////

int get_min_max_delta_tilt(
    std::vector<PigPoint> instrument_a,
    std::vector<PigPoint> instrument_b,
    const double possible_sinkage,
    double &min_tilt_radians, double &max_tilt_radians)
{
    const size_t combination_choose_size = 3u;

    const ComputeNormals compute_normals_a =
        for_each_combination(
            &instrument_a[0],
            &instrument_a[0] + combination_choose_size,
            &instrument_a[0] + instrument_a.size(),
            ComputeNormals(possible_sinkage));

    const ComputeNormals compute_normals_b =
        for_each_combination(
            &instrument_b[0],
            &instrument_b[0] + combination_choose_size,
            &instrument_b[0] + instrument_b.size(),
            ComputeNormals(possible_sinkage));

    double min_tilt_cos = 0.;
    double max_tilt_cos = 0.;

    // These shouldn't happen; they used to be asserts
    if (compute_normals_a.normals.size() <= 0 ||
	compute_normals_b.normals.size() <= 0) {
	return 0;
    }

    for (size_t ai = 0; ai < compute_normals_a.normals.size(); ai++)
    {
        for (size_t bi = 0; bi < compute_normals_b.normals.size(); bi++)
        {
            const double tilt_cos = compute_normals_a.normals[ai] %
                                    compute_normals_b.normals[bi];
            if (ai == 0 and bi == 0)
            {
                min_tilt_cos = max_tilt_cos = tilt_cos;
            }
            else
            {
		// cos gets bigger with smaller angle
                if (tilt_cos > min_tilt_cos)
                {
                    min_tilt_cos = tilt_cos;
                }

                if (tilt_cos < max_tilt_cos)
                {
                    max_tilt_cos = tilt_cos;
                }
            }
        }
    }

    min_tilt_radians = acos(min_tilt_cos);
    max_tilt_radians = acos(max_tilt_cos);
    return 1;
}

////////////////////////////////////////////////////////////////////////
// Return pair (min, max) tilt in radians, for standard tilt.
// Returns 1 on success, 0 on failure
////////////////////////////////////////////////////////////////////////

int get_min_max_tilt(
    std::vector<PigPoint> feet_points, const double possible_sinkage,
    double &min_tilt_radians, double &max_tilt_radians,
    PigVector *avg_normal)
{
    const size_t combination_choose_size = 3u;

    const ComputeNormals compute_normals =
        for_each_combination(
            &feet_points[0],
            &feet_points[0] + combination_choose_size,
            &feet_points[0] + feet_points.size(),
            ComputeNormals(possible_sinkage));

    double min_tilt_cos = 0.;
    double max_tilt_cos = 0.;

    if (compute_normals.normals.size() <= 0) {
	return 0;	// bad; used to be assert so this shouldn't happen
    }

    if (avg_normal != NULL)
    {
	*avg_normal = compute_normals.avg_normal;
	if (compute_normals.n_avgs != 0)
	    *avg_normal = *avg_normal / compute_normals.n_avgs;
    }

    for (size_t i = 0; i < compute_normals.normals.size(); i++)
    {
        const double tilt_cos = UP_VECTOR % compute_normals.normals[i];
        if (i == 0)
        {
            min_tilt_cos = max_tilt_cos = tilt_cos;
        }
        else
        {
	    // cos gets bigger with smaller angle
            if (tilt_cos > min_tilt_cos)
            {
                min_tilt_cos = tilt_cos;
            }

            if (tilt_cos < max_tilt_cos)
            {
                max_tilt_cos = tilt_cos;
            }
        }
    }

    min_tilt_radians = acos(min_tilt_cos);
    max_tilt_radians = acos(max_tilt_cos);
    return 1;		// success!
}

////////////////////////////////////////////////////////////////////////
// Figure out the footprint locations, accounting for the size of the
// actual foot.
// BOTH regular and delta tilt
////////////////////////////////////////////////////////////////////////

// Helper struct for process_foot_point
class FootPointStruct
{
  public:
    SimpleImage<double> *xyz;		// XYZ image
    double radius;			// radius of footprint circle
    double radius_sq;			// computed
    double center_x, center_y;		// XY of circle center
    int max_window;			// max window size to grow
    int num_points;			// number of points in below arrays
    double *highest_x, *highest_y;	// correspond to highest_z
    double *highest_z;			// the one that matters

    FootPointStruct(SimpleImage<double> xyz_in[3], int max_window_in)
    {
	xyz = xyz_in;
	radius = radius_sq = 0.0;
	center_x = center_y = 0.0;
	num_points = 0;
	max_window = max_window_in;
	highest_x = new double[SQ(2*max_window+1)];
	highest_y = new double[SQ(2*max_window+1)];
	highest_z = new double[SQ(2*max_window+1)];
    }
    ~FootPointStruct()
    {
	delete[] highest_x;
	delete[] highest_y;
	delete[] highest_z;
    }
};

////////////////////////////////////////////////////////////////////////
// Helper function for determine_instrument_feet_points().
// Check to see if the point is in the circle, and retain the highest Z if
// it is.  Remember +Z is down so highest Z is really the smallest number.
// Conforms to MarsWindowFunc signature
////////////////////////////////////////////////////////////////////////

extern "C" int process_foot_point(void *p, int xi, int yi)
{
    FootPointStruct *fp = (FootPointStruct *) p;

    double x = fp->xyz[0].get(yi,xi);
    double y = fp->xyz[1].get(yi,xi);
    double z = fp->xyz[2].get(yi,xi);

    // Skip this point if it's invalid
    if (x == 0.0 && y == 0.0 && z == 0.0)
	return 0;

    // Compute the distance from the center in XY space.
    // There is an implicit assumption here that we are working in Site
    // coordinates, i.e. the gravity vector is +Z.  This simplifies things
    // tremendously since we can ignore the quaternion, etc.  It's valid
    // because we really want the highest point in the circle, that a foot
    // would touch.

    double dist_sq = (x-fp->center_x)*(x-fp->center_x) +
		     (y-fp->center_y)*(y-fp->center_y);
    if (dist_sq > fp->radius_sq)
	return 0;			// outside the radius

    // Good point, save it in the arrays

    fp->highest_x[fp->num_points] = x;
    fp->highest_y[fp->num_points] = y;
    fp->highest_z[fp->num_points++] = z;

    return 1;
}

////////////////////////////////////////////////////////////////////////
// Compute the actual highest point in the foot area, using the points
// gathered in highest_*.  Ignores outliers (which is the whole reason for
// saving the points for analysis later rather than doing it in the
// grow_window helper function).
// NOTE: num_points must be >= 2; THIS IS NOT CHECKED HERE
////////////////////////////////////////////////////////////////////////

PigPoint compute_foot_point(double filter_scale, const FootPointStruct *fp)
{
    int i;
    PigPoint highest_pt(fp->highest_x[0], fp->highest_y[0], fp->highest_z[0]);

    // First get the mean z
    double mean_z = 0;

    for (i=0; i < fp->num_points; i++)
	mean_z += fp->highest_z[i];
    mean_z /= fp->num_points;

    // Now get the stdev (really the variance, to avoid a sqrt)

    double variance = 0;
    for (i=0; i < fp->num_points; i++)
	variance += SQ(fp->highest_z[i] - mean_z);
    variance /= (fp->num_points-1);

    // Now look for the highest point, but skip anything that is too many
    // stdev's away.  To avoid the sqrt (turning variance into stdev) we
    // square everything else instead.
    // Remember, highest is *minimum* Z...

    double compare_to = SQ(filter_scale) * variance;
    for (i=0; i < fp->num_points; i++) {
	register double z = fp->highest_z[i];
	register double diff = z - mean_z;
	if (SQ(diff) <= compare_to) {

	    // Good point, look at Z value.  Remember, highest is *MINIMUM*

	    if (z < highest_pt.getZ())
		highest_pt.setXYZ(fp->highest_x[i], fp->highest_y[i], z);
	}
    }
    return highest_pt;
}

////////////////////////////////////////////////////////////////////////
// Iterate through circles to extract z values from input_xyz. Return false if
// failed due to not finding a close point in the XYZ input.
// Basically we're looking for the highest point within each of the foot
// circles... the idea being, that's where the foot will sit.
////////////////////////////////////////////////////////////////////////

static bool determine_instrument_feet_points(
    const double raw_x,
    const double raw_y,
    const std::vector<SampleCircle> &circles,
    const PointCloud2D<double> &cloud,
    const KDTree &kd_tree,
    const double point_epsilon,
    std::vector<PigPoint> &points,
    int foot_window,
    double filter_scale,
    FootCache &cache,
    FootPointStruct *fp)
{
    bool okay = true;

    FOR_ITERATOR(circle_it, circles)
    {
        const auto circle = *circle_it;
        const double x = raw_x + circle.x;
        const double y = raw_y + circle.y;

	// Find the point closest to the center of the circle

        const auto index = closest_index(kd_tree, x, y, point_epsilon);
	if (index < 0) {
	    okay = false;
	    break;
	}

        int ll, ss;
        cloud.get_ls(index, ll, ss);

	PigPoint xyz_pt;
	if (cache.getCache(ll, ss, xyz_pt, circle.radius)) {
	    points.push_back(xyz_pt);
	    continue;
	}
  
	// Now look for all points within the foot circle.  We look a max
	// of foot_window pixels away from the center.  The highest is
	// the foot Z.

	// Ideally we'd use the input x/y since it's the "exact" position
	// of the foot.  However, for the cache to work right, we instead use
	// the x/y as found in the XYZ image (because it's the same every time
	// we hit that pixel).  The difference is negligible, and the speedup
	// enabled by the cache is well worth it.

	fp->radius = circle.radius;
	fp->radius_sq = circle.radius * circle.radius;
	fp->center_x = fp->xyz[0].get(ll,ss);
	fp->center_y = fp->xyz[1].get(ll,ss);
	fp->num_points = 0;

	mars_grow_window(process_foot_point, (void *)fp, foot_window, ss, ll,
		fp->xyz[2].getNS(), fp->xyz[2].getNL());

	if (fp->num_points > 2) {
	    PigPoint pt = compute_foot_point(filter_scale, fp);
            points.push_back(pt);
	    cache.setCache(ll, ss, pt, circle.radius);
	}
    }

    if (circles.size() != points.size())
	okay = false;			// a foot failed somewhere

    return okay;
}

////////////////////////////////////////////////////////////////////////
// Driver function for standard tilt
////////////////////////////////////////////////////////////////////////

void compute_tilt(const TiltParams &params,
                  SimpleImage<double> input_xyz[3],
                  const std::vector<SampleCircle> &raw_circles,
                  SimpleImage<double> output_min_max_tilt[2],
		  SimpleImage<double> output_avg_normal[3],
		  SimpleImage<double> &output_z, 
                  SimpleImage<double> output_xpx[3], 
                  PigCoordSystem *cs, PigCoordSystem *p_cs, int do_xpx)
{
    const int MSG_LEN = 1024;
    char msg[MSG_LEN];

    const PointCloud2D<double> cloud = xyz_to_point_cloud(input_xyz);
    KDTree kd_tree(
        2,
        cloud,
        nanoflann::KDTreeSingleIndexAdaptorParams(params.leaf_max_size));
    kd_tree.buildIndex();

    const int nl = output_min_max_tilt[0].getNL();
    const int ns = output_min_max_tilt[0].getNS();
    assert(nl == output_min_max_tilt[1].getNL());
    assert(ns == output_min_max_tilt[1].getNS());

    // Figure out the iteration closest to the center, for the output normal.
    // The +step*0.1 just makes sure we don't have a roundoff issue.
    double crng = (params.clock_range[1] - params.clock_range[0]);
    int nciter = (int)(crng + params.clock_step*0.1)/params.clock_step;
    int ctr_itr = nciter/2;

    snprintf(msg, MSG_LEN, "Gathering average normal for iteration %d (clock angle %f)\n",
		ctr_itr, params.clock_range[0]+ctr_itr*params.clock_step);
    zvmessage(msg, "");

    // The foot cache depends on all feet are the same radius... if any are
    // not, the cache is ignored.

    FootCache cache(nl, ns, raw_circles[0].radius);
  
    int omp_on = zvptst("OMP_ON");

#pragma omp parallel for schedule(dynamic) if (omp_on)
    for (int line = 0; line < nl; line++)
    {
        if (line % 100 == 0) printf("line %d\n", line);

	// Allocate here so it's thread-private but only once per line
	FootPointStruct *fp = new FootPointStruct(input_xyz,params.foot_window);

        for (int samp = 0; samp < ns; samp++)
        {
            output_min_max_tilt[0].set(line, samp, params.bad_tilt);
            output_min_max_tilt[1].set(line, samp, params.bad_tilt);

            const auto raw_x = input_xyz[0].get(line, samp);
            const auto raw_y = input_xyz[1].get(line, samp);
	    if (raw_x == 0.0 && raw_y == 0.0 &&
			input_xyz[2].get(line,samp) == 0.0)
		continue;		// no value here

            // Convert radial and cross-radial offsets to x and y offsets
            // if inst is WTS. If it is not WTS, then offset_x and offset_y
            // are 0.0.
            double offset_x = params.iparams.getOffsetX(raw_x, raw_y, params.inst);
            double offset_y = params.iparams.getOffsetY(raw_x, raw_y, params.inst);

	    // Clock loop.  All clock positions have to be good in order for
	    // us to report a valid point... because if any clock is unknown,
	    // we can't honestly say this is a potentially valid position,
	    // because it might clock into that hole.  So "all_good" keeps
	    // track of this.  Also, we only want to compute the surface
	    // normal on the center iteration, so "iter" and "ctr_itr"
	    // track that.
	    // The epsilon makes sure we get the last iteration

	    int iter = 0;
	    PigPoint average_normal;
	    int all_good = true;
	    for (double clock = params.clock_range[0];
			clock <= params.clock_range[1] + EPSILON;
			clock += params.clock_step) {

                const auto new_circles = params.iparams.rotateInstrument(
                    raw_circles, raw_x, raw_y, params.inst, clock);
                
                //apply offsets if inst is WTS
                const auto circles = params.iparams.applyWTSOffsets(new_circles,
                    offset_x, offset_y, params.inst);

                std::vector<PigPoint> points;
                if (determine_instrument_feet_points( raw_x, raw_y,
                    	circles, cloud, kd_tree,
                    	params.point_epsilon, points, params.foot_window,
			params.filter_scale, cache, fp)) {

		    double min, max;
                    if (get_min_max_tilt(points, params.possible_sinkage,
				min, max,
				(iter==ctr_itr) ? &average_normal : NULL)) {

		        min = PigRad2Deg(min);
		        double cur_min = output_min_max_tilt[0].get(line, samp);
		        if (cur_min == params.bad_tilt || cur_min > min)
			    output_min_max_tilt[0].set(line, samp, min);

		        max = PigRad2Deg(max);
		        double cur_max = output_min_max_tilt[1].get(line, samp);
		        if (cur_max == params.bad_tilt || cur_max < max)
			    output_min_max_tilt[1].set(line, samp, max);

			// Deal with the normal and Z value

			if (iter == ctr_itr) {
	        	    output_avg_normal[0].set(line, samp,
							average_normal.getX());
	        	    output_avg_normal[1].set(line, samp,
							average_normal.getY());
	        	    output_avg_normal[2].set(line, samp,
							average_normal.getZ());

			    double z_value;
			    if (calculate_z(points, raw_x, raw_y,
					    average_normal, z_value)) {
				output_z.set(line, samp, z_value);
 
                                if (do_xpx) {
                                    PigPoint old_xyz(raw_x, raw_y, z_value);
        
                                    if (old_xyz.getX() != 0.0 || 
                                        old_xyz.getY() != 0.0 || 
                                        old_xyz.getZ() != 0.0) {
           
                                        PigPoint new_xyz = p_cs->convertPoint(
                                            old_xyz, cs);
                                        output_xpx[0].set(line, samp,
                                                          new_xyz.getX());
                                        output_xpx[1].set(line, samp,
                                                          new_xyz.getY());
                                        output_xpx[2].set(line, samp, 
                                                          new_xyz.getZ());
                                    }
                                }

			    } else {
				all_good = false;
			    }
			}
		    } else {
			all_good = false;
		    }
		} else {
		    all_good = false;
		}
		iter++;

		if (!all_good)
		    break;		// no need to keep iterating clocks
	    }
	    if (!all_good) {
	        output_avg_normal[0].set(line, samp, 0.0);
	        output_avg_normal[1].set(line, samp, 0.0);
	        output_avg_normal[2].set(line, samp, 0.0);
		output_z.set(line, samp, 0.0);
                output_xpx[0].set(line, samp, 0.0);
                output_xpx[1].set(line, samp, 0.0);
                output_xpx[2].set(line, samp, 0.0);
		output_min_max_tilt[0].set(line, samp, params.bad_tilt);
		output_min_max_tilt[1].set(line, samp, params.bad_tilt);
	    }
	}
	delete fp;
    }
}

////////////////////////////////////////////////////////////////////////
// Driver function for delta tilt
////////////////////////////////////////////////////////////////////////

void compute_delta_tilt(const TiltParams &params,
                        SimpleImage<double> input_xyz[3],
                        const std::vector<SampleCircle> &raw_instrument_a,
                        const std::vector<SampleCircle> &raw_instrument_b,
                        InsightInstrument instrument_type_a,
                        InsightInstrument instrument_type_b,
                        SimpleImage<double> output_min_max_tilt[2])
{
    const PointCloud2D<double> cloud = xyz_to_point_cloud(input_xyz);
    KDTree kd_tree(
        2,
        cloud,
        nanoflann::KDTreeSingleIndexAdaptorParams(params.leaf_max_size));
    kd_tree.buildIndex();

    const int nl = output_min_max_tilt[0].getNL();
    const int ns = output_min_max_tilt[0].getNS();
    assert(nl == output_min_max_tilt[1].getNL());
    assert(ns == output_min_max_tilt[1].getNS());

    // The foot cache depends on all feet are the same radius... if any are
    // not, the cache is ignored.

    FootCache cache_a(nl, ns, raw_instrument_a[0].radius);
    FootCache cache_b(nl, ns, raw_instrument_b[0].radius);

    int omp_on = zvptst("OMP_ON");

#pragma omp parallel for schedule(dynamic) if (omp_on)
    for (int line = 0; line < nl; line++)
    {
	if (line % 100 == 0) printf("line %d\n", line);	

	// Allocate here so it's thread-private but only once per line
	FootPointStruct *fp = new FootPointStruct(input_xyz,params.foot_window);

        for (int samp = 0; samp < ns; samp++)
        {
            output_min_max_tilt[0].set(line, samp, params.bad_tilt);
            output_min_max_tilt[1].set(line, samp, params.bad_tilt);

            const auto raw_x = input_xyz[0].get(line, samp);
            const auto raw_y = input_xyz[1].get(line, samp);
	    if (raw_x == 0.0 && raw_y == 0.0 &&
			input_xyz[2].get(line,samp) == 0.0)
		continue;		// no value here

            // Convert radial and cross-radial offsets to x and y offsets
            // if inst is WTS. If it is not WTS, then offset_x and offset_y
            // are 0.0.
            double inst_a_offset_x = params.iparams.getOffsetX(raw_x, raw_y, 
                                                          instrument_type_a);
            double inst_a_offset_y = params.iparams.getOffsetY(raw_x, raw_y, 
                                                          instrument_type_a);
            double inst_b_offset_x = params.iparams.getOffsetX(raw_x, raw_y, 
                                                          instrument_type_b);
            double inst_b_offset_y = params.iparams.getOffsetY(raw_x, raw_y,
                                                          instrument_type_b);

	    // Clock loop (actually nested loops, for the two instruments).
	    // All clock positions have to be good in order for us to report
	    // a valid point... because if any clock is unknown, we can't
	    // honestly say this is a potentially valid position, because it
	    // might clock into that hole.  So "all_good" keeps track of this.
	    // The epsilon makes sure we get the last iteration

	    int all_good = true;
	    for (double clock = params.clock_range[0];
			clock <= params.clock_range[1] + EPSILON;
			clock += params.clock_step) {

                const auto new_instrument_a = params.iparams.rotateInstrument(
                		raw_instrument_a,
                		raw_x, raw_y,
                		instrument_type_a, clock);

                //apply offsets if inst is WTS
                const auto circles_a = params.iparams.applyWTSOffsets(
                                new_instrument_a, inst_a_offset_x,
                                inst_a_offset_y, instrument_type_a);

		for (double clock2 = params.delta_wts_range[0];
			    clock2 <= params.delta_wts_range[1] + EPSILON;
			    clock2 += params.delta_wts_step) {

            	    const auto new_instrument_b = params.iparams.rotateInstrument(
		                raw_instrument_b,
                                raw_x, raw_y,
                                instrument_type_b, clock2);
       
                    //apply offsets if inst is WTS
                    const auto circles_b = params.iparams.applyWTSOffsets(
                                new_instrument_b, inst_b_offset_x, 
                                inst_b_offset_y, instrument_type_b);

            	    std::vector<PigPoint> points_a;
            	    std::vector<PigPoint> points_b;
            	    if (determine_instrument_feet_points(
                    			raw_x, raw_y, circles_a,
                    			cloud, kd_tree, params.point_epsilon,
                    			points_a, params.foot_window,
					params.filter_scale, cache_a, fp) &&
               		determine_instrument_feet_points(
                    			raw_x, raw_y, circles_b,
                    			cloud, kd_tree, params.point_epsilon,
                    			points_b, params.foot_window,
					params.filter_scale, cache_b, fp)) {

			double min, max;
                	if (get_min_max_delta_tilt(points_a, points_b,
                        	params.possible_sinkage, min, max)) {

			    min = PigRad2Deg(min);
			    double cur_min = output_min_max_tilt[0].get(line,samp);
			    if (cur_min == params.bad_tilt || cur_min > min)
			        output_min_max_tilt[0].set(line, samp, min);

			    max = PigRad2Deg(max);
			    double cur_max = output_min_max_tilt[0].get(line,samp);
			    if (cur_max == params.bad_tilt || cur_max < max)
			        output_min_max_tilt[1].set(line, samp, max);
			} else {
			    all_good = false;
			}
		    } else {
			all_good = false;
		    }
		}
            }
	    if (!all_good) {
		output_min_max_tilt[0].set(line, samp, params.bad_tilt);
		output_min_max_tilt[1].set(line, samp, params.bad_tilt);
	    }
        }
	delete fp;
    }
}

