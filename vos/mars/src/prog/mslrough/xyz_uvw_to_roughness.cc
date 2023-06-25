/**********************************************************************
 *
 * xyz_uvw_to_roughness.cc - code for computing surface roughness for
 *                          DRILL and DRT
 *
 * Matt Robinson, JPL, 2011 
 *                     based on MER roughness code by Chris Leger, 2003
 *
 * Bob Crocco, 2020 updated for curvature
 *                     based on code by Kris Wehage and
 *                     Curtis Collins, 2020
 *
 **********************************************************************/
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "xyz_uvw_to_roughness.h"
#include "zvproto.h"
#ifdef _OPENMP
#include <omp.h>
#endif
/**********************************************************************
 *
 * Macros
 *
 **********************************************************************/

#define MIN(a, b) ((a) < (b) ? (a) : (b))
#define MAX(a, b) ((a) > (b) ? (a) : (b))
#define SQ(x) ((x)*(x))

#if 0
#define debug_level (1)
#define DEBUG(val, foo) if (val <= debug_level) foo
#else
#define DEBUG(val, foo)
#endif


/* roughness state */
#define no_solution (0)
#define both_bad    (1)
#define patch_bad   (2)
#define ring_bad    (3)
#define good        (4)

#define near_equal_epsilon (1e-6)

/* curvature state*/
//shared: no_solution       (0)
#define cs_both_bad         (1)
#define cs_convex_bad       (2)
#define cs_concave_bad      (3)
#define cs_high_thresh_good (5)
#define cs_low_thresh_good  (6)

#define SENTINEL_VAL 1e12

/**********************************************************************
 *
 * Function definitions
 *
 **********************************************************************/
int process_point(XyzUvwToRoughnessParams *params,
        int xi, int yi, int num_cols, double *xyz[3], 
        double n[3], double rc[3], double* rz_coords[2],
        double center[3], int* num_close_points) { 
  int found_a_point = 0;
  double pt[3];                          /* other point */
  double radius;                         /* planar dist from center to pt */
  register double dtemp;                 /* temporary variable */
  double rv[3];                          /* radial vector to point */
  double d;

  pt[0] = xyz[0][(yi)*num_cols + (xi)];
  pt[1] = xyz[1][(yi)*num_cols + (xi)];
  pt[2] = xyz[2][(yi)*num_cols + (xi)];

  /* skip this point if it's invalid (all zeroes) */
  if (fabs(pt[0]) < XYZ_EPS && 
      fabs(pt[1]) < XYZ_EPS && 
      fabs(pt[2]) < XYZ_EPS) {
    DEBUG(3, printf("skipped empty\n"));
    return 0;
  }

  /* skip this point if it fails the sphere test (optional)
     the sphere test rejects points that are nearby in pixels
     but distant in meters. The use case is to filter points
     that are within the cylindrical projection but are in front
     or behind the point being test due to a near horizontal normal.
     This test is not ideal as it will include points on small peaks
     behind the point of interest and ignore real hazards such as 
     overhangs along the direction of the normal*/
  if(params->sphere_cull_radiusSq > 0)  {
      double deltaPt[3];
      deltaPt[0] = center[0] - pt[0];
      deltaPt[1] = center[1] - pt[1];
      deltaPt[2] = center[2] - pt[2];
      double distSq = SQ(deltaPt[0]) + SQ(deltaPt[1]) + SQ(deltaPt[2]);
      if(distSq > params->sphere_cull_radiusSq) {
          DEBUG(3, printf("skipped sphere\n"));
          return 0;
      }
  }
  /* compute the distance to the point along the normal */
  d = pt[0]*n[0] + pt[1]*n[1] + pt[2]*n[2];

  /* compute the radial vector
   *   rv = P * pt = (I3 - n*n^T) * pt = pt - n*(n^T*pt) = pt - d*n
   */
  rv[0] = pt[0] - n[0]*d;
  rv[1] = pt[1] - n[1]*d;
  rv[2] = pt[2] - n[2]*d;
  
  /* compute the radial distance from the center point.
   * i've done a little optimization here: the original 
   * equations are:
   * 
   *   r = P*(pt - pt_current);
   *   radius = abs(r);
   * 
   * But we've already computed rc = P*pt_current, so 
   * we can compute the radius as:
   * 
   *   rv = P*pt;
   *   radius = abs(rv-rc)
   * 
   * Which allows us to be slightly more efficient, since
   * we don't need to compute the entire vector (pt-pt_current)
   * and keep it around when computing r.
   */
            
  dtemp = rv[0]-rc[0];
  radius = SQ(dtemp);
  dtemp = rv[1]-rc[1];
  radius += SQ(dtemp);
  dtemp = rv[2]-rc[2];
  radius += SQ(dtemp);
  
  /* radius = sqrt(radius); */
  DEBUG(2, printf("n    pt: %.3f %.3f %.3f\n",
		  pt[0], pt[1], pt[2]));
  DEBUG(2, printf("    rv: %.3f %.3f %.3f   radius = %.3f\n",
		  rv[0], rv[1], rv[2], radius));

  double sqInner = SQ(params->ring_inner_radius);
  double sqOuter = SQ(params->ring_outer_radius);
  double sqPatch = SQ(params->patch_radius);

  int do_ring = fabs(params->ring_outer_radius - params->ring_inner_radius) > near_equal_epsilon;
  int in_ring = radius >= sqInner && 
                radius <= sqOuter;

  if ((do_ring && in_ring) || 
      radius <= sqPatch) {

    rz_coords[0][*num_close_points] = radius;
    rz_coords[1][*num_close_points] = d;
    found_a_point = 1;
    *num_close_points = *num_close_points + 1;
  }

  return found_a_point;
}

void compute_roughness(int index, XyzUvwToRoughnessParams *params, 
          double d_patch_min, double d_patch_max,
          double d_ring_min, double d_ring_max,
          double *roughness[3]) {
  
  double delta_roughness_patch;
  double delta_roughness_ring;
  int roughness_state = good; 
 
  /* Band 1 is patch*/

  bool bad_patch_stats = (d_patch_max == -SENTINEL_VAL) || (d_patch_min == SENTINEL_VAL);
  bool bad_ring_stats = (d_ring_max == -SENTINEL_VAL) || (d_ring_min == SENTINEL_VAL);
  if(bad_patch_stats && bad_ring_stats)
  {
      //not possible to calculate roughness, return here so state 
      // remains 'no_solution', and values remain bad
      return;
  }

  if(bad_patch_stats)
  {
    roughness[1][index] = params->bad_roughness;
    roughness_state = patch_bad;
  }
  else
  {
    delta_roughness_patch = fabs(d_patch_max - d_patch_min);
    roughness[1][index] = delta_roughness_patch;

    if(delta_roughness_patch > params->max_roughness_patch) {
        roughness_state = patch_bad;
    }
  }

  /* Band 2 is ring*/
  /* Include ring only if inner radius != outer radius */
  if(fabs(params->ring_outer_radius - params->ring_inner_radius) > near_equal_epsilon) {

    bool ring_is_bad = false;
    if(bad_ring_stats)
    {
        roughness[2][index] = params->bad_roughness;
        ring_is_bad = true;
    }
    else
    { 
        delta_roughness_ring = fabs(d_ring_max - d_ring_min);
        roughness[2][index] = delta_roughness_ring;
        ring_is_bad = delta_roughness_ring > params->max_roughness_ring;
    }

    if(ring_is_bad)
     {
        if(roughness_state == good) {
            roughness_state = ring_bad;
        } else {
            /* Mark both bad if patch was also bad */
            roughness_state = both_bad;
        }
    }
  }

  roughness[0][index] = roughness_state;

}

void compute_curvature(int index, XyzUvwToRoughnessParams *params,
          double d_patch_min, double d_patch_max,
          double mean,
          double *curvature[3]) {
 
  int curvature_state;
  int convexity_passes;
  int concavity_passes;
  double delta_convex_patch;
  double delta_concave_patch;

  bool bad_patch_stats = d_patch_max == -SENTINEL_VAL || d_patch_min == SENTINEL_VAL;
  if(bad_patch_stats)
  {
      //not possible to calculate curvature, return here so state 
      // remains 'no_solution', and values remain bad
      return;
  }
  
  delta_convex_patch = fabs(d_patch_max - mean);
  delta_concave_patch = fabs(d_patch_min - mean);
  curvature[1][index] = delta_concave_patch;
  curvature[2][index] = delta_convex_patch;
    
  /* thresholds are absolute distance from mean plane.
     low threshold: more stringent test
     high threshold: more relaxed test */
      
    //                a
    // --------------------------------- high thresh convex
    //                b
    // --------------------------------- low thresh convex
    //                c
    // --------------------------------- mean plane
    //                d
    // --------------------------------- low thresh concave
    //                e
    // --------------------------------- high thresh concave
    //                f

    bool c = delta_convex_patch <= params->convexity_threshold_low;   //convexity_low_passes
    bool b = delta_convex_patch <= params->convexity_threshold_high;  //convexity_high_passes
    bool a = !b && !c;                                                // all convex fail

    bool d = delta_concave_patch <= params->concavity_threshold_low;  // concavity_low_passes
    bool e = delta_concave_patch <= params->concavity_threshold_high; //concavity_high_passes
    bool f = !d && !e;                                                // all concave fail

    if (a && f) {
        curvature_state = cs_both_bad;          //both bad
    } else if (c && d) {
        curvature_state = cs_low_thresh_good;   //both good
    } else if (a && (d || e)) {
        curvature_state = cs_convex_bad;        //convex bad
    } else if (f & (b || c)) {
        curvature_state = cs_concave_bad;       //concave bad
    } else if ((b || c) && (e || d)) {
        curvature_state = cs_high_thresh_good; //high thresh good
    } else {
        char msg[256];
        snprintf(msg, 256, "unexpected curvature status calculation");
        zvmessage(msg, "");
        zabend();
    }

  curvature[0][index] = curvature_state;
}

int compute_output(int index, XyzUvwToRoughnessParams *params, 
          double *output[3], double* rz_coords[2], int num_close_points) 
  {
    int i;
    double radius;
    double d;
    double mean;
    double std;
    double d_patch_max = -SENTINEL_VAL;
    double d_patch_min = SENTINEL_VAL;
    double d_ring_max = -SENTINEL_VAL;
    double d_ring_min = SENTINEL_VAL;

    if(num_close_points <=1)return -1;

    mean = 0.0;

    for(i = 0; i < num_close_points; i++) {
      mean = mean + rz_coords[1][i];
      DEBUG(2, printf("    rz_coords: %.3f %.3f\n",
        rz_coords[0][i],rz_coords[1][i]));
    }

    mean = mean/num_close_points;

    std = 0.0;
    for(i = 0; i < num_close_points; i++) {
      std = std + SQ(rz_coords[1][i]-mean);
    }

    std = std/(num_close_points - 1);
    std = sqrt(std);

    for(i = 0; i < num_close_points; i++) {

      radius = rz_coords[0][i];

      d =  rz_coords[1][i];

      if(fabs(d-mean) > params->filter_scale*std) continue;
  
      if(radius <= SQ(params->patch_radius)) {
        if(d > d_patch_max) d_patch_max = d;
        if(d < d_patch_min) d_patch_min = d;
      }

      if(radius >= SQ(params->ring_inner_radius) && //NOTE: was > INNER_RADIUS in msl code
          radius <= SQ(params->ring_outer_radius)) {
        if(d > d_ring_max) d_ring_max = d;
        if(d < d_ring_min) d_ring_min = d;
      }
    }

    if(params->do_curvature)
    {
        compute_curvature(index, params, d_patch_min, d_patch_max,
          mean, output);
    }
    else
    {
       compute_roughness(index, params, d_patch_min, d_patch_max,
          d_ring_min, d_ring_max, output);
    }

    return 0;
  } 

int xyz_uvw_to_roughness(XyzUvwToRoughnessParams *params,   /* parameters */
      double *xyz[3], /* arrays of x, y, z points */
      double *uvw[3], /* arrays of u, v, w points */
      int num_rows,   /* number of rows in images */
      int num_cols,   /* number of columns in images */
      int uvw_is_matrix, /* if true, uvw is a matrix of
                            vectors with num_rows and
                            num_cols dimensions like xyz.
                            if false, uvw is a single vector. */
      double *output[3], /* output array */
      int omp_on)     /* multithread or not */
 {  
  
  int max_window;                /* conservative window max */
  double bad_value;

  if (params->max_window_size > 0) {
    max_window = params->max_window_size;
  } else {
    max_window = MIN(num_rows, num_cols)/8;
  }

  /* clear output map */
  for(int idx = 0; idx < 3; idx++)
    memset(output[idx], 0, num_rows*num_cols*sizeof(double));

  /* loop over points in image, ignoring border of window_radius pixels
   * since we won't be able to center a window in those regions.  (Note 
   * that we've already zeroed the uvw arrays, so we don't have to do
   * anything additional for the border pixels)
   */

  /* set the output to a bad value; we'll re-set it if
    * we're able to compute an actual value.
    */
  bad_value = params->do_curvature ? params->bad_curvature : 
  params->bad_roughness;
    
#ifdef _OPENMP
  #pragma omp parallel for schedule(dynamic) if (omp_on)
  for (int y_current = 0; y_current < num_rows; y_current++) {
#else
  for (int y_current = 0; y_current < num_rows; y_current++) {
#endif

    if (y_current % 100 == 0) {
	char msg[256];
	snprintf(msg, 256, "line %d", y_current);
	zvmessage(msg, "");
    }
    
    int i;
    int index;

    double n[3];                           /* computed normal */
    int num_close_points;                  /* # of points w/in radius */
    double rc[3];                          /* radial projection to center */
    double *rz_coords[2];                  /* pointer to cylindrical coordinates of points w/in radius 
                                            cylindrical coords are radius and height above local plane */
    int xi, yi;           /* loop variables for window */
    double pt_center[3];           /* center point */
    int x0, y0, x1, y1;            /* window boundaries */

    /* allocate filtered subwindow */
    for (i = 0; i < 2; i++) {
        rz_coords[i] = (double*)malloc(SQ(2*max_window+1)*sizeof(double));
    }

    for (int x_current = 0; x_current < num_cols; x_current++) {

      /* faster to not clear the local points, counters use the num_close_points to 
         avoid reading junk from previous iterations */
      //   for (i = 0; i < 2; i++) {
      //         memset(rz_coords[i], 0, SQ(2*max_window+1)*sizeof(double));
      //   }

      index = y_current*num_cols + x_current;
     
      output[0][index] = (double)no_solution;
      output[1][index] = bad_value;
      output[2][index] = bad_value;

      /* get the normal for the center point. this check is done
       * first since it'll eliminate more invalid points then
       * checking the xyz point (not all xyz points have a 
       * defined normal).
       */
      if (uvw_is_matrix) {
        n[0] = uvw[0][index];
        n[1] = uvw[1][index];
        n[2] = uvw[2][index];
      }
      else {
        n[0] = uvw[0][0];
        n[1] = uvw[1][0];
        n[2] = uvw[2][0];
      }
      
      /* skip this point if it's invalid (all zeroes) */
      if (fabs(n[0]) < XYZ_EPS && 
          fabs(n[1]) < XYZ_EPS && 
          fabs(n[2]) < XYZ_EPS) {
        continue;
      }

      /* get the center point */
      pt_center[0] = xyz[0][index];
      pt_center[1] = xyz[1][index];
      pt_center[2] = xyz[2][index];

      /* skip this point if it's invalid (all zeroes) */
      if (fabs(pt_center[0]) < XYZ_EPS && 
	  fabs(pt_center[1]) < XYZ_EPS && 
	  fabs(pt_center[2]) < XYZ_EPS) {
	    continue;
      }

      if (fabs(pt_center[0] - params->x_center) > params->box_radius || 
	  fabs(pt_center[1] - params->y_center) > params->box_radius) {
	    continue;
      }

      DEBUG(1, printf("  c = %.3f %.3f %.3f\n", pt_center[0], 
		   pt_center[1], pt_center[2]));

      /* Here, we use the projection matrix which removes the component
       * of a vector along the normal to compute the radial projection 
       * to the center point.  The projection matrix is
       *     P = I3 - n*n^T
       * an the radial projection is computed as
       *     r = P*v = (I3 - n*n^T)*v
       * However, the radial projections is computed like
       *     r = v - n*(n^T*v)
       * which eliminates the need to compute P explicitly and eliminates
       * matrix/vector multiplication, hence, significantly reducing
       * the number of scalar multiplications.
       */

      double dot_p = n[0]*pt_center[0] + n[1]*pt_center[1] + n[2]*pt_center[2];
      rc[0] = pt_center[0] - n[0]*dot_p;
      rc[1] = pt_center[1] - n[1]*dot_p;
      rc[2] = pt_center[2] - n[2]*dot_p;

      DEBUG(3, printf("  rc: %.3f %.3f %.3f\n", rc[0], rc[1], rc[2]));

      /* since we don't know how big a window we need to search in order
       * to find points within the desired radius, start with a small 
       * window and keep getting larger until none of the points in the
       * window are within the radius
       */

      /* compute window bounds */
      x0 = MAX(x_current - max_window,0);
      x1 = MIN(x_current + max_window, num_cols-1);
      y0 = MAX(y_current - max_window,0);
      y1 = MIN(y_current + max_window, num_rows-1);
      int num_close_points = 0;
    
      for(yi = y0; yi <= y1; yi++)
      {
          for(xi = x0; xi <= x1; xi++) {  
              int found = process_point(params, xi, yi, num_cols, xyz, n, rc, rz_coords, pt_center, &num_close_points);
          }
      }

      if (num_close_points >= params->min_close_points) {
          compute_output(index, params, output, rz_coords, num_close_points);
      }
    }
      
    for (i = 0; i < 2; i++) {
       free(rz_coords[i]);
    }
  }

  return 0;
}

