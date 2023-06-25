/**********************************************************************
 *
 * xyz_uvw_to_roughness.c - code for computing surface roughness for RAT
 *
 * Chris Leger, JPL, 2003 
 *
 **********************************************************************/
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "xyz_uvw_to_roughness.h"

/**********************************************************************
 *
 * Macros
 *
 **********************************************************************/

#define MIN(a, b) ((a) < (b) ? (a) : (b))
#define MAX(a, b) ((a) > (b) ? (a) : (b))
#define SQ(x) ((x)*(x))

#if 0
int debug_level = 1;
#define DEBUG(val, foo) if (val <= debug_level) foo
#else
#define DEBUG(val, foo)
#endif

static  double d_outside_min;
static  double d_outside_max;
static  double d_inside_max;
static  double d_outside_min_2;
static  double d_outside_max_2;
static  double d_inside_max_2;
static  double n[3];                           /* computed normal */
static  int num_close_points;                  /* # of points w/in radius */
static  double P[3][3];                        /* projection matrix */
static  double rc[3];                          /* radial projection to center */
static  int exceeded_max;                      /* exceeded max roughness? */

/**********************************************************************
 *
 * Function definitions
 *
 **********************************************************************/

int process_point(XyzUvwToRoughnessParams *params,
		  int xi, int yi, int num_cols, double *xyz[3]) { 
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
    DEBUG(3, printf("skipped\n"));
    return 0;
  }
  
  /* compute the distance to the point along the normal */
  d = pt[0]*n[0] + pt[1]*n[1] + pt[2]*n[2];

  /* compute the radial vector */
  rv[0] = P[0][0]*pt[0] + P[0][1]*pt[1] + P[0][2]*pt[2];
  rv[1] = P[1][0]*pt[0] + P[1][1]*pt[1] + P[1][2]*pt[2];
  rv[2] = P[2][0]*pt[0] + P[2][1]*pt[1] + P[2][2]*pt[2];
  
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

  if (radius <= SQ(params->inner_radius)) {
    if (d > d_inside_max_2) d_inside_max_2 = d;
    else if (d > d_inside_max) d_inside_max = d;
    found_a_point = 1;
    num_close_points++;
  } else if (radius <= SQ(params->outer_radius)) {
    if (d < d_outside_min_2) d_outside_min_2 = d;
    else if (d < d_outside_min) d_outside_min = d;
    if (d > d_outside_max_2) d_outside_max_2 = d;
    else if (d > d_outside_max) d_outside_max = d;
    found_a_point = 1;
    num_close_points++;
  }

  return found_a_point;
}

int xyz_uvw_to_roughness(XyzUvwToRoughnessParams *params,   /* parameters */
			 double *xyz[3], /* arrays of x, y, z points */
			 double *uvw[3], /* arrays of u, v, w points */
			 int num_rows,   /* number of rows in images */
			 int num_cols,   /* number of columns in images */
			 double *roughness) {         /* output array */
  register int xi, yi;                   /* loop variables for window */
  int found_a_point;                     /* found any points in window? */
  double pt_center[3];                   /* center point */
  int x_current, y_current;              /* image coords of current point */
  int x0, y0, x1, y1;                    /* window boundaries */
  int lim;                               /* loop limit */
  int window_size;                       /* current window size */
  int max_window;                        /* conservative window max */
  double *xyz_filt[3];
  double d;
  int i;

  if (params->max_window_size > 0) {
    max_window = params->max_window_size;
  } else {
    max_window = MIN(num_rows, num_cols)/8;
  }

  /* clear roughness map */
  memset(roughness, 0, num_rows*num_cols*sizeof(double));

  /* allocate filtered subwindow */
  for (i = 0; i < 3; i++) {
    xyz_filt[i] = malloc(SQ(2*max_window+1)*sizeof(double));
  }
  /* loop over points in image, ignoring border of window_radius pixels
   * since we won't be able to center a window in those regions.  (Note 
   * that we've already zeroed the uvw arrays, so we don't have to do
   * anything additional for the border pixels)
   */

  for (y_current = 0; y_current < num_rows; y_current++) {

    if (y_current % 20 == 0) {
      printf("line %d\n", y_current);
    }

    for (x_current = 0; x_current < num_cols; x_current++) {

      /* set the roughnes to a bad value; we'll re-set it if
       * we're able to compute an actual value.
       */
      roughness[y_current*num_cols + x_current] = params->bad_roughness;

      /* get the normal for the center point. this check is done
       * first since it'll eliminate more invalid points then
       * checking the xyz point (not all xyz points have a 
       * defined normal).
       */
      n[0] = uvw[0][y_current*num_cols + x_current];
      n[1] = uvw[1][y_current*num_cols + x_current];
      n[2] = uvw[2][y_current*num_cols + x_current];

      /* skip this point if it's invalid (all zeroes) */
      if (fabs(n[0]) < XYZ_EPS && 
	  fabs(n[1]) < XYZ_EPS && 
	  fabs(n[2]) < XYZ_EPS) {
	continue;
      }

      /* get the center point */
      pt_center[0] = xyz[0][y_current*num_cols + x_current];
      pt_center[1] = xyz[1][y_current*num_cols + x_current];
      pt_center[2] = xyz[2][y_current*num_cols + x_current];

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

      /* compute the projection matrix which removes the component
       * of a vector along the normal.  The matrix is defined as:
       * Ident3 - n*transpose(n), where n is a column vector and
       * Ident3 is the 3x3 identity matrix.
       */
      P[0][0] = 1-n[0]*n[0];
      P[1][1] = 1-n[1]*n[1];
      P[2][2] = 1-n[2]*n[2];
      P[0][1] = P[1][0] = -n[0]*n[1];
      P[0][2] = P[2][0] = -n[0]*n[2];
      P[1][2] = P[2][1] = -n[1]*n[2];

      DEBUG(3, printf("  P: %.3f %.3f %.3f\n", P[0][0], P[0][1], P[0][2]));
      DEBUG(3, printf("     %.3f %.3f %.3f\n", P[1][0], P[1][1], P[1][2]));
      DEBUG(3, printf("     %.3f %.3f %.3f\n", P[2][0], P[2][1], P[2][2]));

      /* compute the radial projection to the center point, which will
       * save us some computations later 
       */

      rc[0] = 
	P[0][0]*pt_center[0] + P[0][1]*pt_center[1] + P[0][2]*pt_center[2];
      rc[1] = 
	P[1][0]*pt_center[0] + P[1][1]*pt_center[1] + P[1][2]*pt_center[2];
      rc[2] = 
	P[2][0]*pt_center[0] + P[2][1]*pt_center[1] + P[2][2]*pt_center[2];

      DEBUG(3, printf("  rc: %.3f %.3f %.3f\n", rc[0], rc[1], rc[2]));

      d_inside_max = -1e12;
      d_outside_min = 1e12;
      d_outside_max = -1e12;
      d_inside_max_2 = -1e12;
      d_outside_min_2 = 1e12;
      d_outside_max_2 = -1e12;

      /* since we don't know how big a window we need to search in order
       * to find points within the desired radius, start with a small 
       * window and keep getting larger until none of the points in the
       * window are within the radius
       */

      /* clear the filtered points */
      for (i = 0; i < 3; i++) {
	memset(xyz_filt[i], 0, SQ(2*max_window+1)*sizeof(double));
      }

      for (window_size = 1, found_a_point = 0, num_close_points = 0; 
	   !exceeded_max &&
	     (found_a_point || !num_close_points) 
	     && window_size < max_window; 
	   window_size++) {
        found_a_point = 0;

	DEBUG(2, printf("  ----- window = %d -----\n", window_size));

	/* compute window bounds */
	x0 = x_current - window_size;
	x1 = x_current + window_size;
	y0 = y_current - window_size;
	y1 = y_current + window_size;

	/* search upper border */
	if (y0 >= 0) {
	  lim = MIN(x1, num_cols-1);
	  yi = y0;
	  for (xi = MAX(x0, 0); xi <= lim; xi++) {
	    if (process_point(params, xi, yi, num_cols, xyz)) {
	      found_a_point = 1;
	    }
	  }
	}

	/* search lower border */
	if (y1 < num_rows) {
	  yi = y1;
	  lim = MIN(x1, num_cols-1);
	  for (xi = MAX(x0, 0); xi <= lim; xi++) {
	    if (process_point(params, xi, yi, num_cols, xyz)) {
	      found_a_point = 1;
	    }
	  }
	}

	/* search left border */
	/* Leave out both ends since they're caught in the top/bottom loops */
	if (x0 >= 0) {
	  xi = x0;
	  lim = MIN(y1, num_rows-1);
	  for (yi = MAX(0, y0) + 1; yi <= lim - 1; yi++) {
	    if (process_point(params, xi, yi, num_cols, xyz)) {
	      found_a_point = 1;
	    }
	  }
	}

	/* search right border */
	/* Leave out both ends since they're caught in the top/bottom loops */
	if (x1 < num_cols) {
	  xi = x1;
	  lim = MIN(y1, num_rows-1);
	  for (yi = MAX(0, y0) + 1; yi <= lim - 1; yi++) {
	    if (process_point(params, xi, yi, num_cols, xyz)) {
	      found_a_point = 1;
	    }
	  }
	}
      }

      if (num_close_points >= params->min_close_points) {
	DEBUG(1, printf("  range = %.3f %.3f %.3f\n",
			d_inside_max, d_outside_max, d_outside_min));
	d_inside_max -= d_outside_min;
	d_outside_max -= d_outside_min;
	
	d = MAX(d_inside_max, d_outside_max);
	if (fabs(d) > params->max_roughness) d = params->bad_roughness;
	roughness[y_current*num_cols + x_current] = d;

      }
      DEBUG(1, printf("  ROUGHNESS %d %d = %.3f  \n",
		      x_current, y_current, 
		      roughness[y_current*num_cols + x_current]));
    }
  }

  return 0;
}

