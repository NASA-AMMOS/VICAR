/**********************************************************************
 *
 * xyz_to_uvw.c - code for computing surface normals 
 *
 * Chris Leger, JPL, 2003 
 *
 **********************************************************************/
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "xyz_to_uvw.h"
#include "zvproto.h"

/**********************************************************************
 *
 * Variables and constants
 *
 **********************************************************************/

#define SQ(x) ((x)*(x))

/**********************************************************************
 *
 * Function prototypes
 *
 **********************************************************************/
static int compute_eigenvectors(double m[3][3], double xc, double yc, double zc,
			       double n[3]);
static int make_covariance_matrix(int np, double com[3], double cov[3][3],
				  double *pt_list);
static double compute_error(int np, double com[3], double n[3],
			    double *pt_list);
static int remove_outliers(int np, double error, XyzToUvwParams *params,
			   double *pt_list);


/**********************************************************************
 *
 * Function definitions
 *
 **********************************************************************/

/***********************************************************************
 *
 * xyz_to_uvw  -  computes surface normal (uvw) image from 3D (xyz) image.
 *
 * Images are stored by row (e.g. row0, row1, row2...), with each band
 * as a separate array.  The images are passed as 3-element arrays of 
 * pointers to the image bands.
 *
 * xyz and uvw points with all three coordinates equal to zero are
 * considered invalid.  Units for xyz coordinates are meters.
 *
 * The camera location is needed to select an outward-pointing normal
 * (since the camera's location is known to be outside the surface).
 *
 * returns 0 on success and -1 on failure.
 * 
 **********************************************************************/
int xyz_to_uvw(XyzToUvwParams *params,   /* parameters */
	       double *xyz[3],           /* arrays of x, y, z points */
	       int num_rows,             /* number of rows in xyz image */
	       int num_cols,             /* number of columns in xyz image */
	       double camera[3],         /* camera location */
	       double *uvw[3]) {         /* output array */

  double max_sep_sq;
  int max_points;
  double flip_distance_sq;		/* square of flip_distance */

  /* check parameters */
  if (params->window_radius < 1) {
    fprintf(stderr, "Bad window_radius %d; setting to 3\n", 
	    params->window_radius);
    params->window_radius = 3;
  }
  if (params->min_num_points < 3) {
    fprintf(stderr, "Bad min_num_points %d; setting to 3\n", 
	    params->min_num_points);
    params->min_num_points = 3;
  }

  /* clear normal map */
  memset(uvw[0], 0, num_rows*num_cols*sizeof(double));
  memset(uvw[1], 0, num_rows*num_cols*sizeof(double));
  memset(uvw[2], 0, num_rows*num_cols*sizeof(double));

  /* allocate memory */
  max_points = SQ(params->window_radius*2+1);

  if (params->max_point_separation < 0) {
    max_sep_sq = 0;
  } else {
    max_sep_sq = SQ(params->max_point_separation);
  }

  flip_distance_sq = SQ(params->flip_distance);

  /* loop over points in image, ignoring border of window_radius pixels
   * since we won't be able to center a window in those regions.  (Note 
   * that we've already zeroed the uvw arrays, so we don't have to do
   * anything additional for the border pixels)
   */

  int omp_on = zvptst("OMP_ON");
  int error_status = 0;

#pragma omp parallel for schedule(dynamic) if (omp_on)
  for (int y_current = 0; y_current < num_rows; y_current++) {

    double error = 0;                      /* plane fit error */
    double pt[3];                          /* center point */
    double pt2[3];                         /* other point */
    double n[3];                           /* computed normal */
    double pt_temp[3];
    double com[3];                         /* center of plane */
    double cov[3][3];                      /* covariance matrix for points */
    int window_radius;			   /* dynamically adjusted */
    int step = 1;
    int x0, y0, x1, y1;                    /* window boundaries */
    int np;                                /* number of points in plane fit */
    int diff;
    register int xi, yi;                   /* loop variables for window */
    register int offset;                   /* offset into image */
    double dist_sq;			/* square of camera->point distance */

    if (y_current % 100 == 0) {
      char msg[256];
      snprintf(msg, 256, "line %d", y_current);
      zvmessage(msg, "");
    }

    /* list of points */
    double *pt_list = 0;

    /* x, y, z, and error for each point */
    pt_list = (double *)malloc(max_points*4*sizeof(double));
    memset(pt_list, 0, max_points*4*sizeof(double));

    if (!pt_list) {
      error_status = -1;
      continue;
    }

    for (int x_current = 0; x_current < num_cols; x_current++) {

      pt[0] = xyz[0][y_current*num_cols + x_current];
      pt[1] = xyz[1][y_current*num_cols + x_current];
      pt[2] = xyz[2][y_current*num_cols + x_current];

      /* skip this point if it's invalid (all zeroes) */
      if (fabs(pt[0]) < XYZ_EPS && 
	  fabs(pt[1]) < XYZ_EPS && 
	  fabs(pt[2]) < XYZ_EPS) {
	continue;
      }
      if (fabs(pt[0] - params->x_center) > params->box_radius || 
	  fabs(pt[1] - params->y_center) > params->box_radius) {
	continue;
      }

      /* If in "slope" mode, adjust the window radius and step size.  These
       * are both for efficiency.  As we move farther away, the pixel scale
       * increases, so we don't have to look at as many pixels to find all the
       * points within the max_point_separation threshold.  Thus, window_radius
       * shrinks.  And, since in "slope" mode we have a very large separation
       * threshold, we don't really need to look at every pixel close in...
       * thus "step" increases the closer we get.
       *
       * The distance scale and reduction factors are arbitrary and were
       * tuned for the MER pancam.  TBD: parameterize this somehow.
       *
       * If not in slope mode, the window radius is constant, and the step
       * size is 1.
       */

      dist_sq=SQ(pt[0]-camera[0]) + SQ(pt[1]-camera[1]) + SQ(pt[2]-camera[2]);

      if (params->slope_mode) {

	if (dist_sq < SQ(10.0)) {
	  step = 4;
	  window_radius = params->window_radius;
	} else if (dist_sq < SQ(15.0)) {
	  step = 3;
	  window_radius = params->window_radius*0.75;
	} else if (dist_sq < SQ(20.0)) {
	  step = 2;
	  window_radius = params->window_radius*0.66;
	} else {
	  step = 1;
	  window_radius = params->window_radius*0.5;
	}
      }
      else {			/* not slope mode */
	step = 1;
	window_radius = params->window_radius;
      }

      /* compute window bounds.  Note the window gets "squished" up against
       * the edge of the image.
       */
      x0 = x_current - window_radius;
      x1 = x_current + window_radius;
      y0 = y_current - window_radius;
      y1 = y_current + window_radius;
      if (x0 < 0) x0 = 0;
      if (x1 > num_cols-1) x1 = num_cols-1;
      if (y0 < 0) y0 = 0;
      if (y1 > num_rows-1) y1 = num_rows-1;

      np = 0;

      /* Ensure that the central pixel is covered */

      diff = x_current - x0;
      if (diff < 0) diff = 0;
      x0 += diff % step;

      diff = y_current - y0;
      if (diff < 0) diff = 0;
      y0 += diff % step;

      /* add all points in window that are within the distance threshold */
      for (yi = y0; yi <= y1; yi += step) {
	offset = yi*num_cols + x0;
	for (xi = x0; xi <= x1; xi += step, offset += step) {
	  /* copy other point into pt2 */
	  pt2[0] = xyz[0][offset];
	  pt2[1] = xyz[1][offset];
	  pt2[2] = xyz[2][offset];

	  /* skip point if it's invalid */
	  if (fabs(pt2[0]) < XYZ_EPS && 
	      fabs(pt2[1]) < XYZ_EPS && 
	      fabs(pt2[2]) < XYZ_EPS) {
	    continue;
	  }

	  /* add point to list if it's within the distance threshold,
	   * or if the distance threshold is disabled
	   */
	  pt_temp[0] = pt2[0] - pt[0];
	  pt_temp[1] = pt2[1] - pt[1];
	  pt_temp[2] = pt2[2] - pt[2];
	  if (max_sep_sq <= 0 ||
	      (SQ(pt_temp[0]) + SQ(pt_temp[1]) + SQ(pt_temp[2])) < 
	      max_sep_sq) {
	    /* copy this point into the list */
	    memcpy(pt_list+np*4, pt_temp, 3*sizeof(double));
	    np++;
	  }
	}
      }
      
      do {
	make_covariance_matrix(np, com, cov, pt_list);
	if (compute_eigenvectors(cov, com[0], com[1], com[2], n)) {
	  error = compute_error(np, com, n, pt_list);

	  if (error > params->max_plane_error) {
	    np = remove_outliers(np, error, params, pt_list);
	  }
	} else {
	  /* couldn't compute eigenvectors; set n and np to zero
	   * so we exit the loop
	   */
	  n[0] = 0;
	  n[1] = 0;
	  n[2] = 0;
	  np = 0;
	}
      } while (np >= params->min_num_points && 
	       params->max_plane_error > 0 && 
	       error > params->max_plane_error &&
	       !params->slope_mode);

      if (np >= params->min_num_points && 
	  (error <= params->max_plane_error || params->max_plane_error <= 0)) {

	/* found a valid normal; copy it */

	/* compute vector from point to camera */
	pt_temp[0] = camera[0] - pt[0];
	pt_temp[1] = camera[1] - pt[1];
	pt_temp[2] = camera[2] - pt[2];

	/* take dot product of point-to-camera vector and normal; should
	 * have same sign for outward-pointing normals.  If not, store
	 * negated normal.
	 */
	
	if (pt_temp[0]*n[0] + pt_temp[1]*n[1] + pt_temp[2]*n[2] >= 0) {
	  /* normal points outward; use it as-is */
	  uvw[0][y_current*num_cols + x_current] = n[0];
	  uvw[1][y_current*num_cols + x_current] = n[1];
	  uvw[2][y_current*num_cols + x_current] = n[2];
	} else {
	  /* normal points inward; use its negation */
	  uvw[0][y_current*num_cols + x_current] = -n[0];
	  uvw[1][y_current*num_cols + x_current] = -n[1];
	  uvw[2][y_current*num_cols + x_current] = -n[2];
	}

	/* Check for Z component being over the threshold, and invert the
	 * vector if so.  This prevents noise from giving us inverted vectors
	 * when looking at a plane in the distance almost parallel to the
	 * look vector.  The side effect is that it effectively prevents
	 * nearly-horizontal overhangs from being processed correctly.
	 * We only do this if the distance is greater than the flip_distance.
	 * Added by rgd 2/15/05.
	 */

	if ((dist_sq > flip_distance_sq) &&
	    (uvw[2][y_current*num_cols + x_current] > params->flip_threshold)) {
	  uvw[0][y_current*num_cols + x_current] = - uvw[0][y_current*num_cols + x_current];
	  uvw[1][y_current*num_cols + x_current] = - uvw[1][y_current*num_cols + x_current];
	  uvw[2][y_current*num_cols + x_current] = - uvw[2][y_current*num_cols + x_current];
	}

      }
    }
    free(pt_list);
  }

  if (error_status != 0) {
      fprintf(stderr, "Failed to allocate memory.\n");
      return error_status;
  }
  return 0;
}

int make_covariance_matrix(int np, double com[3], double cov[3][3],
			   double *pt_list) {
  register int i;
  register double *pt, dx, dy, dz;
  double npInv;

  npInv = 1.0/np;

  /* first, compute center */
  memset(com, 0, 3*sizeof(double));
  for (i = 0, pt = pt_list; i < np; i++, pt += 4) {
    com[0] += pt[0];
    com[1] += pt[1];
    com[2] += pt[2];
  }
  com[0] *= npInv;
  com[1] *= npInv;
  com[2] *= npInv;

  /* next, compute convariance matrix */
  memset(cov, 0, 9*sizeof(double));
  for (i = 0, pt = pt_list; i < np; i++, pt += 4) {
    dx = pt[0] - com[0];
    dy = pt[1] - com[1];
    dz = pt[2] - com[2];
    cov[0][0] += dx*dx;
    cov[0][1] += dx*dy;
    cov[0][2] += dx*dz;
    cov[1][1] += dy*dy;
    cov[1][2] += dy*dz;
    cov[2][2] += dz*dz;
  }
  cov[1][0] = cov[0][1];
  cov[2][0] = cov[0][2];
  cov[2][1] = cov[1][2];
  
  return 0;
}

double compute_error(int np, double com[3], double n[3], double *pt_list) {
  register int i;
  register double *pt, dx, dy, dz;
  double error;

  error = 0;
  for (i = 0, pt = pt_list; i < np; i++, pt += 4) {
    dx = pt[0] - com[0];
    dy = pt[1] - com[1];
    dz = pt[2] - com[2];
    pt[3] = fabs(dx*n[0] + dy*n[1] + dz*n[2]);
    error += pt[3];
  }
  error /= np;
  return error;
}

int error_cmp(const void *p1, const void *p2) {
  double *fp1 = (double *)p1;
  double *fp2 = (double *)p2;

  if (fp1[3] < fp2[3]) return -1;
  if (fp1[3] > fp2[3]) return 1;
  return 0;
}

int remove_outliers(int np, double error, XyzToUvwParams *params,
		    double *pt_list) {
  qsort(pt_list, np, 4*sizeof(double), error_cmp);

  /* always get rid of worst point */
  np--;

  if (np <= params->min_num_points) return np;

  /* keep throwing out points w/ error > rejection_ratio*average */
  while (np > 0 && pt_list[(np-1)*4+3] >= error*params->rejection_ratio) {
    np--;
  }

  return np;
}

#define ROTATE(a,i,j,k,l) g=a[i][j];h=a[k][l];a[i][j]=g-s*(h+g*tau);\
	a[k][l]=h+s*(g-h*tau);

int jacobi(double a[3][3], double d[], double v[3][3], int *nrot) {
  register int j, iq, ip, i;
  double tresh,theta,tau,t,sm,s,h,g,c;
  double b[3], z[3];

  for (ip = 0; ip < 3; ip++) {
    for (iq = 0; iq < 3; iq++) {
      v[ip][iq]=0.0;
    }
    v[ip][ip]=1.0;
  }
  for (ip = 0; ip < 3; ip++) {
    b[ip]=d[ip]=a[ip][ip];
    z[ip]=0.0;
  }
  *nrot=0;
  for (i = 0; i < 50; i++) {
    sm = 0.0;
    for (ip = 0; ip < 3-1; ip++) {
      for (iq = ip+1; iq < 3; iq++) {
	sm += fabs(a[ip][iq]);
      }
    }
    if (sm == 0.0) {
      return 1;
    }
    if (i < 3) {
      tresh = 0.2*sm/(3*3);
    } else {
      tresh = 0.0;
    }
    for (ip = 0; ip < 3-1; ip++) {
      for (iq = ip+1; iq < 3; iq++) {
	g = 100.0*fabs(a[ip][iq]);
	if (i > 4 && fabs(d[ip])+g == fabs(d[ip])
	    && fabs(d[iq])+g == fabs(d[iq])) {
	  a[ip][iq]=0.0;
	} else if (fabs(a[ip][iq]) > tresh) {
	  h = d[iq]-d[ip];
	  if (fabs(h)+g == fabs(h)) {
	    t = (a[ip][iq])/h;
	  } else {
	    theta = 0.5*h/(a[ip][iq]);
	    t = 1.0/(fabs(theta)+sqrt(1.0+theta*theta));
	    if (theta < 0.0) t = -t;
	  }
	  c = 1.0/sqrt(1+t*t);
	  s = t*c;
	  tau = s/(1.0+c);
	  h = t*a[ip][iq];
	  z[ip] -= h;
	  z[iq] += h;
	  d[ip] -= h;
	  d[iq] += h;
	  a[ip][iq] = 0.0;
	  for (j=0; j <= ip-1; j++) {
	    ROTATE(a,j,ip,j,iq);
	  }
	  for (j = ip+1; j <= iq-1; j++) {
	    ROTATE(a,ip,j,j,iq);
	  }
	  for (j = iq+1; j < 3; j++) {
	    ROTATE(a,ip,j,iq,j);
	  }
	  for (j = 0; j < 3; j++) {
	    ROTATE(v,j,ip,j,iq);
	  }
	  ++(*nrot);
	}
      }
    }
    for (ip = 0; ip < 3; ip++) {
      b[ip] += z[ip];
      d[ip] = b[ip];
      z[ip] = 0.0;
    }
  }
  return 0;
}

int get_normal_from_eigenvectors(double val[3], double vec[3][3], double n[3]) {
  int sm_ev, md_ev, lg_ev;
  double mag;

  if (fabs(val[0])<=fabs(val[1])) {
    if (fabs(val[0]) <= fabs(val[2])) sm_ev = 0;
    else sm_ev = 2;
  } else {
    if (fabs(val[1])<=fabs(val[2])) sm_ev = 1;
    else sm_ev = 2;
  }
  if (fabs(val[0])>fabs(val[1])) {
    if (fabs(val[0]) > fabs(val[2])) lg_ev = 0;
    else lg_ev = 2;
  } else {
    if (fabs(val[1])>fabs(val[2])) lg_ev = 1;
    else lg_ev = 2;
  }
  
  if (sm_ev == 0 || lg_ev == 0) {
    if (sm_ev == 1 || lg_ev == 1) md_ev = 2;
    else md_ev = 1;
  } else md_ev = 0;

  if (fabs(val[lg_ev]) < 1e-4) return 0;
  if (fabs(val[sm_ev]/val[md_ev]) > 0.7 &&
      fabs(val[md_ev]/val[lg_ev]) < 0.4) return 0;

  /* set normal */

  if(vec[2][sm_ev] > 0) {
    n[0] = -vec[0][sm_ev];
    n[1] = -vec[1][sm_ev];
    n[2] = -vec[2][sm_ev];
  } else {
    n[0] = vec[0][sm_ev];
    n[1] = vec[1][sm_ev];
    n[2] = vec[2][sm_ev];
  }

  mag = sqrt(n[0]*n[0] + n[1]*n[1] + n[2]*n[2]);
  if (fabs(mag) > 0.99) {
    /* magnitude should be very close to 1.0 already */
    n[0] /= mag;
    n[1] /= mag;
    n[2] /= mag;
  } else {
    /* something seriously bad happened */
    return 0;
  }

  return 1;
}

int compute_eigenvectors(double m[3][3], double xc, double yc, double zc,
			double n[3]) {
  double diag[3];
  int j_rotations, i;
  double evtemp[3][3];

  for (i = 0; i < 3; i++) {
    diag[i] = 0.0; 
  }

  if (!jacobi(m, diag, evtemp, &j_rotations)) return 0;

  return get_normal_from_eigenvectors(diag, evtemp, n);
}

