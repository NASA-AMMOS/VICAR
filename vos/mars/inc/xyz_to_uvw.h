/**********************************************************************
 *
 * xyz_to_uvw.h - code for computing surface normals 
 *
 * Chris Leger, JPL, 2003 
 *
 **********************************************************************/
#ifndef INCxyz_to_uvw_h
#define INCxyz_to_uvw_h

#define XYZ_EPS (1e-6)

/**********************************************************************
 * 
 * XyzToUvwParams - parameters governing behavior of surface normal (uvw)
 * generation
 *
 **********************************************************************/
typedef struct {
  double max_point_separation;  /* maximum distance in meters between
				 * point of interest and points to use
				 * in plane fit; negative indicates no
				 * limit 
				 */
  double max_plane_error;       /* maximum plane fit error in meters
				 * (average distance from points to 
				 * plane); negative indicates no limit
				 */
  int min_num_points;           /* minimum number of points used in the 
				 * plane fit; must be >= 3 (6 or more
				 * is recommended)
				 */
  int window_radius;            /* The number of pixels to consider around
				 * the pixel of interest (e.g. 
				 * window_radius of 2 would yield a 5x5
				 * window)
				 */
  double rejection_ratio;       /* reject points with plane fit error
				 * greater than this ratio times the 
				 * average error.  2 is recommended
				 */
  double x_center, y_center;    /* center of bounding box.  points 
				 * lying outside the box will 
				 * not have normals computed for them.
				 */
  double box_radius;            /* half-width of bounding box.
				 */
  double flip_threshold;	/* Threshold for Z component above which the
				 * vector is inverted.  Helps prevent noise
				 * from inverting vectors.  A value > 1.0
				 * effectively disables the sign flipping.
				 */
  double flip_distance;		/* Min distance from camera for sign flipping.
				 */
  int slope_mode;		/* If true, set "slope" mode, which dynamically
				 * adjusts window_radius and a step size based
				 * on the distance of the XYZ point from the
				 * origin.  The step size skips over every 2nd,
				 * 3rd etc pixel close in.  Both are for
				 * efficiency.  It is recommended to set
				 * box_radius to something huge.  Slope mode
				 * also prevents the loop that rejects points
				 * until max_plane_error is reached: only one
				 * iteration is performed.
				 */
} XyzToUvwParams;

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
	       int num_columns,          /* number of columns in xyz image */
	       double camera[3],         /* location of camera */
	       double *uvw[3]);          /* output array */

#endif /* INCxyz_to_uvw_h */
