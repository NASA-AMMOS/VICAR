/**********************************************************************
 *
 * xyz_uvw_to_roughness.h - code for computing surface roughness
 *
 * Chris Leger, JPL, 2003 
 *
 **********************************************************************/
#ifndef INCxyz_uvw_to_roughness_h
#define INCxyz_uvw_to_roughness_h

#if 0
#include "xyz_to_uvw.h"  /* for some constants */
#endif
#define XYZ_EPS (1e-6)

/**********************************************************************
 * 
 * XyzUvwToRoughnessParams - parameters governing behavior of surface 
 * roughness calculation
 *
 **********************************************************************/
typedef struct {
  double outer_radius;    /* radius over which to consider points in 
			   * roughness calculations.  
			   */
  double inner_radius;    /* radius over which to consider points in 
			   * roughness calculations.  
			   */
  double max_roughness;   /* upper limit on roughness values; if a 
			   * valid roughness exceeds this value, it will
			   * be clipped.  roughness is defined as 
			   * peak-to-peak deviation from local plane.
			   */
  double bad_roughness;   /* value to use for lack of roughness data.  Must
			   * be >= max_roughness.
			   */
  int max_window_size;    /* Conservative guess at maximum window
			   * size which points falling into the radius
			   * will be found.  This is used to prevent
			   * infinite loops, and a zero or negative value
			   * will cause a default of 
			   * MIN(num_rows, num_cols)/8 to be used (which
			   * should be acceptable given the spatial 
			   * resolution of the stereo data relative to the
			   * radius).
			   */
  int min_close_points;   /* minimum number of points used in the 
			   * plane fit.  Should be greater than or equal
			   * to XyzToUvwParams::min_num_points; a value of 
			   * 6 or more is recommended.
			   */
  double x_center, y_center;    /* center of bounding box.  points 
				 * lying outside the box will 
				 * not have normals computed for them.
				 */
  double box_radius;            /* half-width of bounding box. */
} XyzUvwToRoughnessParams;

/***********************************************************************
 *
 * xyz_uvw_to_roughness  -  computes roughness map from point (xyz) and
 * normal (uvw) images.  Roughness is defined as the peak-to-peak 
 * deviation from the local plane.
 * 
 *
 * Images are stored by row (e.g. row0, row1, row2...), with each band
 * as a separate array.  The xyz and uvw images are passed as 
 * 3-element arrays of pointers to the image bands.
 *
 * xyz and uvw points with all three coordinates equal to zero are
 * considered invalid.  Units for xyz coordinates and roughness are 
 * meters.  
 *
 * returns 0 on success and -1 on failure.
 * 
 **********************************************************************/
int xyz_uvw_to_roughness(XyzUvwToRoughnessParams *params,   /* parameters */
			 double *xyz[3], /* arrays of x, y, z points */
			 double *uvw[3], /* arrays of u, v, w points */
			 int num_rows,   /* number of rows in images */
			 int num_cols,   /* number of columns in images */
			 double *roughness);         /* output array */

#endif /* INCxyz_uvw_to_roughness_h */
