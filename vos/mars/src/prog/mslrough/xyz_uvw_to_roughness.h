/**********************************************************************
 *
*
 * xyz_uvw_to_roughness.c - code for computing surface roughness for
 *                          DRILL and DRT
 *
 * Matt Robinson, JPL, 2011 
 *                     based on MER roughness code by Chris Leger, 2003
 * 
 * Bob Crocco, 2020 updated for curvature based on code by Kris Wehage
 *                     and Curtis Collins, 2020
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

/*
Current implementation: 

     -------ring outer-------             
    / ++++++++++++++++++++++++\
   /++++----ring innner----++++\ 
  /+++/                     \+++\
 |+++|                       |+++|
 |+++|     -patch outer-     |+++|
 |+++|    / ........... \    |+++|
 |+++|   |.............. |   |+++|     


MSL mapping: the patch covers the ring:

     ---ring outer/patch---
   / ++++++++++++++++++++++++\
  /++++----ring innner----++++\ 
 /+++/.....................\+++\
|+++|.......................|+++|
|+++|.......................|+++|
|+++|.......................|+++|
|+++|.......................|+++|
*/
typedef struct {
  double patch_radius;    /* radius over which to consider points 
			   * in roughness calculations. covers area <= patch outer radius
			   */
  double ring_inner_radius;    /* defines the inner radius of the ring over
			   *  which to consider points in roughness calculations. can be 0
			   */
  double ring_outer_radius; /* radius that defines the outer radius of the ring
			   * over which to consider points in roughness calculations. must be
			   * greater than ring inner radius to perform the ring calculations
			   */

  double max_roughness_ring;/* upper limit on acceptable roughness values 
			   * in the outer ring, i.e. for points that fall
                           * between ring_inner_radius <= r <= ring_outer_radius
                           * values greater than max_roughness_ring
                           * are considered bad; roughness is defined as 
			   * peak-to-peak deviation from local plane.
			   */
  double max_roughness_patch;/* upper limit on acceptable roughness values 
			   * overall, i.e. for points that fall within
                           * the outer radius, r <= patch_radius
                           * values greater than max_roughness
                           * are considered bad; roughness is defined as 
			   * peak-to-peak deviation from local plane.
			   */

  double bad_roughness;   /* value to use for lack of roughness data.  Must
                           * be >= max_roughness_ring and max_roughness_patch.
                           */
  double filter_scale;    /* filter points with roughness >= filter*std,
                             where std is the standard deviation of all
                             points within patch_radius
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

  int do_curvature;      /* 0: don't do curvature, do roughness (default). 
                            1: do curvature, no roughness */

  double bad_curvature;  /* value to use for lack of curvature data.  Must
                           * be > max(high threshold concavity, high threshold convexity).
                           */

  double convexity_threshold_high;    /*---- curvature: convexity threshold high ---- */
  double convexity_threshold_low;     /*---- curvature: convexity threshold low  ---- */
                                      /*++++++++++++++   mean plane   +++++++++++++++ */
  double concavity_threshold_low;     /*---- curvature: concavity threshold low  ---- */
  double concavity_threshold_high;    /*---- curvature: concavity threshold high ---- */

  double sphere_cull_radiusSq;       /*optional additional point rejection based on 3d 
                                        sphere around point*/

} XyzUvwToRoughnessParams;

/***********************************************************************
 *
 * xyz_uvw_to_roughness  -  computes roughness map from point (xyz) image
 * and normal (uvw) image or vector.  If the input parameter uvw_is_matrix
 * is true, then uvw is an image (a matrix of vectors) with the same 
 * dimensions as the input xyz image.  If uvw_is_matrix is false, then 
 * uvw is just a single vector.  Roughness is defined as the peak-to-peak 
 * deviation from the local plane.
 * 
 * Roughness map has three bands:
 * Band 1: State
 *         0.0  no solution
 *         1.0 both ring and overall roughness are bad
 *         2.0 overall roughness is bad
 *         3.0 ring roughness is bad
 *         4.0 both overall and ring roughness are good
 * Band 2: Overall roughness, a value of 1.0 means no solution
 * Band 3: Ring roughness, a value of 1.0 means no solution
 * For DRT set inner radius = 0.0
 *
 *
 * Optionally the code can instead calculate curvature output
 * Band 1: Curvature state
 *          0.0 no solution
 *          1.0 both concavity and convexity worse than high threshold
 *          2.0 convexity worse than high threshold
 *          3.0 concavity worse than high threshold
 *          5.0 high threshold good
 *          6.0 low threshold good (most stringent test)
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
            int uvw_is_matrix, /* if true, uvw is a matrix of
                                            vectors with num_rows and
                                            num_cols dimensions like xyz.
                                            if false, uvw is a single vector. */
            double *output[3],  /* output array: can be roughness or curvature */
            int omp_on);     /* multithread or not */
			 

#endif /* INCxyz_uvw_to_roughness_h */
