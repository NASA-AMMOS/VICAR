//image_processing.h 
//Support functions for marsmesh program
//
#include<stdio.h> 
#include "SimpleImage.h"
#include "PigVector.h"

#include <mat3.h>
#include "grape/object.h"
#include "grape/sfcmodel.h"

#include "xyz_to_mesh.h"

////////////////////////////////////////////////////////////////////////

bool getPoint3D(SimpleImage<float> *image, int line, int sample, double *xyz);
bool setPoint3D(SimpleImage<float> *image, int line, int sample, double *xyz);
void image_resample(SimpleImage<float> *iimg, SimpleImage<float> *oimg);
void image_resample_X(SimpleImage<float> *iimg, SimpleImage<float> *oimg);
void range_normal(SimpleImage<float> *image, int x, int y, int xres, int yres, double *norm);

// Compute normal for one triangle in XYZ image
// (2-1-3 = counterclockwise corner in left-handed image), accumulate
// for average cell normal. Return 1 if okay (all points are valid)
int add_one_normal(SimpleImage<float> *image, int x1, int y1, int x2,
                                int y2, int x3, int y3, int valid, double *norm);

bool  mask_image_by_tile_resolution(SimpleImage<float> *inp_image, SimpleImage<int> *mask_image, int res_level, SimpleImage<float> *& out_image);
bool  copy_image(SimpleImage<float> *inp_image, SimpleImage<float> *& out_image);

/// Accumulate interpolation at specified map cell, if valid.
// wnew = distance from hole being interpolated
// Return 1 if valid cell
int acc3(SimpleImage<float> *inimg, int x, int y, double wnew, double acc[], double *wsum);

// Weighted interpolation of nearest non-empty cells to
// fill in an empty cell. Updates imap and returns true if successful.
// !!!! maybe should check that points are nearly coplanar? 
bool interpolate (SimpleImage<float> *iimg, SimpleImage<float> *tmpimg, int x, int y, int maxgap);

// interpolate across gaps in range image
void image_range_interpolate(SimpleImage<float> *inimg, int maxgap);

SfcModel* populate_octree(SimpleImage<float> *inimg, SfcModel *in_octree, long *num_polygons, double *range_out_min, double *range_out_max, bool create_and_init_octree_only, double c_pt[3], ZMatrix modelToWorld, double mdlcam[3], double pixfov, double max_angle, double range_low, double range_high);
SfcModel* init_octree(double max_x, double min_x, double max_y, double min_y, double max_z, double min_z, double max_edge, double min_edge, double zstretch);


/*
PigPoint ZconvertWorldToObjFrame(PigPoint* world_pt, PigPoint* pt_offset,
                                double rot[3][3]);
bool ZconvertWorldToObjFrame(double* world_pt, double* pt_offset,
                            double rot[3][3], double* cm_pt);
bool ZconvertObjToWorldFrame(double* cm_pt, double* pt_offset, double rot[3][3],
                            double* world_pt);
void Zcreate_camera_centric_frame(PigCAHV* camera_model, double rot[3][3]);
*/
