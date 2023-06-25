// rng2oct.C 1.13 03/10/22 15:17:28
/** \file
* This function converts an XYZ image, such as from a range map,
* into a grape SfcModel (octree+mesh) object. An alternate interface
* adds voxels from the XYZ map to an existing octree (without meshing).
* (see range2oct.C for sample application).
*
* Range and RGB input data are assumed to be left-handed (Y=0 at top).
* RGB data is ignored if it's not at the same resolution as range data,
* except for the special case of a 1x1 image, which sets all voxels to
* the one pixel's color.
*/

#include <stdio.h>
#include <float.h>
#include <math.h>
#include "image/geoimage.h"
#include "grape/sfcmodel.h"
#include "grape/vector_ops.h"
#include "summitt_func.h"

extern int verbose;
static double mdlcam[3];		// camera position in model space
static double *edge_matrix;  		// cell edge length at each row/column,
					// -1 for invalid cell
static Triangle_Model *tm;		// Optional triangle mesh
static ImageData *xyzmap;		// XYZ range map
static int xres, yres;			// range image resolution
static ZMatrix camxform;		// transform world->camera frame
static double min_cos_norm;
static double face_threshold;
static int xfcam;			// apply camxform?

// Max ratio of longest triangle edge to sum of shorter edges.
// Triangles exceeding this ratio are candidate slivers
double sliver_ratio = 0.95;

// Max cosine of angle between long triangle edge and vector to eye.
// Triangles exceeding this cosine (i.e. edges within 1 degree of eye vector)
// are discarded as slivers
double sliver_cos = 0.9998;

int sliver_count;			// diagnostic (sliver rejects)
int angle_count;			// diagnostic (angle rejects)

// Extract one point from range XYZ data, return zero if invalid.
// Optionally transforms to camera frame.
// i = column (0=left), j=row (0=top)
static int getxyz(int i, int j, double *xyz)
{
	// skip if already marked invalid
	if (edge_matrix[j*xres + i] < 0.0)
		return 0;

	double tmp[3];
	double *p = xfcam ? tmp : xyz;
	p[0] = xyzmap->get_float(i, j, 0);
	if (p[0] < -90000.0)	// large negative X means invalid
		return 0;
	p[1] = xyzmap->get_float(i, j, 1);
	p[2] = xyzmap->get_float(i, j, 2);
	// all zeros means invalid
	if (p[0]==0.0 && p[1]==0.0 && p[2]==0.0)
		return 0;

	if (xfcam) {		// transform to camera frame
		MultPoints(tmp, camxform, xyz);
		// points behind the camera are bogus
		if (xyz[0] < 0.0)
			return 0;
	}
	return 1;
}

// Compute normal for one triangle in XYZ image
// (2-1-3 = counterclockwise corner in left-handed image), accumulate
// for average cell normal. Return 1 if okay (all points are valid)
static int add_one_normal(int x1, int y1, int x2, int y2,
				int x3, int y3, int valid, double *norm)
{
	double p1[3], p2[3], p3[3], tnorm[3];

	if (!getxyz(x1, y1, p1) || !getxyz(x2, y2, p2) || !getxyz(x3, y3, p3))
		return 0;
	surface_normal(p1, p3, p2, tnorm);

	if (!valid) {		// first normal for this cell
		memcpy(norm, tnorm, sizeof(tnorm));

	// Accumulate, unless apparently a discontinuity. 
	} else if (fabs(dot_product(tnorm, norm)) > min_cos_norm) {
		vector_sum(norm, tnorm, norm);
	}

	return 1;
}

// Estimate surface normal for XYZ image at cell x, y by averaging
// normals of the 4 triangles using that cell (or fewer, at the edges
// and adjacent to invalid points).
static void range_normal(int x, int y, double *norm)
{
	int valid = 0;			// found a valid normal yet?

	if (x > 0 && y > 0)  		// upper left triangle
		valid += add_one_normal(x, y, x-1, y, x, y-1, valid, norm);
	if (x+1 < xres && y > 0) 	// upper right triangle
		valid += add_one_normal(x, y, x, y-1, x+1, y, valid, norm);
	if (x > 0 && y+1 < yres) 	// lower left triangle
		valid += add_one_normal(x, y, x, y+1, x-1, y, valid, norm);
	if (x+1 < xres && y+1 < yres) 	// lower right triangle
		valid += add_one_normal(x, y, x+1, y, x, y+1, valid, norm);

	if (valid) {		// normalize accumulated result
		normalize_vector(norm);
	} else {		// none valid, set default vertical normal
		norm[0] = norm[1] = 0.0;
		norm[2] = 1.0;
	}
}

// Add candidate triangle if normal vector is okay
// (indicating face is visible from eye point [camera])
// Note all calculations are in model (octree) space.
static int try_mesh(NodeSpec *v1, NodeSpec *v2, NodeSpec *v3)
{
	double to_eye[3];

	// Check for sliver triangle:
	// First, is longest edge close to sum of smaller edges?
	double p1[3], p2[3], p3[3], *pa, *pb;
	v1->get_global_center(p1);
	v2->get_global_center(p2);
	v3->get_global_center(p3);
	double len1 = distance(p1, p2);
	double len2 = distance(p2, p3);
	double len3 = distance(p3, p1);
	double ratio;
	if (len1 > len2) {
		if (len1 > len3) {	// 1 is longest
			ratio = len1 / (len2 + len3);
			pa = p1; pb = p2;
		} else {		// 3 is longest
			ratio = len3 / (len1 + len2);
			pa = p3; pb = p1;
		}
	} else if (len2 > len3) {	// 2 is longest
		ratio = len2 / (len1 + len3);
		pa = p2; pb = p3;
	} else {			// 3 is longest
		ratio = len3 / (len1 + len2);
		pa = p3; pb = p1;
	}
	if (ratio > sliver_ratio) {
		// have candidate (pa-pb); 
		// is edge nearly colinear with eye point?
		double edge_unit[3];
		get_vector(pa, pb, edge_unit);
		get_vector(pa, mdlcam, to_eye);
		if (fabs(dot_product(edge_unit, to_eye)) > sliver_cos) {
			sliver_count++;
			return FALSE;
		}
	} else {
		// get unit vector from any vertex to eye point
		get_vector(p1, mdlcam, to_eye);
	}

	// get angle between eye vector and triangle normal
	Mesh_Triangle *mt = new Mesh_Triangle(v1, v3, v2);
	double n[3];
	mt->tri_surface_normal(n);
	double cos_angle = dot_product(n, to_eye);
	if (cos_angle < face_threshold) {
		delete mt;
		angle_count++;
		return FALSE;
	}

	// okay, add triangle to the mesh
	tm->add_triangle(mt);
	return TRUE;
}

// Possibly add triangles to mesh for given set of four points.
// Invalid points have NULL nodespec pointers.
// ulns ----- urns
//   |         |
//   |         |
// llns ----- lrns
static void add_mesh(NodeSpec *ulns, NodeSpec *urns, NodeSpec *llns,
			NodeSpec *lrns)
{
	// count valid points
	int nvalid = (ulns != NULL) + (urns != NULL) + (llns != NULL) +
		(lrns != NULL);

	if (nvalid == 4) {		// all points valid, two triangles
		// try splitting on LL-UR diagonal
		nvalid = try_mesh(ulns, urns, llns) +
			 try_mesh(urns, lrns, llns);
		if (nvalid == 0) {	// no good, try UL-LR diagonal
			try_mesh(ulns, urns, lrns);
			try_mesh(ulns, lrns, llns);
		}

	} else if (nvalid == 3) {	// only one triangle
		if (ulns == NULL)
			try_mesh(urns, lrns, llns);
		else if (urns == NULL)
			try_mesh(ulns, lrns, llns);
		else if (llns == NULL)
			try_mesh(ulns, urns, lrns);
		else
			try_mesh(ulns, urns, llns);
	}
}

/// Convert XYZ map data to surface model
// Originally by K. Sturdevant 12-8-98
// If insfc is NULL (the normal case), create new surface model
// xyz = XYZ range map
// rgb = corresponding color (or grayscale) image
// cam = camera position; normally (0,0,0) if data is already in
// 	camera frame or will be transformed to camera frame
// fov is horizontal FOV in radians (for voxel edge length)
// if xcam flag is true, points are transformed using cxform to camera frame
SfcModel *range2octree(ImageData *xyz, ImageData *rgb, double cam[],
	float fov, float range_low, float range_high, 
	ZMatrix cxform, int flags, double mcnorm, double fthresh, 
	double zstretch, SfcModel *insfc)
{
	int	i, j;
	int	edge_index;
	double	min_x, max_x;
	double	min_y, max_y;
	double	min_z, max_z;
	double	min_range, max_range;
	double	min_edge, max_edge;
	double  scale;
	SfcModel *octree = insfc;

	// Save some arguments in local variables
	xyzmap = xyz;
	xfcam = flags & R2O_XCAM;
	if (xfcam && cxform)
		MatCopy(cxform, camxform);
	min_cos_norm = mcnorm;
	face_threshold = fthresh;
	sliver_count = angle_count = 0;

	// get range map size
	xyz->get_res(&xres, &yres);
	if (verbose)
		fprintf(stderr, "XYZ xres = %d, yres = %d\n", xres, yres);
	
	double pixfov = tan(fov/xres);   // Tan horiz fov for 1 pixel

	// allocate array to track previous row's nodes for mesh
	NodeSpec **prow = new NodeSpec *[xres];

	// Allocate space for edge storage
	int edge_size = xres*yres;
	edge_matrix = new double[edge_size];
	memset(edge_matrix, 0, edge_size * sizeof(double));
	
	// find data limits in image
	// Get the Z values to compute the depth, and then the level
	min_x = min_y = min_z = min_range = min_edge = FLT_MAX;
	max_x = max_y = max_z = max_range = max_edge = -FLT_MAX;

	int invalid_counter = 0;
	int minrange_counter = 0;
	int maxrange_counter = 0;

        for(j=0; j<yres; j++) {
		edge_index = j * xres;
		for(i=0; i<xres; i++, edge_index++) {
			double imgxyz[3]; 
			if (!getxyz(i, j, imgxyz)) {
			    invalid_counter++;
			    edge_matrix[edge_index] = -1.0;
			    continue;
			} else {
			    // Set edge here
			    double dcam[3];
			    vector_diff(cam, imgxyz, dcam);
			    double pt_range = vector_magnitude(dcam);

			   if (pt_range < range_low) {
			        minrange_counter++;
			        edge_matrix[edge_index] = -1.0;
			        continue;
			   } 
			   if (pt_range > range_high) {
			        maxrange_counter++;
			        edge_matrix[edge_index] = -1.0;
			        continue;
			   } 

			   // Set mins and maxes once we know the point is
			   // valid and within range

			   if(imgxyz[0] < min_x) min_x = imgxyz[0];
			   if(imgxyz[0] > max_x) max_x = imgxyz[0];

			   if(imgxyz[1] < min_y) min_y = imgxyz[1];
			   if(imgxyz[1] > max_y) max_y = imgxyz[1];

			   if(imgxyz[2] < min_z) min_z = imgxyz[2];
			   if(imgxyz[2] > max_z) max_z = imgxyz[2];

			    edge_matrix[edge_index] = pt_range*pixfov;

			    // The smallest range will indicate the smallest edge.
			    if (pt_range < min_range) {
				min_range = pt_range;
				min_edge = edge_matrix[edge_index];
			    }
			    if (pt_range > max_range) {
				max_range = pt_range;
				max_edge = edge_matrix[edge_index];
			    }
			}
		}
	}

	if (verbose) {
		fprintf(stderr, "range limits: %g to %g\n", 
			min_range, max_range);
		fprintf(stderr, "edge range is %g to %g\n", min_edge, max_edge);
		fprintf(stderr, "x range is %g to %g\n", min_x, max_x);
		fprintf(stderr, "y range is %g to %g\n", min_y, max_y);
		fprintf(stderr, "z range is %g to %g\n", min_z, max_z);
	}

	if (min_edge <= 0.0) {
		fprintf(stderr, "Min_edge must be greater than zero, is %f\n",
			min_edge);
		delete [] edge_matrix;
		delete [] prow;
		return NULL;
	}

    if (insfc == NULL) {	// create a new octree

	// Choose the largest of the difference between the x, y, z to 
	// determine the scale (largest dimension of the bounding box).
	scale = max_x - min_x;
	if ( (max_y - min_y) > scale)
		scale = max_y - min_y;
	if ( (max_z - min_z) > scale)
		scale = max_z - min_z;

	// If had just one point, scale could be 0 because max_x - min_x = 0.
	if (scale < max_edge)
		scale = max_edge;

	// Calculate the octree depth
	// 2^levels = depth
	// levels = log (Base 2) depth = log (depth) / log (2);
	// round up
	// Depth is the ratio of the largest boundary dimension to the smallest voxel
	double depth = scale/min_edge;

	// levels is the number of levels needed in the octree
	int levels = int(ceil( log(depth)/log(2.0) )) + 1;
	if (verbose)
		fprintf(stderr, "scale = %f, depth = %f, levels = %d\n", 
			scale, depth, levels);

	// In the case of 1 point, we could end up with a levels of 0, which
	// is impossible.  Therefore, make sure levels is at least 1.
	if (levels <= 0)
		levels = 1;

	// Create the octree/surface model
	octree = new SfcModel(levels);
    }
	tm = &(octree->mesh);

        NodeSpec *ns = NULL, *pns = NULL;
        double  cntr[OCTREE_DIMS];
	uchar red, green, blue;

	if (flags & R2O_GRID) {
	    // Add grid voxels for testing 
	    // Looping scale is just a smidge higher than 1 to allow for round-off
		
	    float x,y;
            int extra = 0;
	    blue = 0;
	    cntr[2] = 0;
	    
	    for (y=-1.0; y < 1.01; y+=0.1) {
		for (x=-1.0; x < 1.01; x+=0.1) {
        		ns = new NodeSpec();
			red = uchar((x+1.0)*127.4);
			green = uchar((y+1.0)*127.4);
        		ns->set_color(red, green, blue);
       			ns->edge_length = .02;

       			ns->use_alpha(TRUE);
			cntr[0] = x;
			cntr[1] = y;
			cntr[2] = 0;
                	ns->set_global_center(cntr);

	               	octree->add_voxel(ns);
			extra++;
		}
	    }

	    if (verbose)
		    fprintf(stderr, "Added %d grid voxels\n", extra);
	}

    if (insfc == NULL) {
	// Setup model-object transformation matrix
        octree->x = -(max_x + min_x)/scale;
        octree->y = -(max_y + min_y)/scale;
        octree->z = -(max_z + min_z) * zstretch / scale;
        octree->xrot = octree->yrot = octree->zrot = 0.0;
	scale *= 0.5;		// modify scale since volume ranges -1 to 1
	octree->xscale = octree->yscale = scale;
	octree->zscale = scale / zstretch;
    } else {	// extract existing scale factor
    	scale = octree->xscale;
    }

	// No more changes to transform parms, so precompute matrices
	// to speed up add_voxel()
	octree->freeze_xform();

	// map camera position from object coords to model space
	ZMatrix xform;
	octree->GetObjectToModelTransform(xform);
	MultPoints(cam, xform, mdlcam);

	// default color = red
	red = 255;
	green = blue = 0;

	// check that RGB is the same size as XYZ, or 1x1
	if (rgb) {
		int rx, ry;
		rgb->get_res(&rx, &ry);
		if (rx == 1 && ry == 1) {	// special 1-color case
			rgb->get_color(0, 0, &red, &green, &blue);
			rgb = NULL;		// don't query image any more
		} else if (rx != xres && ry != yres) {
			fprintf(stderr, "RGB size %d x %d doesn't match XYZ\n",
				rx, ry);
			rgb = NULL;	// ignore image data
		}
	}

	// Read through the range image and create voxels
	// Loop:  Read the range file one pixel at a time
	// for each pixel
	//    create a voxel
        //    get color and calculate edge

	int counter = 0;

	// Some xyz's will be "bad" and need to be tossed. kfs
        for(j=0; j<yres; j++) {
		edge_index = j * xres;
		if (xres>1) {
			prow[xres-2] = pns;
			prow[xres-1] = ns;
		}
		ns = NULL;

		for(i=0; i<xres; i++, edge_index++) {
			if (i>1)
				prow[i-2] = pns;
			pns = ns;

			// Skip invalid points
        		if (edge_matrix[edge_index] <= 0.0) {
				ns = NULL;
			} else {
        			ns = new NodeSpec();

				// Get color from rgb file, or default
				if (rgb) 
					rgb->get_color(i, j, 
							&red, &green, &blue);
        			ns->set_color(red, green, blue);
        			ns->use_alpha(TRUE);

				// Division is for scaling. 
        			ns->edge_length = edge_matrix[edge_index]/scale;

				getxyz(i, j, cntr);
    	   			ns->set_global_center(cntr);

				// compute voxel surface normal vector
				double norm[3];
				range_normal(i, j, norm);
				ns->set_normal(norm);

				octree->add_voxel(ns);
				counter++;
			}

			// update mesh, if not left column or top row
			if ((flags & R2O_MESH) && i>0 && j>0) 	
				add_mesh(prow[i-1], prow[i], pns, ns);

			// ** Add empties here
			// Empties will go from point to eye, getting smaller
			// as they get closer to the eye.
                }
        }

	if (flags & R2O_EYE) {
		// Add voxel to represent eye point
        	ns = new NodeSpec();
        	ns->set_color(0.0, 0.0, 1.0, 1.0);
        	ns->edge_length = .02;
        	ns->use_alpha(TRUE);
        	ns->set_global_center(cam);
		octree->add_voxel(ns);
		counter++;
	}

	// ** Condense when all done?
	//octree->condense();

	// clean up
	delete [] edge_matrix;
	delete [] prow;

	if (verbose) {
		fprintf(stderr, "Found %d invalid points\n", invalid_counter);
		fprintf(stderr, "Found %d <min-range, %d >max-range points\n", 
			minrange_counter, maxrange_counter);
		fprintf(stderr, "Discarded %d sliver triangles\n",
			sliver_count);
		fprintf(stderr, "Rejected %d triangles for bad direction\n",
			angle_count);
		fprintf(stderr, "Added total of %d points\n", counter);
	}

	// Return a pointer to the octree/surface model
	return( octree );
}
