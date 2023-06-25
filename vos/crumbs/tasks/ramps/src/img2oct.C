// img2oct.C 1.19 02/08/07 15:13:59
/** \file
 * This utility converts an image file to an octree. 
 * The image is considered an elevation file for a 2.5D model.
 * K. Sturdevant 12-04-98  					
 * Based on img2vtk by John Wright  	
 *
 * Octree model space (including surface normals and mesh orientation)
 * is right-handed and scaled into the range -1:+1 in XYZ.
 * Object space corresponds to image pixel center coordinates, by default
 * flipped in Y to right-handed form: (0.5,0.5) at lower left,
 * but optionally in left-handed form with (0.5, 0.5) at upper left.
 * 
 */
#include <fstream.h>
#include <float.h>
#include <math.h>
#include <malloc.h>
#include "image/geoimage.h"
#include "summitt_func.h"

static const char usage[] = 
"Usage: %s [-v] [-t thickness] [-u] [-e] [-g] [-lh]\n"
"  [-c rgb_file] [-d discont] [-m] -i imgfile [-o outfile]\n\n"
"Creates octree from height image 'imgfile' to 'outfile' (default stdout).\n"
"If 'imgfile' is georefenced, a one-tree forest outfile.fst is also built.\n"
"-v = verbose output\n"
"-e = generate empty voxels\n"
"-g = generate grid voxels\n"
"-u = force uniform X/Y/Z scaling\n"
"-m = include triangle mesh in output\n"
"-lh = build left-handed object space ((0,0) is upper left)\n"
"thickness = height units/row-col units (default 1.0)\n"
"discont = min normal angle (deg) discarded as discontinuity (default=60)\n";

extern int verbose;

// Minimum cosine of angle between normal vectors. Smaller cosine
// (larger angle) indicates a discontinuity.
static double min_cos_norm = 0.5;	// default = 60 degrees

// Output object space left-handed? 
static int left_handed;

// Optional triangle mesh
static Triangle_Model *tm;

// input image resolution/size
static int xres, yres;

static char *progname;

// test to reject invalid height cells
#define VALID_Z(z)	((z) >= -90000.0)

int main(int argc, char **argv)
{
	GeoImage	*img=NULL, *rgb=NULL;
	int	i;
	char	*ifname=NULL, *ofname=NULL, *rgbfname=NULL;
	float vertscale=1.0;
	Octree  *oct;
	Octree * image2octree(GeoImage * img, GeoImage * rgb, float vertscale, int ufs, int genEmpties, int genGrid);

	int uniform = FALSE;
	int genEmpties = FALSE;
	int genGrid = FALSE;

	progname = argv[0];
	for(i=1; i<argc; i++) {
		if(!strcmp(argv[i], "-t")) {
			vertscale = atof(argv[++i]);
		} else if(!strcmp(argv[i], "-u")) {
			uniform = TRUE;
		} else if (!strcmp(argv[i], "-e")) {
			genEmpties = TRUE;
		} else if (!strcmp(argv[i], "-g")) {
			genGrid = TRUE;
 		} else if (!strcmp(argv[i], "-v")) {
                        verbose = TRUE;
		} else if (!strcmp(argv[i], "-d")) {
			min_cos_norm = cos(TORADIANS * atof(argv[++i]));
		} else if (!strcmp(argv[i], "-lh")) {
			left_handed = TRUE;
		} else if (!strcmp(argv[i], "-m")) {
			tm = new Triangle_Model;
                } else if(!strcmp(argv[i], "-i")) {
                        ifname = argv[++i];
                } else if(!strcmp(argv[i], "-c")) {
                        rgbfname = argv[++i];
 		} else if(!strcmp(argv[i], "-o")) {
                        ofname = argv[++i];
		} else {
			fprintf(stderr, "Unknown argument %s\n", argv[i]);
			fprintf(stderr, usage, argv[0]);
			return 1;
		}
	}

        if (ifname==NULL) {
            fprintf(stderr, usage, argv[0]);
            return 1;
        }

	if (verbose) {
		fprintf(stderr, "Empties will%s be generated.\n",
			genEmpties ? "" : " not");
		fprintf(stderr, "Grid will%s be generated.\n",
			genGrid ? "" : " not");
		fprintf(stderr, "Image file is: %s\n", ifname);
	}
	
	// open image file
	img = new GeoImage(ifname);

	if (rgbfname != NULL)
		rgb = new GeoImage(rgbfname);

	// Create the octree, given an image
	if ((oct = image2octree(img, rgb, vertscale, uniform, 
			genEmpties, genGrid)) == (Octree *)NULL)
	{
		fprintf(stderr, "%s: Unable to create octree.\n", argv[0]);
		return 1;
	}

	// Write out octree
        FILE_Dataport  *fp = new FILE_Dataport();
	
	if (ofname) {
	        if (!fp->wopen(ofname)) {
			fprintf(stderr, 
				"%s: Unable to create output file %s.\n", 
				argv[0], ofname);
			return 1;
		}
	} else {
		fp->open(stdout);
	}
	// (really should setup a SfcModel...)
	if (tm)
		put_token(fp, "SFC_MODEL_V1");
        oct->parse_out(fp);

	// write mesh faces if built
	if (tm)
		tm->parse_out(fp);
	fp->close();

	if (img->get_geo_data() && ofname) {
		// georeferenced image - also create a one-tree forest
		// to store the corresponding georef data
		// (note: node must be dynamically allocated, see GroupObj)
		ObjNode *node = new ObjNode;
		node->set_reference(ofname);
		node->set_name(ofname);
		node->set_geo_data(img->get_geo_data());

		Forest forest;
		forest.add_child(node);
		
		char fstname[4096];
		sprintf(fstname, "%s.fst", ofname);
	        if (!fp->wopen(fstname)) {
			fprintf(stderr, 
				"%s: Unable to create output file %s.\n", 
				argv[0], fstname);
			return 1;
		}
		forest.parse_out(fp, FALSE);
		fp->close();
	}
	
	return 0;
}

// Recursive function to count the full and empty leaf voxels in an octree
// Integer counters must be zerod before calling function.
static void count_leaf_nodes( Octree_Data * oct , int * voxel_count, 
		int * empty_voxel_count, int * no_data_count=NULL)
{
	double r,g,b,a;
	NodeSpec *ns;
	int ref;

	if (!oct) return;

	// If no children, we have a leaf node.
	// Distinguish between empties and real ones.

	if ( !oct->any_children() ) {
		if((ns = oct->get_node_data()) != NULL)	{
                    ns->get_color(&r, &g, &b, &a);

                    if(a > 0.0)
			(*voxel_count)++;
		    else if(a == -1.0)   // Empties are alpha = -1
			(*empty_voxel_count)++;
		} else {
		    fprintf(stderr, 
		    	"%s: Found a leaf node with no node data\n",
		    	progname);
		}
	} else {
		//Count the number of nodes that do not have data, e.g.
		//the non-leaf nodes

		if ((ns = oct->get_node_data()) == NULL)
			(*no_data_count)++;

		for (ref=UPPER_LEFT_BACK; ref<NUMBER_OF_SPACE_PARTITIONS; ref++) {
                        count_leaf_nodes(oct->get_child((Octree_Child_Ref)ref), 
				voxel_count, empty_voxel_count, no_data_count);
		}
	}
}

static int get_mallinfo()
{
	struct mallinfo mi = mallinfo();
	return (mi.usmblks + mi.uordblks);
}

// Compute normal for one triangle in elevation image, accumulate
// for average cell normal
static void add_one_normal(GeoImage *img, int x1, int y1, int x2, int y2,
		int x3, int y3, int nvalid, float zs, double *norm)
{
	double V12[3], V13[3], tnorm[3];
	double z1 = img->get_data()->get_float(x1, y1);
	V12[0] = x2 - x1;
	V12[1] = y2 - y1;
	V12[2] = zs * (img->get_data()->get_float(x2, y2) - z1);

	V13[0] = x3 - x1;
	V13[1] = y3 - y1;
	V13[2] = zs * (img->get_data()->get_float(x3, y3) - z1);

	// do cross product in reverse order to compensate for
	// left-handedness of image coordinates (Y increases downward)
	//if (left_handed)
		//cross_product(V12, V13, tnorm);
	//else
		cross_product(V13, V12, tnorm);
	normalize_vector(tnorm);

	if (nvalid==0) {		// first normal for image cell
		memcpy(norm, tnorm, sizeof(tnorm));

	// Accumulate unless apparently a discontinuity. 
	} else if (fabs(dot_product(tnorm, norm)) > min_cos_norm) {
		vector_sum(norm, tnorm, norm);
	}
}

// Estimate surface normal for elevation image at cell x,y by averaging
// normals of the 4 triangles using that cell (or fewer, at the edges).
//
//                x,y-1
//               / | \ 
//              /  |  \ 
//        x-1,y - x,y - x+1,y
//              \  |  /
//               \ | /
//                x,y+1
static void img_normal(GeoImage *img, int x, int y, float zs, double *norm)
{
	int nvalid = 0;			// found a valid normal yet?

	if (x > 0 && y > 0) { 		// upper left triangle
		add_one_normal(img, x, y, x, y-1, x-1, y, nvalid, zs, norm);
		nvalid++;
	}
	if (x+1 < xres && y > 0) {	// upper right triangle
		add_one_normal(img, x, y, x+1, y, x, y-1, nvalid, zs, norm);
		nvalid++;
	}
	if (x > 0 && y+1 < yres) {	// lower left triangle
		add_one_normal(img, x, y, x-1, y, x, y+1, nvalid, zs, norm);
		nvalid++;
	}
	if (x+1 < xres && y+1 < yres) {	// lower right triangle
		add_one_normal(img, x, y, x, y+1, x+1, y, nvalid, zs, norm);
		nvalid++;
	}
	
	normalize_vector(norm);
}

// Given and image and a thickness (scale), create an
// octree representation of the image.  Return a pointer
// to the octree.
// K. Sturdevant 12-8-98
Octree *image2octree(GeoImage * img, GeoImage *rgb, float vertscale,
			int ufs, int genEmpties, int genGrid)
{
	int	i,j, rj;
	float	z;
	float	min, max, temp;
	double	depth;
	int 	level;
	double	edge;
	int	voxel_count = 0;
	int	empty_voxel_count = 0;
	int	no_data_count = 0;
	int	curr_ram = 0;
	int	max_ram = 0;

	// get resolution
	img->get_res(&xres, &yres);

	// If creating mesh, allocate array to track previous column's nodes
	NodeSpec **pcol;
	if (tm) 
		pcol = new NodeSpec *[yres];

	// Get min and max valid elevations (z values) in image
	min = FLT_MAX;
	max = -FLT_MAX;
	for(i=0; i<xres; i++) {
		for(j=0; j<yres; j++) {
			temp = img->get_data()->get_float(i, j);
			if (VALID_Z(temp)) {
				if(temp < min)min = temp;
				if(temp > max)max = temp;
			}
		}
	}
	
	if (verbose) {
		fprintf(stderr, "Z Min=%f, max=%f\n",min,max);
		fprintf(stderr, "vertscale=%f\n", vertscale);
		fprintf(stderr, "uniform=%d\n", ufs);
		fprintf(stderr, "Image is %d X %d pixels\n", xres, yres);
	}
	
	depth = ceil((xres > yres) ? xres : yres);

	// Calculate the octree level
	// 2^level = depth
	// level = log (Base 2) depth = log (depth) / log (2);
	// round up
	level = int(ceil( log(depth)/log(2) ));

	if (verbose)
		fprintf(stderr, "depth is %f, level is %d\n", depth, level);

	// Create the octree
	Octree  *octree = new Octree(level + 1);
        NodeSpec *ns;
        double  cntr[OCTREE_DIMS];

        float x,y;
        int extra = 0;
	uchar red, green, blue;

	if (genGrid)		//Add extra grid cells
	{
	    cntr[2] = 0;
	    blue = 0;

            for (y=-1.0; y <=1.01; y+=0.1) {
        	for (x=-1.0; x <=1.01; x+=0.1) {
                        ns = new NodeSpec();
                        red = uchar((x+1.0)*127.4);
                        green = uchar((y+1.0)*127.4);
                        ns->set_color(red, green, blue);
                        ns->edge_length = .02;

                        ns->use_alpha(TRUE);
                        cntr[0] = x;
                        cntr[1] = y;
                        ns->set_global_center(cntr);

                        octree->add_voxel(ns);
                        extra++;
                }
            }

	    if (verbose)
		fprintf(stderr, "Done adding extra voxels, count = %d\n",
			extra);
	}

	// Set transformation matrix parameters for
	// converting octree model data (-1 to +1) to object/world (pixel/ht)
	// (object x/y = pixel coord, object z = pixel value * vertscale)
        octree->x = -1.0;
        octree->y = left_handed ? 1.0 : -1.0;

        octree->xrot = 0.0; 
        octree->yrot = 0.0;
        octree->zrot = 0.0;

	// default scale: leaf cell = exactly one pixel
	// (divide by two because model range is -1 to +1)
	double xyscale = pow(2.0, (double)level) / 2.0;
	double zscale = (max - min) * vertscale / 2.0;
	if (ufs) {			// force uniform scaling
		if (xyscale < zscale)		// choose largest
			xyscale = zscale;
		zscale = xyscale;		// use for Z too
	}
	// translate so Z range is centered at model z = 0
        octree->xscale = xyscale;
        octree->yscale = left_handed ? -xyscale : xyscale;
	octree->zscale = zscale;
	edge = 1.0/xyscale;		// cell edge in object coords
	octree->z = -(max + min) * vertscale / (2.0 * octree->zscale);

	// No more changes to transform parms, so precompute matrices
	// to speed up add_voxel()
	octree->freeze_xform();

	if (verbose) {
		curr_ram = get_mallinfo();
		fprintf(stderr, "After image read, mallinfo is %d bytes\n", 
			curr_ram);
		if (curr_ram > max_ram )
 	           max_ram = curr_ram;
	}

	// Read through the image and create voxels
	int counter = 0;
	int empty_counter = 0;
	int zdiff = int((max - min)*vertscale);

	// default color
	red = 255;
	green = blue = 0;

#ifdef FLEXCOLOR
	int clrbands = rgb ? rgb->get_data()->get_bands() : 3;
#endif
	
        for(i=0; i<xres; i++) {
		// rj = source row index
                for(j=0, rj=yres-1; j<yres; j++, rj--) {
			// ignore if no data at this location
                        z = img->get_data()->get_float(i, rj);
			if (! VALID_Z(z)) {
				if (tm)
					pcol[j] = NULL;
				continue;
			}

        		ns = new NodeSpec();

			if (tm) {	// update mesh
				if (i>0 && pcol[j]) {	// not left column
					Mesh_Triangle *mt;
					// valid triangle above?
					if (j < yres-1 && pcol[j+1]) {
						mt = new Mesh_Triangle(ns, pcol[j+1], pcol[j]);
						tm->add_triangle(mt);
					}
					// valid triangle below?
					if (j>0 && pcol[j-1]) {
						mt = new Mesh_Triangle(ns, pcol[j], pcol[j-1]);
						tm->add_triangle(mt);
					}
				}
				pcol[j] = ns;
			}

			// set voxel color (from file, or default)
#ifdef FLEXCOLOR
			if (rgb) {
				ColorSpec *color = new ColorSpec(clrbands);
				for (int band=0; band<clrbands; band++)
					color->set_color(band, 
						rgb->get_data()->get_float(i, rj, band));
				ns->set_color(color);
				int alpha = rgb->get_data()->get_alpha_band();
				if (alpha >= 0) {
        				ns->use_alpha(TRUE);
					color->set_alpha(
						rgb->get_data()->get_float(i, rj, alpha));
				}
			} else {
	       			ns->set_color(red, green, blue);
			}
#else
			if (rgb)
				rgb->get_data()->get_color(i, rj, 
							&red, &green, &blue);
       			ns->set_color(red, green, blue);
        		ns->use_alpha(TRUE);
#endif

        		ns->edge_length = edge;

        		cntr[0] = i + 0.5; 
			cntr[1] = (left_handed ? rj : j) + 0.5; 
			cntr[2] = z*vertscale + 0.5;
        		ns->set_global_center(cntr);

			// Compute a surface normal for the voxel
			double norm[3];
			img_normal(img, i, rj, vertscale, norm);
			ns->set_normal(norm);

			// add it to the tree
			octree->add_voxel(ns);
			counter++;

			// Fill space "above" this voxel with empties
			// Increase Z value by 1 until we reach the top
                        // of the cube. Z height is 2/zscale or 2*edge.
			if (genEmpties)	{
				// use integral z value?
				cntr[2] = ceil(z*vertscale - 0.5) + 0.5;

				for (cntr[2] = cntr[2] + 2.0*zdiff*edge; 
					cntr[2] < max*vertscale - edge/2.0; 
					cntr[2] += zdiff*edge/2.0)
				{
        		    		ns = new NodeSpec();
			    		// Empty should have no color kfs
			    		// Setting on now for vtkcubeoctree display only
        		    		ns->set_color(1.0, 1.0, 0.0, -1.0);
        		    		ns->edge_length = edge;
        		    		ns->use_alpha(TRUE);
        		    		ns->set_global_center(cntr);
			    		octree->add_voxel(ns);
			    		empty_counter++;
				}
			}
                }

	        // condense the octree every 2 rows
		if (i % 2) {
		    if (verbose) {
			    curr_ram = get_mallinfo();
			    if (curr_ram > max_ram )
			        max_ram = curr_ram;
		    }
		    octree->condense();
	        }
        }

	if (verbose) {
		fprintf(stderr, "Final mallinfo is %d bytes\n", get_mallinfo());
		fprintf(stderr, "Added total count of %d voxels/samples\n", 
			counter);
		fprintf(stderr, "Added total count of %d EMPTY voxels\n", 
			empty_counter);
	
		voxel_count = 0;
		empty_voxel_count = 0;
		no_data_count = 0;
		count_leaf_nodes( octree->get_data(), 
			&voxel_count, &empty_voxel_count, &no_data_count);

		fprintf(stderr, "Final tally: %d voxels\n", voxel_count);
		fprintf(stderr, "Final tally: %d empties\n", 
			empty_voxel_count);
		fprintf(stderr, "Final tally: %d no data nodes\n", 
			no_data_count);

		curr_ram = get_mallinfo();
		if (curr_ram > max_ram)
	   		max_ram = curr_ram;
		fprintf(stderr, "Final mallinfo is %d bytes\n", curr_ram);
		fprintf(stderr, "MAX ram used was %d bytes\n", max_ram);
		fprintf(stderr, "Percent Z range:  min: %f,  max: %f\n", 
			min*vertscale, max*vertscale);
	}
	
	// Return a pointer to the newly created octree
	return( octree );
}
