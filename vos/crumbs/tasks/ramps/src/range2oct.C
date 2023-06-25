// range2oct.C 1.26 02/08/07 15:18:20
/** \file
* This program use the range2octree() function to convert an XYZ image
* into a grape octree or surface model file.
*/

#include <fstream.h>
#include <float.h>
#include <math.h>
#include "summitt_func.h"

static const char usage[] = 
"Usage: %s [-v] [-t camera_x camera_y camera_z]\n"
"  [-r roll pitch yaw] [-g] [-eye] [-f fov ] [-min range] [-max range]\n"
"  [-d discont] [-m [-a max_angle]] -i xyzfile [-c rgb_file] [-o outfile]\n\n"
"Creates octree from XYZ (range) image 'xyzfile' to 'outfile'\n"
"(default stdout).\n"
"-v = verbose output\n"
"-t = camera position (default 0,0,0)\n"
"-r = camera rotation (and xforms data to camera frame)\n"
"-g = generate grid voxels\n"
"-eye = generate voxel at eye (camera) point\n"
"-f = horizontal field of view in degrees (default=20)\n"
"-min/max = limits for valid range data\n"
"-d = min normal angle in degrees discarded as discontinuity (default 60)\n"
"-m = include triangle mesh in output\n"
"-a = max degrees between face normal and camera direction (default 85)\n";

int verbose = FALSE;

// Minimum cosine of angle between normal vectors. Smaller cosine
// (larger angle) indicates a discontinuity.
static double min_cos_norm = 0.5;	// default = 60 degrees

// Minimum cosine of angle between face normal and eye vector. Smaller
// cosine (larger angle) indicates invalid face triangle.
static double face_threshold = 0.08716;	// default = 85 degrees

static double camrot[3];		// camera rotation in world space
static ZMatrix camxform;		// transform world->camera space
static double zstretch = 1.0;		// extra scale factor for model Z (-s)

static char *progname;

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

int main(int argc, char **argv)
{
	int	i;
	char	*ifname=NULL, *ofname=NULL, *rgbfname = NULL;
	double	cam[3] = {0,0,0};
	float	fov = TORADIANS * 20.0;
	float	range_low = 0.0;
	float	range_high = FLT_MAX;
	int	flags = 0;

	progname = argv[0];
	for(i=1; i<argc; i++) {
		if(!strcmp(argv[i], "-t")) {
			cam[0] = atof(argv[++i]);
			cam[1] = atof(argv[++i]);
			cam[2] = atof(argv[++i]);
		} else if(!strcmp(argv[i], "-f")) {
			fov = TORADIANS * atof(argv[++i]);
		} else if(!strcmp(argv[i], "-r")) {
			camrot[0] = atof(argv[++i]);
			camrot[1] = atof(argv[++i]);
			camrot[2] = atof(argv[++i]);
			flags |= R2O_XCAM;
		} else if(!strcmp(argv[i], "-eye")) {
                        flags |= R2O_EYE;
                } else if (!strcmp(argv[i], "-g")) {
                        flags |= R2O_GRID;
		} else if (!strcmp(argv[i], "-m")) {
			flags |= R2O_MESH;
                } else if (!strcmp(argv[i], "-v")) {
                        verbose = TRUE;
		} else if(!strcmp(argv[i], "-min")) {
			range_low = atof(argv[++i]);
		} else if(!strcmp(argv[i], "-max")) {
			range_high = atof(argv[++i]);
		} else if (!strcmp(argv[i], "-d")) {
			min_cos_norm = cos(TORADIANS * atof(argv[++i]));
		} else if (!strcmp(argv[i], "-a")) {
			face_threshold = cos(TORADIANS * atof(argv[++i]));
		} else if(!strcmp(argv[i], "-i")) {
			ifname = argv[++i];
		} else if(!strcmp(argv[i], "-c")) {
			rgbfname = argv[++i];
		} else if(!strcmp(argv[i], "-o")) {
			ofname = argv[++i];
		} else if(!strcmp(argv[i], "-s")) {
			zstretch = atof(argv[++i]);
		} else {
			fprintf(stderr, "Argument %s unrecognized.\n", argv[i]);
			fprintf(stderr, usage, argv[0]);
		        exit(1);
		}
	}

	// ifname is required argument
	// May be a comma-separated list, e.g. "file_x,file_y,file_z"
	if (ifname == NULL) {
		fprintf(stderr, "Must specify input XYZ file.\n");
		fprintf(stderr, usage, argv[0]);
	        exit(1);
	}
	GeoImage *img = new GeoImage(ifname);
	if (img->get_res() < 1) {
		fprintf(stderr, "%s: Could not open XYZ file %s\n", 
			argv[0], ifname);
		exit(1);
	}

	// Open rgb file - if none specified or invalid, all voxels are red
	GeoImage *rgb = NULL;
	if (rgbfname) {
		rgb = new GeoImage(rgbfname);
		if (rgb == NULL || rgb->get_data() == NULL) {
			fprintf(stderr, "%s: Could not open rgb file %s\n", 
				argv[0], rgbfname);
		}
	}

	// if requested, setup transform to map range data into
	// camera coordinates (camera at 0,0,0 looking down +X, +Z=up)
	if (flags & R2O_XCAM) {
		ZMatrix mat2;

		if (verbose)
			fprintf(stderr, "Transforming camera to origin\n");

		// ** any scaling needed?

		// translate, then rotate

		// inverse translation is negative of camera position
		MakeTranslationMatrix(mat2, -cam[0], -cam[1], -cam[2]);

		// inverse rotation in transpose of normal rotation
		MakeRotationMatrix(camxform, camrot[0], camrot[1], camrot[2]);
		MatTranspose(camxform, camxform);

		// (rot) * (trans) applies in desired order
		MatMult(camxform, mat2);

		// now set camera at origin in the new frame
		memset(cam, 0, sizeof(cam));
	}

	// Create the octree, given range info
	SfcModel *oct = range2octree(img->get_data(), 
		rgb ? rgb->get_data() : NULL, cam, fov,
		range_low, range_high, camxform, flags,
		min_cos_norm, face_threshold, zstretch);
	if (oct == NULL) {
		fprintf(stderr, "%s: Unable to create octree.\n", argv[0]);
		exit (1);
	}

	// some final stats
	if (verbose) {
		int voxel_count = 0;
		int empty_voxel_count = 0;
		int no_data_count = 0;
		count_leaf_nodes(oct->get_data(), &voxel_count, 
			&empty_voxel_count, &no_data_count);

		fprintf(stderr, "Final tally:  %d voxels\n", voxel_count);
		fprintf(stderr, "Final tally:  %d empties\n", 
			empty_voxel_count);
		fprintf(stderr, "Final tally:  %d no data nodes\n", 
			no_data_count);
		if (flags & R2O_MESH)
			fprintf(stderr, "%d triangles\n", 
				oct->mesh.tri_count());
	}

	// Write out octree/surface model
        FILE_Dataport fp;
	if (ofname) {
	        if (!fp.wopen(ofname)) {
			fprintf(stderr, "%s: Can't create output file %s\n",
				argv[0], ofname);
			exit (1);
		}
	} else {
		fp.open(stdout);
	}
	
	if (flags & R2O_MESH)
		oct->parse_out(&fp);
	else
		oct->Octree::parse_out(&fp);
	return 0;
}
