// pnt2oct.C 1.2 02/08/07 15:17:14
/** \file
** Convert an ASCII point list file (X Y Z R G B) into an octree.
** RGB data may be in separate files.
*/
#include "summitt_func.h"

static const char usage[] = 
"Usage: %s [-v] -i input_points [-o output_octree]\n"
"  [-e edge_length] [-c color_file [color_file ...]]\n\n"
"Creates octree from ASCII 3D point list file\n"
"-v = verbose output\n"
"-i = input point list file\n"
"-o = output octree file (default = stdout)\n"
"-e = voxel edge length (default = 1.0)\n"
"-c = ordered list of point files with RGB data corresponding to input\n";

struct Point {
	double x, y, z;		// coordinates
	int r, g, b;		// color
};

int verbose;
static char **args;		// copy of argv for color file list
static int rgbstart, rgbend;	// argv index for RGB files

// read point and possibly RGB data, return FALSE at EOF/error
static int read_point(FILE *fp, Point *p, int getrgb)
{
	char buf[1024];
	if (fgets(buf, sizeof(buf), fp) == NULL)
		return FALSE;
	int n = sscanf(buf, "%lf %lf %lf %d %d %d", 
		&p->x, &p->y, &p->z, &p->r, &p->g, &p->b);
	if (n == 6)		// all set
		return TRUE;
	if (n != 3) {		// something wrong
		fprintf(stderr, "Invalid input line %s\n", buf);
		return FALSE;
	}

	// only got XYZ; color data must be in a separate file
	if (!getrgb)		// don't care this pass
		return TRUE;

	static FILE *rgbfp = NULL;
	if (rgbfp == NULL || fgets(buf, sizeof(buf), rgbfp) == NULL) {
		// if finished this RGB file, get next one
		if (rgbfp) {
			fclose(rgbfp);
			rgbfp = NULL;
			rgbstart++;
		}
		if (rgbstart >= rgbend) {
			// out of RGB data, set the voxel to red
			p->r = 255;
			p->g = p->b = 0;
			return TRUE;
		}
		rgbfp = fopen(args[rgbstart], "r");
		if (rgbfp == NULL) {
			fprintf(stderr, "Can't open RGB file %s\n",
				args[rgbstart]);
			exit(1);
		}
		if (fgets(buf, sizeof(buf), rgbfp) == NULL) {
			fprintf(stderr, "empty RGB file %s\n", args[rgbstart]);
			return FALSE;
		}
	}
	double xx, yy, zz;
	if (sscanf(buf, "%lf %lf %lf %d %d %d", 
			&xx, &yy, &zz, &p->r, &p->g, &p->b) != 6) {
		fprintf(stderr, "Invalid RGB file %s\n", args[rgbstart]);
		return FALSE;
	}

	return TRUE;
}

int main (int argc, char **argv)
{
	char *infile = NULL;
	char *outfile = NULL;
	double edge = 1.0;	// voxel edge length
	int i;

	args = argv;		// save for color list
	for (i=1; i<argc; i++) {
		if (!strcmp(argv[i], "-i")) {
			infile = argv[++i];
		} else if (!strcmp(argv[i], "-o")) {
			outfile = argv[++i];
		} else if (!strcmp(argv[i], "-v")) {
			verbose = TRUE;
		} else if (!strcmp(argv[i], "-e")) {
			edge = atof(argv[++i]);
		} else if (!strcmp(argv[i], "-c")) {
			rgbstart = ++i;
			// find next option or end of arg list
			while (++i<argc && argv[i][0] != '-')
				;
			rgbend = --i;
		} else {
			fprintf(stderr, "Unknown argument %s\n", argv[i]);
			fprintf(stderr, usage, argv[0]);
			return 1;
		}
	}

	// start input point list
	// (can't use stdin as we're going to make two passes on it)
	if (!infile) {
		fprintf(stderr, "No input point file given\n");
		fprintf(stderr, usage, argv[0]);
		return 1;
	}

	FILE *fp_in = fopen(infile, "r");
	if (fp_in == NULL) {
		fprintf(stderr, "%s: can't open %s for reading\n", 
			argv[0], infile);
		return 1;
	}

	// scan input points for bounding volume
	Summitt_range bbox;
	bbox.init();
	Point p;
	int npoint = 0;
	while (read_point(fp_in, &p, FALSE)) {
		npoint++;
		bbox.include(&p.x);	
	}
	if (verbose) {
		fprintf(stderr, "%d input points\n", npoint);
		bbox.dump(stderr, "bounding volume");
	}

	// determine octree depth from bounding volume and edge length
	double scale = bbox.xmax - bbox.xmin;
	if (scale < bbox.ymax - bbox.ymin)
		scale = bbox.ymax - bbox.ymin;
	if (scale < bbox.zmax - bbox.zmin)
		scale = bbox.zmax - bbox.zmin;
	if (scale < edge)
		scale = edge;
	double depth = scale / edge;
	int levels = int(ceil(log(depth) / log(2.0))) + 1;
	if (verbose)
		fprintf(stderr, "scale=%f depth=%f levels=%d\n",
			scale, depth, levels);

	// setup output octree
	Octree *oct = new Octree(levels);
	Octree_Data *od = oct->init_data();

	oct->x.set_value(-(bbox.xmax + bbox.xmin) / scale); 
	oct->y.set_value(-(bbox.ymax + bbox.ymin) / scale);
	oct->z.set_value(-(bbox.zmax + bbox.zmin) / scale);
	oct->xrot.set_value(0.0);
	oct->yrot.set_value(0.0);
	oct->zrot.set_value(0.0);
	scale /= 2.0;	// so model space goes -1 to 1
	oct->xscale.set_value(scale);
	oct->yscale.set_value(scale);
	oct->zscale.set_value(scale);
	if (verbose) {
		fprintf(stderr, "Setting transform to: "
			"Trans=%f, %f, %f Rot=%f, %f, %f Scale=%f, %f, %f\n",
			oct->x.get_value(),oct->y.get_value(),
			oct->z.get_value(),
			oct->xrot.get_value(),oct->yrot.get_value(),
			oct->zrot.get_value(),
			oct->xscale.get_value(),oct->yscale.get_value(),
			oct->zscale.get_value());
	}
    
	// freeze transforms to speed up voxel adding
	oct->freeze_xform();

	// re-scan input point file, adding voxels to octree
	rewind(fp_in);
	while (read_point(fp_in, &p, TRUE)) {
		NodeSpec *ns = new NodeSpec();
		ns->set_color(p.r, p.g, p.b, 1);
		ns->use_alpha(TRUE);
		ns->edge_length = edge/scale;
		ns->set_global_center(&p.x);
		oct->add_voxel(ns);
	}

	// Write output octree
	FILE_Dataport fp_out;
	if (outfile) {
		if (!fp_out.wopen(outfile)) {
			fprintf(stderr, "%s: can't open %s for writing\n", 
				argv[0], outfile);
			return 1;
		}
	} else {
    		fp_out.open(stdout);
	}
	oct->parse_out(&fp_out);
	fp_out.close();
	return 0;
}
