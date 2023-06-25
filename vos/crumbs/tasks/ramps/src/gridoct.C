// Build octree containing a uniform grid
// ** INCOMPLETE (probably not needed)
#include "grape/octree.h"

void main(int argc, char **argv)
{
	const char usage[] = "Usage is: %s [-c r g b] [-z z] "
		"-x x1 x2 nx -y y1 y2 ny -o octfile\n"
		" -r g b = color components (0-255), default=red\n"
		" -z = Z value, default = 0\n"
		" x1 x2 = min/max X value, nx = number of X values\n";

	int red = 255;
	int green = blue = 0;
	float z = 0.0;
	float x1, x2, y1, y2;
	int nx = 0, ny = 0;
	char *ofname=NULL;

	for(i=1; i<argc; i++) {
		if(!strcmp(argv[i], "-c")) {
			red = atoi(argv[++i]);
			green = atoi(argv[++i]);
			blue = atoi(argv[++i]);
                } else if(!strcmp(argv[i], "-z")) {
			z = atof(argv[++i]);
                } else if(!strcmp(argv[i], "-o")) {
                        ofname = argv[++i];
		} else if (!strcmp(argv[i], "-x")) {
			x1 = atof(argv[++i]);
			x2 = atof(argv[++i]);
			nx = atoi(argv[++i]);
		} else if (!strcmp(argv[i], "-y")) {
			y1 = atof(argv[++i]);
			y2 = atof(argv[++i]);
			ny = atoi(argv[++i]);
		} else {
			fprintf(stderr, usage, argv[0]);
			exit(1);
		}
	}
	if (nx <= 0 || ny <= 0 || ofname == NULL) {
		fprintf(stderr, "%s: required arguments missing\n", argv[0]);
		exit(1);
	}

        FILE_Dataport  *fp = new FILE_Dataport();
        if (fp->wopen(ofname) == NULL) {
		fprintf(stderr, "%s: Unable to create %s.\n", argv[0], ofname);
		exit (-1);
	}


	// Create the octree and fill with grid

	// Write out octree
	oct->parse_out(fp);
        fp->close();
	exit(0);
}

	// Calculate the octree level
	// 2^level = depth
	// level = log (Base 2) depth = log (depth) / log (2);
	// round up
	level = ceil( log(depth)/log(2) );

fprintf(stdout, "depth is %f, level is %f\n", depth, level);

	// Create the octree
	Octree  *octree = new Octree(level + 1);
        NodeSpec *ns;
        double  cntr[OCTREE_DIMS];

	cntr[2] = 0;

	float x, y;
        for (y=-1.0; y <=1.0; y+=0.1) {
        	for (x=-1.0; x <=1.0; x+=0.1) {
                        ns = new NodeSpec();
                        ns->set_color(red, green, blue, 1.0);
                        ns->edge_length = .02;

                        ns->use_alpha(TRUE);
                        cntr[0] = x;
                        cntr[1] = y;
                        ns->set_global_center(cntr);

                        octree->add_voxel(ns);
                }
	}

	// Set transformation matrix parameters for
	// converting octree model data (-1 to +1) to object/world (pixel/ht)
	// (object x/y = pixel coord, object z = pixel value * vertscale)
        octree->x = -1;
        octree->y = -1;

        octree->xrot = 0.0; 
        octree->yrot = 0.0;
        octree->zrot = 0.0;

	// default scale: leaf cell = exactly one pixel
	// (divide by two because model range is -1 to +1)
	double xyscale = pow(2.0, level) / 2.0;
	double zscale = (max - min) * vertscale / 2.0;
	if (ufs) {			// force uniform scaling
		if (xyscale < zscale)		// choose largest
			xyscale = zscale;
		zscale = xyscale;		// use for Z too
	}
	// translate so Z range is centered at model z = 0
        octree->xscale = octree->yscale = xyscale;
	octree->zscale = zscale;
	edge = 1.0/xyscale;		// cell edge in object coords
	octree->z = -(max + min) * vertscale / (2.0 * octree->zscale);

	fprintf(stdout, "Added total count of %d voxels/samples\n", counter);
}
