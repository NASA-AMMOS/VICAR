/** \file
* Scale range (XYZ) map in range dimension
* (e.g. to compensate for imperfect camera separation info)
*/
#include <math.h>
#include "image/image.h"
#include "image/datatypes.h"
#include "image/types/all.h"
#include "grape/vector_ops.h"

static const char usage[] = 
"Usage: %s [-v] [-c camx camy camz]\n"
"  -i input_xyz_map -o output_xyz_map -s scale_factor\n"
"Scale range (XYZ) map in range dimension\n"
"-c = camera position (default 0,0,0)\n"
"-v = verbose output\n";

int verbose = 0;

// scale factor
static double scale;

// camera position
static double cam[3];

// input range map size
static int xres, yres;

int main(int argc, char **argv)
{
	char *infile = NULL;
	char *outfile = NULL;
	int i;
	for(i=1; i<argc; i++) {
		if(!strcmp(argv[i], "-s")) {
			scale = atof(argv[++i]);
                } else if(!strcmp(argv[i], "-c")) {
                        cam[0] = atof(argv[++i]);
                        cam[1] = atof(argv[++i]);
                        cam[2] = atof(argv[++i]);
		} else if(!strcmp(argv[i], "-v")) {
			verbose = 1;
		} else if(!strcmp(argv[i], "-i")) {
			infile = argv[++i];
		} else if(!strcmp(argv[i], "-o")) {
			outfile = argv[++i];
		} else {
			fprintf(stderr, "Unknown argument %s\n", argv[i]);
			fprintf(stderr, usage, argv[0]);
			return 1;
		}
	}

	if (infile == NULL || outfile == NULL) {
		fprintf(stderr, "%s: Missing input/output filenames\n", 
			argv[0]);
		return 1;
	}
	if (scale == 0.0) {
		fprintf(stderr, "%s: Missing or zero scale\n", argv[0]);
		return 1;
	}
	
	// read RAN file
	if (verbose)
		fprintf(stderr, "reading range file %s\n", infile);
	Image *ranfile;
	if ((ranfile = new Image(infile)) == NULL) {
		fprintf(stderr, "%s: Can't read range file %s\n", 
			argv[0], infile);
		return 1;
	}
	ImageData *imap = ranfile->get_data();
	if (!imap) {
		fprintf(stderr, "%s: No range file data in %s\n",
			argv[0], infile);
		return 1;
	}
	imap->get_res(&xres, &yres);
	if (verbose)
		fprintf(stderr, "range map size = %d x %d\n", xres, yres);
	
	for (int y=0; y<yres; y++) {
		for (int x=0; x<xres; x++) {
			// scale this point
			for (i=0; i<3; i++) {
				double xyz = imap->get_float(x, y, i);
				if (xyz > -90000.0)	// valid cell
					xyz = cam[i] + scale * (xyz - cam[i]); 
				imap->set_float(xyz, x, y, i);
			}
		}
	}
	
	// write result
	Image ri;
	RANFile rf;
	ri.set_data(imap);
	ri.set_file(&rf);
	rf.write_image(outfile);
	return 0;
}
