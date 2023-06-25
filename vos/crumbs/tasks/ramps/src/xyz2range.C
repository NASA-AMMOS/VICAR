/** \file
* Compute range data from XYZ map (opposite of range2xyz)
*/
#include <math.h>
#include "image/image.h"
#include "image/datatypes.h"
#include "image/types/all.h"
#include "grape/vector_ops.h"

static const char usage[] = 
"Usage: %s [-v] -i input_xyz_map -o output_range_map [-c camx camy camz]\n"
"Compute raw range (distance) from XYZ range map\n"
"-v = verbose output\n"
"-c = camera position (default 0,0,0)\n";

int verbose = 0;

// camera position
static double cam[3];

// input range map size
static int xres, yres;

int main(int argc, char **argv)
{
	char *infile = NULL;
	char *outfile = NULL;
	for(int i=1; i<argc; i++) {
		if(!strcmp(argv[i], "-c")) {
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
	imap->get_res(&xres, &yres);
	if (verbose)
		fprintf(stderr, "range map size = %d x %d\n", xres, yres);
	
	// setup output map
	ImageData *omap = new floatData;
	if(!omap->allocate(xres,yres,1)) {
		fprintf(stderr, "%s: Can't allocate output map\n", 
				argv[0]);
		return 1;
	}

	// compute range
	for (int y=0; y<yres; y++) {
		for (int x=0; x<xres; x++) {
			double xyz[3];
			xyz[0] = imap->get_float(x, y, 0);
			if (xyz[0] < -90000.0) {	// invalid cell
				omap->set_float(-100000.0, x, y);
				continue;
			}
			xyz[1] = imap->get_float(x, y, 1);
			xyz[2] = imap->get_float(x, y, 2);
			omap->set_float(distance(cam, xyz), x, y);
		}
	}
	
	// write result
	Image ri;
	VicRealMMap rf;
	ri.set_data(omap);
	ri.set_file(&rf);
	rf.write_image(outfile);
	return 0;
}
