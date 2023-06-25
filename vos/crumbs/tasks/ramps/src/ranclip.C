// Clip range (XYZ) map - mark points outside limits as invalid
// ranclip.C 1.1 02/08/07 15:12:03

#include <stdio.h>
#include <math.h>
#include "image/image.h"
#include "image/datatypes.h"
#include "image/types/all.h"
#include "grape/range.h"

static const char usage[] = 
"Usage: %s [-v] -i input_xyz_map -o output_map\n"
"  -r xmin xmax ymin ymax zmin zmax\n"
"Clip range map by removing points outside specified limits\n"
"Default range is 'everything', which just cleans up the range file\n"
"(converts to native byte order, and converts zero points to invalid)\n"
"-v = verbose output\n";

int verbose = 0;

// input range map size
static int xres, yres, bands;

int main(int argc, char **argv)
{
	char *infile = NULL;
	char *outfile = NULL;
	Range r;
	int i;

	r.full();
	for(i=1; i<argc; i++) {
		if(!strcmp(argv[i], "-v")) {
			verbose = 1;
		} else if (!strcmp(argv[i], "-i")) {
			infile = argv[++i];
		} else if (!strcmp(argv[i], "-o")) {
			outfile = argv[++i];
		} else if (!strcmp(argv[i], "-r")) {
			r.xmin = atof(argv[++i]);
			r.xmax = atof(argv[++i]);
			r.ymin = atof(argv[++i]);
			r.ymax = atof(argv[++i]);
			r.zmin = atof(argv[++i]);
			r.zmax = atof(argv[++i]);
		} else {
			fprintf(stderr, "Unknown argument %s\n", argv[i]);
			fprintf(stderr, usage, argv[0]);
			return 1;
		}
	}

	if (infile == NULL || outfile == NULL) {
		fprintf(stderr, "Missing input/output filename\n");
		fprintf(stderr, usage, argv[0]);
		return 1;
	}

	// read RAN file
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
	imap->get_res(&xres, &yres, &bands);
	if (verbose)
		fprintf(stderr, "range map size = %d x %d x %d\n",
			xres, yres, bands);
	
	int num_invalid = 0;
	int num_valid = 0;
	int num_clipped = 0;
	
	for (int y=0; y<yres; y++) {
		for (int x=0; x<xres; x++) {
			double p[3];
			for (int i=0; i<3; i++)
				p[i] = imap->get_float(x, y, i);
			if (p[0] < -90000.0)
				num_invalid++;
			else if (p[0] == 0.0 && p[1] == 0.0 && p[2] == 0.0)
				num_invalid++;
			else {
				num_valid++;
				if (!r.in_range(p)) {
					num_clipped++;
					imap->set_float(-100000.0, x, y, 0);
				}
			}
		}
	}

	// write clipped file
	ranfile->write(outfile);
	
	if (verbose) {
		fprintf(stderr, "input: %d invalid, %d valid\n",
			num_invalid, num_valid);
		fprintf(stderr, "%d valid points clipped\n", num_clipped);
	}
	return 0;
}
