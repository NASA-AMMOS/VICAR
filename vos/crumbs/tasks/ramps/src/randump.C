// randump.C 1.1 02/08/07 15:53:51
// Dump/compute stats on range (XYZ) data

#include <stdio.h>
#include <math.h>
#include "image/image.h"
#include "image/datatypes.h"
#include "image/types/all.h"
#include "range.h"

static const char usage[] = 
"Usage: %s [-v] input_xyz_map\n"
"Compute statistics on range map data\n"
"-v = verbose dump\n";

int verbose = 0;

// input range map size
static int xres, yres, bands;

int main(int argc, char **argv)
{
	char *infile = NULL;
	int i;
	for(i=1; i<argc; i++) {
		if(!strcmp(argv[i], "-v")) {
			verbose = 1;
		} else if (argv[i][0] != '-') {
			infile = argv[i];
		} else {
			fprintf(stderr, "Unknown argument %s\n", argv[i]);
			fprintf(stderr, usage, argv[0]);
			return 1;
		}
	}

	if (infile == NULL) {
		fprintf(stderr, "Missing input filename\n");
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
	fprintf(stderr, "range map size = %dw x %dh x %d\n",
		xres, yres, bands);
	
	int num_invalid = 0;
	int num_zero = 0;
	int num_valid = 0;
	Summitt_range r;
	r.init();
	
	for (int y=0; y<yres; y++) {
		for (int x=0; x<xres; x++) {
			double p[3];
			for (int i=0; i<3; i++)
				p[i] = imap->get_float(x, y, i);
			if (p[0] < -90000.0)
				num_invalid++;
			else if (p[0] == 0.0 && p[1] == 0.0 && p[2] == 0.0)
				num_zero++;
			else {
				num_valid++;
				r.include(p);
				if (verbose)
					printf("%f %f %f\n", p[0], p[1], p[2]);
			}
		}
	}

	r.dump(stderr, "XYZ data");
	fprintf(stderr, "%d invalid points, %d zero points, %d valid points\n",
		num_invalid, num_zero, num_valid);
	return 0;
}
