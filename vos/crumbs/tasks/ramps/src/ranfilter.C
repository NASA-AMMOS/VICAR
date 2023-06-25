// ranfilter.C 1.8 03/05/01 07:53:07
/** \file
* Program to filter range maps using ranfltr services.
*/
#include <math.h>
#include <stdlib.h>
#include "image/image.h"
#include "image/datatypes.h"
#include "image/types/ranfile.h"
#include "summitt_func.h"

static const char usage[] = 
"Usage: %s [-v] [-d delta] [-f] [-r] [-g ngap]\n"
"      -i input_range_map -o output_range_map\n"
"Filters and/or resamples range map\n"
"-v = verbose output\n"
"-d = max delta range to include in filtering (default=1000)\n"
"-f = filter noise\n"
"-r = resample to 1/2 resolution (with smoothing)\n"
"-g = interpolate across gaps up to ngap pixels\n";

int verbose = 0;

int main(int argc, char **argv)
{
	int filter = 0;			// apply smoothing?
	int reduce = 0;			// reduce resolution 2x?
	int ngap = 0;			// interpolate across gaps?
	float range_delta = 1000.0;	// max delta range to filter
	char *infile = NULL;
	char *outfile = NULL;
	for(int i=1; i<argc; i++) {
		if(!strcmp(argv[i], "-d")) {
			range_delta = atof(argv[++i]);
		} else if(!strcmp(argv[i], "-f")) {
			filter = 1;
		} else if(!strcmp(argv[i], "-r")) {
			reduce = 1;
		} else if(!strcmp(argv[i], "-g")) {
			ngap = atoi(argv[++i]);
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
	if (!filter && !reduce && !ngap) {
		fprintf(stderr, "%s: No processing specified -"
			" must use -f, -r, and/or -g\n", argv[0]);
		return 1;
	}
	
	// read range file to map1
	if (verbose)
		fprintf(stderr, "reading range file %s\n", infile);
	Image *ranfile = new Image(infile);
	ImageData *map1 = ranfile->get_data();
	if (map1 == NULL) {
		fprintf(stderr, "%s: Can't read range file %s\n", 
			argv[0], infile);
		return 1;
	}
	int xres, yres;
	map1->get_res(&xres, &yres);
	if (verbose)
		fprintf(stderr, "range map size = %d x %d\n", xres, yres);
	
    //make sure output file will have proper permissions
    umask(002);
	FILE *fp = fopen(outfile, "w");
	if (fp == NULL) {
		fprintf(stderr, "%s: Can't create output file %s\n", 
			argv[0], outfile);
		return 1;
	}
	
	// do filtering
	ImageData *map2;
	if (filter) {
		if (verbose)
			fprintf(stderr, "Filtering...\n");
		map2 = new floatData;
		if(!map2->allocate(xres,yres,3)) {
			fprintf(stderr, "%s: Can't allocate filtered range map\n", 
				argv[0]);
			return 1;
		}
		summitt_range_filter(map1, map2, range_delta);
	} else {
		map2 = map1;
	}

	// do reduction
	ImageData *map3;
	if (reduce) {
		if (verbose)
			fprintf(stderr, "Resampling...\n");
		map3 = new floatData;
		if(!map3->allocate(xres/2,yres/2,3)) {
			fprintf(stderr, "%s: Can't allocate reduced range map\n",
				argv[0]);
			return 1;
		}
		summitt_range_resample(map2, map3);
	} else {
		map3 = map2;
	}

	// interpolate gaps
	if (ngap) {
		if (verbose)
			fprintf(stderr, "Interpolating gaps...\n");
		summitt_range_interpolate(map3, ngap);
	}

	// write result
	Image ri;
	RANFile *rf = new RANFile;
	ri.set_data(map3);
	ri.set_file(rf);
	rf->write_data(fp);
	return 0;
}
