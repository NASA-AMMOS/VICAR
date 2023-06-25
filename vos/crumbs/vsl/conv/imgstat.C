// compute statistics on image data

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <float.h>
#include <math.h>
#include "image/geoimage.h"
#include "image/datamod.h"

const char usage[] = "usage: %s [-min a] [-max b] imgfile [imgfile...]\n";

Image img;
int xres, yres, bands;
double ignore_min = -FLT_MAX;	// ignore pixels outside these limits
double ignore_max = FLT_MAX;

// stats for one image band
static void bandstat(int band)
{
	int x, y;

	double dmin = FLT_MAX;
	double dmax = -FLT_MAX;
	double dsum = 0;
	double dsum2 = 0;
	int n = 0;

	ImageData *idat = img.get_data();
	for (y=0; y<yres; y++) {
		for (x=0; x<xres; x++) {
			double d = idat->get_double(x, y, band);
			if (d < ignore_min || d > ignore_max)
				continue;
			if (d < dmin) dmin = d;
			if (d > dmax) dmax = d;
			dsum += d;
			dsum2 += d*d;
			n++;
		}
	}

	if (n == 0) {
		printf("  No data inside range limits\n");
		return;
	}
	double sdev = sqrt(n * dsum2 - dsum*dsum) / n;
	double mean = dsum / n;
	printf("  Range: %f %f Mean: %f\n  Std. Dev: %f 95%%: %f %f N: %d\n",
		dmin, dmax, mean, sdev, mean - 2*sdev, mean + 2*sdev, n);
}

// stats for all bands of one image
static void imgstat(char *name)
{
	printf("\nImage file %s:\n", name);
	if (img.read(name)) {
		fprintf(stderr, "Error reading input image\n");
		exit(1);
	}

	printf("File type: %d %s\n",
		img.get_file_type(), file_type_name[img.get_file_type()]);
	img.get_res(&xres, &yres, &bands);
	printf("Image size: %d x %d, %d band(s)\n", xres, yres, bands);

	for (int i=0; i<bands; i++) {
		printf("Band %d:\n", i);
		bandstat(i);
	}
}

int main(int argc, char **argv)
{
	int i;

	if (argc < 2) {
		fprintf(stderr, usage, argv[0]);
		return 1;
	}

	for (i=1; i<argc; i++) {
		if (!strcmp(argv[i], "-min")) {
			ignore_min = atof(argv[++i]);
		} else if (!strcmp(argv[i], "-max")) {
			ignore_max = atof(argv[++i]);
		} else if (argv[i][0] == '-') {
			fprintf(stderr, usage, argv[0]);
			return 1;
		} else {
			imgstat(argv[i]);
		}
	}
	return 0;
}
