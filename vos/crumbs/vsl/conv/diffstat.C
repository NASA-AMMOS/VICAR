// test - stats on pixel differences in two images

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <float.h>
#include <math.h>
#include "image/geoimage.h"
#include "image/datamod.h"

int verbose = 0;

int main(int argc, char **argv)
{
	if (argc == 4 && !strcmp(argv[1], "-v")) {
		verbose = 1;
		argc--;
		argv++;
	} else if (argc != 3) {
		fprintf(stderr, "usage: %s [-v] <img1> <img2>\n", argv[0]);
		return 1;
	}

	Image img1, img2;
	if (img1.read(argv[1])) {
		fprintf(stderr, "can't read input image %s\n", argv[1]);
		return 1;
	}
	if (img2.read(argv[2])) {
		fprintf(stderr, "can't read input image %s\n", argv[2]);
		return 1;
	}

	int xres1, yres1, bands1;
	int xres2, yres2, bands2;
	img1.get_res(&xres1, &yres1, &bands1);
	img2.get_res(&xres2, &yres2, &bands2);
	if (xres1 != xres2 || yres1 != yres2 || bands1 != bands2) {
		fprintf(stderr, "input images not same dimensions!\n");
		return 1;
	}

	ImageData *id1 = img1.get_data();
	ImageData *id2 = img2.get_data();

	for (int b=0; b<bands1; b++) {
		double dmin = FLT_MAX;
		double dmax = -FLT_MAX;
		double dsum = 0.0;
		double dsum2 = 0.0;

		for (int y=0; y<yres1; y++) {
			for (int x=0; x<xres1; x++) {
				double d = fabs(id1->get_double(x, y, b) -
						id2->get_double(x, y, b));
				if (d < dmin) dmin = d;
				if (d > dmax) {
					dmax = d;
					if (verbose)
						printf("(%d,%d,%d): %f %f\n", 
						  x, y, b, 
						  id1->get_double(x, y, b),
						  id2->get_double(x, y, b));
				}
				dsum += d;
				dsum2 += d*d;
			}
		}
		int n = xres1 * yres1;
		printf("Band %d deltas: min=%f max=%f mean=%f sdev=%f\n",
			b, dmin, dmax, dsum / n, 
			sqrt(n * dsum2 - dsum*dsum) / n);
	}
}
