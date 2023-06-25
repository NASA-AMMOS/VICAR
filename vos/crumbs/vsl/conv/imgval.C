// test - dump pixel values in window

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
	if (argc != 4) {
		fprintf(stderr, "usage: %s <img> <x> <y>\n", argv[0]);
		return 1;
	}

	Image img1;
	if (img1.read(argv[1])) {
		fprintf(stderr, "can't read input image %s\n", argv[1]);
		return 1;
	}

	int xres1, yres1, bands1;
	img1.get_res(&xres1, &yres1, &bands1);

	ImageData *id1 = img1.get_data();
	int x = atoi(argv[2]);
	int y = atoi(argv[3]);

	for (int b=0; b<bands1; b++) {
		printf("Band %d:\n", b);
		for (int yy = y-2; yy < y+2; yy++) {
			for (int xx = x-2; xx < x+2; xx++) {
				printf(" x=%4d y=%4d data=%f\n",
					xx, yy, id1->get_double(xx, yy, b));
			}
		}
	}
}
