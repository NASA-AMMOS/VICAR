// Estimate 3D model scaling by finding coordinate ranges
// -i = range file
// -p = percent level of coordinate range to select (default=95)
#include <fstream.h>
#include <math.h>
#include "image/geoimage.h"

#define VALID_RANGE	-90000.0
#define HIST_SIZE	1000

double level = 0.95;
int xhist[HIST_SIZE], yhist[HIST_SIZE], zhist[HIST_SIZE];
Boolean verbose = FALSE;

int main(int argc, char **argv)
{
	char	*ifname=NULL;
	int i, j;

	for(i=1; i<argc; i++) {
                if (!strcmp(argv[i], "-v")) {
                        verbose = TRUE;
		} else if (!strcmp(argv[i], "-p")) {
			level = atof(argv[++i]) / 100.0;
		} else if(!strcmp(argv[i], "-i")) {
			ifname = argv[++i];
		} else {
			fprintf(stderr, "Argument %s unrecognized.\n", argv[i]);
			//fprintf(stderr, usage, argv[0]);
		        exit(1);
		}
	}

	// ifname is required argument
	if (ifname == NULL) {
		fprintf(stderr, "Must specify input XYZ file.\n");
		//fprintf(stderr, usage, argv[0]);
	        exit(1);
	}
	GeoImage *img = new GeoImage(ifname);
	if (img == NULL) {
		fprintf(stderr, "%s: Could not open XYZ file %s\n", 
			argv[0], ifname);
		exit(1);
	}

	int xres, yres;
	img->get_res(&xres, &yres);

	// find extremes in xyz, and averages
	double xmin, xmax, ymin, ymax, zmin, zmax;
	xmin = ymin = zmin = FLT_MAX;
	xmax = ymax = zmax = -FLT_MAX;
	double xsum, ysum, zsum;
	xsum = ysum = zsum = 0.0;
	double x, y, z;
	int npts;

	for (j=0; j<yres; j++) {
		for (i=0; i<xres; i++) {
			x = img->get_data()->get_float(i, j, 0);
			if (x < VALID_RANGE)
				continue;
			y = img->get_data()->get_float(i, j, 1);
			z = img->get_data()->get_float(i, j, 2);
			npts++;

 			if (x<xmin)	xmin = x;
 			if (x>xmax)	xmax = x;
 			if (y<ymin)	ymin = y;
 			if (y>ymax)	ymax = y;
 			if (z<zmin)	zmin = z;
 			if (z>zmax)	zmax = z;

			xsum += x;
			ysum += y;
			zsum += z;
		}
	}
	printf("%d pts, raw limits: x=%f - %f y=%f - %f z=%f - %f\n",
		npts, xmin, xmax, ymin, ymax, zmin, zmax);
	printf("centroid = %f %f %f\n", xsum/npts, ysum/npts, zsum/npts);

	// scale factors to map range coord to histogram index
	// index = (x - xmin)*xscale
	double xscale = HIST_SIZE / (xmax + 0.00001 - xmin);
	double yscale = HIST_SIZE / (ymax + 0.00001 - ymin);
	double zscale = HIST_SIZE / (zmax + 0.00001 - zmin);

	// accumulate histograms
	for (j=0; j<yres; j++) {
		for (i=0; i<xres; i++) {
			x = img->get_data()->get_float(i, j, 0);
			if (x < VALID_RANGE)
				continue;
			y = img->get_data()->get_float(i, j, 1);
			z = img->get_data()->get_float(i, j, 2);

			xhist[int((x - xmin)*xscale)]++;
			yhist[int((y - ymin)*yscale)]++;
			zhist[int((z - zmin)*zscale)]++;
		}
	}

	// find percentile limits
	int n;
	for (i=n=0; i<HIST_SIZE; i++) {
		n += xhist[i];
		if (n > (1.0-level)*npts)
			break;
	}
	printf("%f ", i / xscale + xmin);	// Xmin

	for (i=n=0; i<HIST_SIZE; i++) {
		n += xhist[i];
		if (n > level*npts)
			break;
	}
	printf("%f ", i / xscale + xmin);	// Xmax

	for (i=n=0; i<HIST_SIZE; i++) {
		n += yhist[i];
		if (n > (1.0-level)*npts)
			break;
	}
	printf("%f ", i / yscale + ymin);	// ymin

	for (i=n=0; i<HIST_SIZE; i++) {
		n += yhist[i];
		if (n > level*npts)
			break;
	}
	printf("%f ", i / yscale + ymin);	// ymax

	for (i=n=0; i<HIST_SIZE; i++) {
		n += zhist[i];
		if (n > (1.0-level)*npts)
			break;
	}
	printf("%f ", i / zscale + zmin);	// zmin

	for (i=n=0; i<HIST_SIZE; i++) {
		n += zhist[i];
		if (n > level*npts)
			break;
	}
	printf("%f\n", i / zscale + zmin);	// zmax
}
