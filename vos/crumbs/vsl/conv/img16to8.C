// img16to8.C 1.9 04/02/14 13:09:56
// Preprocess rover image for terrain texture map:
// - convert halfword grayscale image (usually 12-bit) to 8-bit SGI RGB,
// - rescale dimensions to power of two
// - adjust intensity range
//
// Intensity mapping options:
//  -f: input 0 -> output 0; input (bitmax) -> output 255
//  -s: input min -> output 0; input max -> output 255
//  -m: input min -> output 0; input mean -> output 128
//  -p<min>:<max>: <min> -> output 0, <max> -> output 255

#include <stdio.h>
#include "image/image.h"
#include "image/types/all.h"
#include "image/datamod.h"

static const char usage[] = 
	"usage: %s [-v] [-2] [-s|m|f|p<min>:<max>] [-e] input_img output_img\n"
	"Preprocess terrain imageinto SGI RGB format\n"
	"-2 = rescale dimensions to nearest power of two\n"
	"-s = stretch input pixel range to output range (default)\n"
	"-m = stretch to map mean pixel value to middle gray\n"
	"-f = fixed shift of limit input to 8-bit output\n"
	"     limit based on SAMPLE_BIT_MASK for PDS, else 4095\n"
	"-p = preset scaling of input <min>:<max> to 8-bit output\n"
	"-e = ignore edges (first/last 8 rows/cols) for stretch stats\n"
	"-v = verbose output\n";

int verbose;
int rescale;
int no_edges;
enum { STRETCH, MEAN, FIXED, PRESET } mapping;

// adjust image dimension to nearest power of two
static int adjust(int dim)
{
	int d;
	for (d=2; dim > 3*d/2; d*=2)
		;
	return d;
}

int main(int argc, char **argv)
{
	char *pname = argv[0];
	int pmin, pmax;
	int x, y;

	while (argc > 1 && argv[1][0] == '-') {
		if (argv[1][1] == 'v')
			verbose = 1;
		else if (argv[1][1] == '2')
			rescale = 1;
		else if (argv[1][1] == 'e')
			no_edges = 1;
		else if (argv[1][1] == 'm')
			mapping = MEAN;
		else if (argv[1][1] == 's')
			mapping = STRETCH;
		else if (argv[1][1] == 'f') {
			mapping = FIXED;
			pmin = 0;
			pmax = 4095;	// default is 12-bit
		} else if (argv[1][1] == 'p') {
			mapping = PRESET;
			if (sscanf(argv[1]+2, "%d:%d", &pmin, &pmax) != 2) {
				fprintf(stderr, usage, pname);
				return 1;
			}
		} else {
			fprintf(stderr, usage, pname);
			return 1;
		}
		argv++;
		argc--;
	}
	if (argc != 3) {
		fprintf(stderr, usage, pname);
		return 1;
	}
	
	// open input image, check results
	Image *img = new Image(argv[1]);
	int xres, yres, bands;
	img->get_res(&xres, &yres, &bands);
	ImageData *idat = img->get_data();
	if (xres < 1 || idat == NULL) {
		fprintf(stderr, "%s: failedd to open input image %s\n",
			pname, argv[1]);
		return 1;
	}
	if (bands != 1 && bands != 3) {
		fprintf(stderr, "%s: warning, expected grayscale or RGB image, "
			"%s actually has %d bands\n",
			pname, argv[1], bands);
	}

	if (verbose)
		printf("Input image = %dw X %dh x %d bands\n", 
			xres, yres, bands);

	// adjust "max bit value" for PDS image with SAMPLE_BIT_MASK header
	if (mapping == FIXED && img->get_file_type() == PDS_FILE_ID) {
		PDSFile *pds = (PDSFile *)img->get_file();
		char *mask = pds->get_value("SAMPLE_BIT_MASK", "IMAGE");
		if (mask) {	// assuming form is "2#<zeros><ones>"
			pmax = 0;
			while (*mask != 0 && *mask != '1')
				mask++;
			while (*mask == '1') {
				pmax = (pmax << 1) + 1;
				mask++;
			}
			if (verbose)
				printf("Max pixel from PDS BIT_MASK = %d\n",
					pmax);
		}
	}

	// rescale to integer powers of two if requested and necessary
	if (rescale) {
		int newxres = adjust(xres);
		int newyres = adjust(yres);
		if (newxres != xres || newyres != yres) {
			if (verbose)
				printf("Resizing to %dw X %dh\n",
					newxres, newyres);
			Image *timg = new Image;
			*timg = new transData(img, 
				0.0, 0.0, (double)newxres/xres,
				0.0, 0.0, (double)newyres/yres);
			timg->allocate(newxres, newyres, bands);
			idat = new interpData(timg);
			xres = newxres;
			yres = newyres;
		}
	}
	
	// determine intensity mapping
	if (mapping != FIXED && mapping != PRESET) {
		pmin = 65536;
		pmax = 0;
		int npix = 0;

		// if RGB image, use green band (really should convert
		// to luminance...)
		if (bands > 1)
			idat->set_default_band(1);

		// find min, max, and mean input intensity,
		// ignoring zero pixels
		// (note: 1Kx1K image, all pixels at 4095 = 32 bits!)
		long long pmean = 0;
		// optionally ignore first/last 8 rows and columns
		int ymin, ymax, xmin, xmax;
		if (no_edges && xres > 16 && yres > 16) {
			xmin = ymin = 8;
			xmax = xres-8;
			ymax = yres-8;
		} else {
			xmin = ymin = 0;
			xmax = xres;
			ymax = yres;
		}
		for (y=ymin; y<ymax; y++) {
			for (x=xmin; x<xmax; x++) {
				int pix = idat->get_ushort(x, y);
				if (pix == 0)	// skip (probably data dropout)
					continue;
				pmean += pix;
				if (pix < pmin)
					pmin = pix;
				if (pix > pmax)
					pmax = pix;
				npix++;
			}
		}
		if (npix)
			pmean /= npix;

		if (verbose)
			printf("Input %d non-zero pixels in range = %d to %d,"
				" mean = %lld\n", 
				npix, pmin, pmax, pmean);
		if (mapping == MEAN)
			pmax = (int)(pmean << 1);
	}

	// setup output image
	Image outimg;
	outimg.create_data_type(UCHAR_DATA);
	ImageData *outdat = outimg.get_data();
	outdat->allocate(xres, yres, bands);

	// copy pixel data, scaling to 8 bits
	int pscale = pmax - pmin + 1;

	for (int b=0; b<bands; b++) {
		for (y=0; y<yres; y++) {
			for (x=0; x<xres; x++) {
				int pix = idat->get_ushort(x, y, b);
				pix = ((pix - pmin) << 8) / pscale;
				outdat->set_uchar(
					pix>255 ? 255 : pix<0 ? 0 : pix, 
					x, y, b);
			}
		}
	}

	// write as SGI RGB format
	if (outimg.write(argv[2], RGB_FILE_ID) != 0) {
		fprintf(stderr, "%s: error writing output image %s\n",
			pname, argv[2]);
		return 1;
	}
	
	return 0;
}
