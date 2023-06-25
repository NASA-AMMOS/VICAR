// ht2iv.C 1.2 03/05/14 11:47:05
/** \file
 ** Convert 2.5D height map (image with pixel value = height)
 ** into Inventor point model
**/

#include <stdio.h>
#include "image/image.h"
#include "image/datatypes.h"

static const char usage[] = 
"Usage: %s -i input_file [-s xyscale] [-t xoffset yoffset]\n"
"  [-z zscale] [-b band] [-v] [-o iv_file]\n\n"
"Converts 2.5D height map to Inventor or VRML format (for display)\n"
"-i = input data (e.g. VICAR REAL image)\n"
"-s = scale from pixel position to X/Y value (if not in image header)\n"
"-t = X/Y offset of upper-left pixel\n"
"-z = height scale factor\n"
"-b = take specified band of input image (default=0)\n"
"-v = output in VRML format instead of Inventor\n"
"-o = output Inventor filename; default is standard output\n";

int vrml;			// output in VRML format?
int band = 0;			// image band to read
int xres, yres;			// input image size
double xyscale = 1.0;		// pixel to output XY scaling
double zscale = 1.0;		// height scaling
double xoffset, yoffset;	// lower left offset

int main(int argc, char **argv)
{
	FILE *ofp;
	char *infile = NULL, *outfile = NULL;
	int i;

	for(i=1; i<argc; i++) {
		if(!strcmp(argv[i],"-i")) {
			infile = argv[++i];
		} else if(!strcmp(argv[i],"-o")) {
			outfile = argv[++i];
		} else if(!strcmp(argv[i],"-t")) {
			xoffset = atof(argv[++i]);
			yoffset = atof(argv[++i]);
		} else if(!strcmp(argv[i],"-s")) {
			xyscale = atof(argv[++i]);
			if (xyscale == 0.0) {
				fprintf(stderr, "%s: Zero scale is invalid\n",
					argv[0]);
				return 1;
			}
		} else if(!strcmp(argv[i],"-z")) {
			zscale = atof(argv[++i]);
		} else if(!strcmp(argv[i],"-b")) {
			band = atoi(argv[++i]);
		} else if(!strcmp(argv[i],"-v")) {
			vrml = 1;
		} else {
			fprintf(stderr, "Unknown argument %s\n", argv[i]);
			fprintf(stderr, usage, argv[0]);
			exit(1);
		}
	}

	if (infile == NULL) {
		fprintf(stderr, "Input file not specified\n");
		fprintf(stderr, usage, argv[0]);
		exit(1);
	}

	// start input
	Image *htmap;
	if ((htmap = new Image(infile)) == NULL) {
		fprintf(stderr, "%s: Can't read height map %s\n", 
			argv[0], infile);
		return 1;
	}
	ImageData *img = htmap->get_data();
	if (!img) {
		fprintf(stderr, "%s: No height data in %s\n",
			argv[0], infile);
		return 1;
	}
	img->get_res(&xres, &yres);
	
	// check for scaling info from input header (comments)
	char *header = htmap->get_comments();
	if (header) {
		char *field;
		if ((field = strstr(header, "XOFFSET=")) != NULL)
			xoffset = atof(field+8);
		if ((field = strstr(header, "X_AXIS_MINIMUM=")) != NULL)
			xoffset = atof(field+15);
		if ((field = strstr(header, "YOFFSET=")) != NULL)
			yoffset = atof(field+8);
		if ((field = strstr(header, "Y_AXIS_MINIMUM=")) != NULL)
			yoffset = atof(field+15);
		if ((field = strstr(header, "MAP_SCALE=")) != NULL)
			xyscale = atof(field+10);
	}

    //make sure output file will have proper permissions
    umask(002);
	// start output
	if (outfile) {
		if ((ofp = fopen(outfile, "w")) == NULL) {
			fprintf(stderr, "%s: Can't create output %s\n", 
				argv[0], outfile);
			exit(1);
		}
	} else {
		ofp = stdout;
	}

	// output inventor (or VRML) ASCII header
	if (vrml)
		fputs("#VRML V1.0 ascii\n", ofp);
	else
		fputs("#Inventor V2.0 ascii\n", ofp);
	fputs("MaterialBinding {value OVERALL}\n", ofp);
	fputs("Material {diffuseColor 1 0.3 0.3}\n", ofp);
	fprintf(ofp, "Scale {scaleFactor %g %g %g}\n",
		xyscale, xyscale, zscale);

	// adjust offsets to come out right after scaling
	xoffset /= xyscale;
	yoffset /= xyscale;

	// output points, skipping extreme values (holes)
	fputs("Coordinate3 {point [\n", ofp);
	for (int y=0; y<yres; y++) {
		for (int x=0; x<xres; x++) {
			double z = img->get_float(x, y, band);
			if (fabs(z) < 1.0E10)
				fprintf(ofp, "%f %f %f,\n", 
					x + xoffset, y + yoffset, z);
		}
	}
	fputs("]}\n", ofp);

	// finish inventor output
	fputs("PointSet {}\n", ofp);
	return 0;
}
