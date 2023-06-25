#include <ctype.h>
#include <float.h>
#include <math.h>
#include "summitt_func.h"

#ifndef MIN
#define MIN(a,b)	((a)<(b) ? (a) : (b))
#define MAX(a,b)	((a)>(b) ? (a) : (b))
#endif

#define ROUND(x)	(int(rint(x)))

#define HIST_BINS	100

static Forest forest;		// the octree forest
static int ntrees;		// number of trees in forest
static ZMatrix m2w;		// current model->world transform
static GeoData *georef;		// current georeference data

static char *infile, *htfile, *rgbfile;	// filenames

static int geo;			// georeferenced map output?
static int xres = 256;		// output resolution (size)
static int yres;
static int max_interp;		// interpolate? (was maximum distance)
static int refsite;		// reference site number
static float hist_dz;		// max histogram delta Z bin (0=no histogram)
static float scale;		// scale factor world coords to pixel
static float zscale = 1.0;	// scale world coords to output height
static float empty = FLT_MAX;	// Z value for empty cell
static Summitt_range range;	// region limits (X/Y or lon/lat)

static ImageData *zmap, *wzmap;	// (output, working maps)
static ImageData *dzmap;	// min height map for delta Z histogram
static int ztype = VIC_REAL_FILE_ID;

static Image rgbimg;		// color data
static ImageData *rgbmap, *wrgbmap;	// (output, working maps)
static int rgbtype = VIC_BYTE_FILE_ID;

// count of input points falling into each output cell,
// for averaging colors and to distinguish valid cells from interpolated cells
static int *npoints;		// count per cell for averaging
#define VALID(x,y)		npoints[(y)*xres+x]

static unsigned char *vmap;	// valid pixel map for line-of-sight testing

void interpolate_map(ImageData *zmap, ImageData *rgbmap, float empty);

int main(int argc, char **argv)
{
	int i;
	int x, y;

	verbose = 1;

	// read map
	Image zimg;
	zimg.read(argv[1]);
	zmap = zimg.get_data();

	zmap->get_res(&xres, &yres, &i);
	printf("bands=%d xres=%d\n", i, xres);
	printf("b0[00]=%f b1[00]=%f\n", zmap->get_float(0,0,0),
		zmap->get_float(0,0,1));
	
	interpolate_map(zmap, NULL, empty);

	zimg.create_file_type(ztype, "foo.ht");
	zimg.write();
}
