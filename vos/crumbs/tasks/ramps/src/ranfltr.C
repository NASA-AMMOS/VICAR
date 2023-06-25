// ranfltr.C 1.12 03/12/02 12:38:55
/** \file
* Functions to filter range (XYZ) maps to 
* - smooth out noise 
* - reduce resolution
* - fill data gaps
*/
#include <math.h>
#include <stdlib.h>
#include "image/image.h"
#include "image/datatypes.h"
#include "image/types/ranfile.h"

extern int verbose;

// max delta range to be included in filtering
static float range_delta;

// input map
static ImageData *map;

// input range map size
static int xres, yres;

// temporary interpolation map
static ImageData *imap;

/// get range cell from map, return TRUE if cell is invalid
static int get_cell(int x, int y, float r[3])
{
	r[0] = map->get_float(x, y, 0);
	if (r[0] < -90000.0)	// invalid X
		return TRUE;
	r[1] = map->get_float(x, y, 1);
	r[2] = map->get_float(x, y, 2);
	// also invalid if all are zero
	return r[0]==0.0 && r[1]==0.0 && r[2]==0.0;
}

/// Conditionally accumulate one range value for filtering
static void acc1(float *sum, int *n, float *ctr, int x, int y)
{
	float val[3];
	
	if (x<0 || x>=xres || y<0 || y>=yres)	// off the edge
		return;

	// check for valid entry and within delta of center point
	if (get_cell(x, y, val) || 
			fabs(val[0]-ctr[0]) > range_delta ||
			fabs(val[1]-ctr[1]) > range_delta ||
			fabs(val[2]-ctr[2]) > range_delta)
		return;
		
	(*n)++;		// okay
	sum[0] += val[0];
	sum[1] += val[1];
	sum[2] += val[2];
}

/**
Copy the map
**/
void summitt_range_copy(ImageData *imap, ImageData *omap)
{
	map = imap;
	map->get_res(&xres, &yres);
	
	for (int j=0; j<yres; j++) {
		for (int i=0; i<xres; i++) {
			float ctr[3];
            ctr[0] = map->get_float(i, j, 0);
            ctr[1] = map->get_float(i, j, 1);
            ctr[2] = map->get_float(i, j, 2);
			omap->set_float(ctr[0], i, j, 0);
			omap->set_float(ctr[1], i, j, 1);
			omap->set_float(ctr[2], i, j, 2);
		}
	}
}

/**
// Replace each entry with average of 3x3 region around cell,
// ignoring "invalid" cells and skipping entries that differ
// by more than range_delta.
// Not a particularly efficient implementation.
**/
void summitt_range_filter(ImageData *imap, ImageData *omap, float delta)
{
	range_delta = delta;
	map = imap;
	map->get_res(&xres, &yres);
	
	for (int j=0; j<yres; j++) {
		for (int i=0; i<xres; i++) {
			float ctr[3], sum[3];
			if (get_cell(i, j, ctr)) {
				omap->set_float(ctr[0], i, j, 0);
				omap->set_float(ctr[0], i, j, 1);
				omap->set_float(ctr[0], i, j, 2);
				continue;
			}
			int n=1;
			memcpy(sum, ctr, sizeof(ctr));
			acc1(sum, &n, ctr, i-1, j-1);
			acc1(sum, &n, ctr, i,   j-1);
			acc1(sum, &n, ctr, i+1, j-1);
			acc1(sum, &n, ctr, i-1, j);
			acc1(sum, &n, ctr, i+1, j);
			acc1(sum, &n, ctr, i-1, j+1);
			acc1(sum, &n, ctr, i,   j+1);
			acc1(sum, &n, ctr, i+1, j+1);
			omap->set_float(sum[0]/n, i, j, 0);
			omap->set_float(sum[1]/n, i, j, 1);
			omap->set_float(sum[2]/n, i, j, 2);
		}
	}
}

// Resample one 1/2-size output range map cell at (i,j) from 
// input map(i2,j2). No averaging, just subsample.
// dx/dy is direction to check for alternate points if specified cell
// is invalid.
static void resample(ImageData *omap, int i, int j, int i2, int j2,
	int dx, int dy)
{
	float ctr[3];
	if (get_cell(i2, j2, ctr)) {	// center is invalid, try neighbors
		if (get_cell(i2+dx, j2, ctr)) {
			if (get_cell(i2, j2+dy, ctr)) {
				if (get_cell(i2+dx, j2+dy, ctr)) {
					// none valid, set invalid output
					omap->set_float(-100000.0f, i, j, 0);
					omap->set_float(-100000.0f, i, j, 1);
					omap->set_float(-100000.0f, i, j, 2);
					return;
				}
			}
		}
	}
	omap->set_float(ctr[0], i, j, 0);
	omap->set_float(ctr[1], i, j, 1);
	omap->set_float(ctr[2], i, j, 2);
}

/// Resample range image to 1/2 resolution.
// Special ordering is to avoid shrinking meshes.
void summitt_range_resample(ImageData *imap, ImageData *omap)
{
	int i, j;	// output (half-size) column/row

	map = imap;
	map->get_res(&xres, &yres);
	int ictr = xres/4;	// output center column/row
	int jctr = yres/4;
	int imax = xres/2 - 1;	// output last column/row
	int jmax = yres/2 - 1;

	// loop on output entries, working from corners inward
	// (some output cells will be recomputed a few times...)
	int i2, j2;
	for (j=j2=0; j<jctr; j++, j2+=2) {
		for (i=i2=0; i<ictr; i++, i2+=2) {
			resample(omap, i,      j,      i2,        j2, 1, 1);
			resample(omap, imax-i, j,      xres-1-i2, j2, -1, 1);
			resample(omap, i,      jmax-j, i2,        yres-1-j2, 1, -1);
			resample(omap, imax-i, jmax-j, xres-1-i2, yres-1-j2, -1, -1);
		}
	}
	// fill in center row/column, one direction only
	i2 = 2*ictr;
	for (j=j2=0; j<=jmax; j++, j2+=2)
		resample(omap, ictr, j, i2, j2, 1, 1);
	j2 = 2*jctr;
	for (i=i2=0; i<=imax; i++, i2+=2)
		resample(omap, i, jctr, i2, j2, 1, 1);
}

/// Accumulate interpolation at specified map cell, if valid.
// wnew = distance from hole being interpolated
// Return 1 if valid cell
// (logic adapted from fst2img.C height map interpolation)
static int acc3(int x, int y, double wnew, double acc[], double *wsum)
{
	float r[3];
	if (get_cell(x, y, r))
		return 0;
	acc[0] += wnew * r[0];
	acc[1] += wnew * r[1];
	acc[2] += wnew * r[2];
	*wsum += wnew;
	return 1;
}

/**
// Weighted interpolation of nearest non-empty cells to
// fill in an empty cell. Updates imap and returns 1 if successful. 
// ** maybe should check that points are nearly coplanar?
**/
static int interpolate(int maxgap, int x, int y)
{
	double acc[3];	// accumulators (X, Y, Z)
	double wsum;	// accumulated weight
	int nnen = 0;	// number of non-empty neighbors
	acc[0] = acc[1] = acc[2] = wsum = 0.0;

	for (int d=1; d<=maxgap; d++) {
		// accumulate non-empty cells in rectangle d units from x,y
		double wnew = 1.0 / d;
		int dx, dy;
		int x1 = x - d; if (x1 < 0) x1 = 0;
		int x2 = x + d; if (x2 >= xres) x2 = xres-1;

		dy = y - d;		// top row
		if (dy >= 0) {
			for (dx=x1; dx<=x2; dx++) 
				nnen += acc3(dx, dy, wnew, acc, &wsum);
		}
		dy = y + d;		// bottom row
		if (dy < yres) {
			for (dx=x1; dx<=x2; dx++) 
				nnen += acc3(dx, dy, wnew, acc, &wsum);
		}

		int y1 = y - d + 1; if (y1 < 0) y1 = 0;
		int y2 = y + d - 1; if (y2 >= yres) y2 = yres-1;

		dx = x - d;		// left column
		if (dx >= 0) {
			for (dy=y1; dy<=y2; dy++) 
				nnen += acc3(dx, dy, wnew, acc, &wsum);
		}
		dx = x + d;		// right column
		if (dx < xres) {
			for (dy=y1; dy<=y2; dy++) 
				nnen += acc3(dx, dy, wnew, acc, &wsum);
		}

		if (nnen >= 4) {	// successful, update temp map
			imap->set_float(acc[0]/wsum, x, y, 0);
			imap->set_float(acc[1]/wsum, x, y, 1);
			imap->set_float(acc[2]/wsum, x, y, 2);
			return 1;
		}
	}
	return 0;
}

/// interpolate across gaps in range image
void summitt_range_interpolate(ImageData *inmap, int maxgap)
{
	int i, j;	// column/row
	float r[3];

	map = inmap;
	map->get_res(&xres, &yres);

	// to avoid using interpolated gap cells in future interpolation,
	// put them into a temporary map
	imap = new floatData;
	if (!imap->allocate(xres,yres,3)) {
		fprintf(stderr, "summitt_range_interpolate: "
			"Can't allocate temporary map %d x %d\n", 
				xres, yres);
		delete imap;
		return;
	}

	int num_int = 0;	// number of interpolated cells (diagnostic)
	for (j=0; j<yres; j++) {
		for (i=0; i<xres; i++) {
			imap->set_float(-100000.0, i, j, 0);
			if (get_cell(i, j, r))
				num_int += interpolate(maxgap, i, j);
		}
	}

	if (verbose)
		fprintf(stderr, "%d range map holes interpolated\n", num_int);

	// now safe to copy interpolated cells back to input map
	for (j=0; j<yres; j++) {
		for (i=0; i<xres; i++) {
			r[0] = imap->get_float(i, j, 0);
			if (r[0] >= -90000.0) {	// valid
                r[1] = imap->get_float(i, j, 1);
                r[2] = imap->get_float(i, j, 2);
				map->set_float(r[0], i, j, 0);
				map->set_float(r[1], i, j, 1);
				map->set_float(r[2], i, j, 2);
			}
		}
	}

	delete imap;
}
