// Interpolate holes in height map (for fst2img).
// Based on John Wright's flood fill code.
// interp_map.C 1.3 03/12/09 08:02:03

#include <stdio.h>
#include <stdlib.h>
#include "image/geoimage.h"

extern int verbose;

class Edge_Point {
    public:
	float	pixel;		// map value (height)
	uchar	r, g, b;	// color value
	int	x;		// map location
	int	y;
	int	xvec;		// edge normal
	int	yvec;
	Edge_Point	*next;	// list link
};

static Edge_Point *edge_list;
static char *hole_map;		// 1 = hole in height map
static ImageData *zmap;		// local copy
static ImageData *rgbmap;	// local copy
static int xpix, ypix;		// map dimensions

// Is there a hole at x,y? Assuming y is valid; return false if X invalid
static inline int is_hole(int x, int y)
{
	return x>=0 && x<xpix && hole_map[x+y*xpix];
}

// Interpolate one hole at (j,i),
// filling in from edge points facing towards the hole.
// Returns interpolated height value.
static inline float interpolate_hole(int j, int i)
{
	float xval = 0.0f;	// accumulated height
	float xwgt = 0.0f;	// accumulated weighting
	float r=0.0f, g=0.0f, b=0.0f;

	// scan list for relevant edge points
	Edge_Point *tedge;
	for (tedge = edge_list; tedge; tedge = tedge->next) {
		int dx = j - tedge->x;
		int dy = i - tedge->y;

		// check dot product, does edge point towards me?
		if (tedge->xvec * dx + tedge->yvec * dy >= 0) {
			// yes, accumulate weighted by closeness
			float dist = 1.0f / (dx*dx + dy*dy);
			dist *= dist;	// 4th power
			// ** dist *= dist; // 8th?
			xwgt += dist;
			xval += tedge->pixel * dist;
			if (rgbmap) {
				r += tedge->r * dist;
				g += tedge->g * dist;
				b += tedge->b * dist;
			}
		}
	}

	if (rgbmap)
		rgbmap->set_color(j, i, int(r/xwgt), int(g/xwgt), int(b/xwgt));
	return xval / xwgt;
}

// Interpolate one hole at (j,i) with filter of immediate neighbors.
// Returns interpolated height value.
static inline float filter_hole(int j, int i)
{
	float xval = 0.0f;
	float r=0.0f, g=0.0f, b=0.0f;
	int wsum = 0;

	// filter weighting by |dx|+|dy|, 0 to 6
	static const int weight[] = { 49, 36, 25, 16, 9, 4, 1 };

	int di, dj;
	for(di=-3; di<=3; di++) {	// 7x7 weighted
		int k = i+di;
		if (k<0 || k>=ypix)
			continue;
		for(dj=-3; dj<=3; dj++) {
			int l = j+dj;
			if (l<0 || l>=xpix || hole_map[l+k*xpix])
				continue;
			// valid neighbor pixel
			int wt = weight[abs(di)+abs(dj)];
			wsum += wt;
			xval += wt * zmap->get_float(l, k, 1);
			if (rgbmap) {
				uchar fr, fg, fb;
				rgbmap->get_color(l, k, &fr, &fg, &fb);
				r += wt*fr;
				g += wt*fg;
				b += wt*fb;

			}
		}
	}
	if (rgbmap)
		rgbmap->set_color(j, i, int(r/wsum), int(g/wsum), int(b/wsum));
	return xval / wsum;
}

// Interpolate float (height) map.
// zmap should be 2- or 3-band float.
// rgbmap can be null to disable color interpolation.
// empty = pixel value indicating a hole.
// alpha = nonzero to add 3rd band alpha/coverage channel
void interpolate_map(ImageData *inzmap, ImageData *clrmap, float empty, int alpha)
{
	rgbmap = clrmap;		// save local copy
	zmap = inzmap;			// save local copy

	zmap->get_res(&xpix, &ypix);
	int xpix1 = xpix - 1;
	int ypix1 = ypix - 1;

	edge_list = NULL;		// init list
	int edge_count = 0;		// diagnostic

	int i, j, k, l;	// i=row, j=col, k=neighbor row, l=neighbor col

	// build map of holes, initialize filtered map
	hole_map = new char[xpix * ypix];
	memset(hole_map, 0, xpix*ypix);
	char *h = hole_map;
	for(i=0; i<ypix; i++) {
		for(j=0; j<xpix; j++, h++) {
			float xval = zmap->get_float(j, i, 0);
			if (xval == empty)
				*h = 1;
			else
				zmap->set_float(xval, j, i, 1);
		}
	}

	// optionally add alpha channel
	if (alpha) {
		h = hole_map;
		for(i=0; i<ypix; i++) {
			for(j=0; j<xpix; j++, h++) {
				// barely covered if hole, else opaque
				if (*h)
					zmap->set_float(1.0/255.0, j, i, 2);
				else
					zmap->set_float(1.0, j, i, 2);
			}
		}
        // -ozp
        	int radius = 10;
        	for (int row = 0; row < ypix; row++) {
            		for (int col=0; col < xpix; col++) {
                                float tValue = zmap->get_float(col, row, 2);
                           	for (int cnt1 = row - radius; cnt1 <= row + radius; cnt1++) {
                                	for (int cnt2 = col - radius; cnt2 <= col + radius; cnt2++) {
                                            //fprintf(stderr,"cnt1,cnt2 = %d,%d\n", cnt1, cnt2);
                                            float testVal = zmap->get_float(cnt2, cnt1, 2);
                                                if (testVal > 0.999) {
                                                    testVal = (float)(((double)radius - sqrt((cnt2-col)*(cnt2-col)+(cnt1-row)*(cnt1-row))) / (double)radius);
                                                    if(testVal > tValue) {
                                                        zmap->set_float(testVal, col, row, 2);
                                                        tValue = testVal;
                                                    }
                                                }
                                        }
                           
                           	}
            		}
        	}
                // rescale the data
                for (int row = 0; row < ypix; row++) {
                        for (int col=0; col < xpix; col++) {
                            float tValue = zmap->get_float(col, row, 2);
                            zmap->set_float(tValue*255.0, col, row, 2);
                        }
                }

	} //alpha chanel

        

	// build list of edge points with normal vectors
	h = hole_map;
	for(i=0; i<ypix; i++) {
		for(j=0; j<xpix; j++, h++) {
			if (*h)			// hole, skip it
				continue;

			// check for edginess, accumulate normal
			int xwgt = 0;
			int xvec = 0;
			int yvec = 0;
			for(k=i-1; k<=i+1; k++) {
				if (k<0 || k>=ypix) continue;
				for(l=j-1; l<=j+1; l++) {
					if (is_hole(l, k)) {
						xwgt++;
						xvec += l-j;
						yvec += k-i;
					}
				}
			}
			if(xwgt) {	// must be an edge
				edge_count++;
				Edge_Point *tedge = new Edge_Point();
				tedge->x = j;
				tedge->y = i;
				tedge->xvec = xvec;
				tedge->yvec = yvec;
				tedge->pixel = zmap->get_float(j, i, 1);
				if (rgbmap)
			  	  rgbmap->get_color(j, i, &tedge->r, 
						&tedge->g, &tedge->b);
				tedge->next = edge_list;
				edge_list = tedge;
			}
		}
	}

	if (verbose)
		fprintf(stderr,"Edge count = %d\n", edge_count);
	if (edge_count == 0)
		return;

	// Build interpolated map. To speed things up, do in 3 phases.

	// Phase 1: full interpolation of edge neighbors
	Edge_Point *tedge;

#if 0 	// this step can be really slow, and doesn't add much...
	for (tedge = edge_list; tedge; tedge = tedge->next) {
		i = tedge->y;
		j = tedge->x;

		for(k=i-1; k<=i+1; k++) {
			if (k<0 || k>=ypix) continue;
			for(l=j-1; l<=j+1; l++) {
				if (is_hole(l, k)) { 
					zmap->set_float(interpolate_hole(l, k), 
							l, k, 1);
					hole_map[l+k*xpix] = 0;
				}
			}
		}
	}
	if (verbose)
		fprintf(stderr, "Edge neighbors interpolated\n");
#endif

	// Phase 2: full interpolation at subresolution
	for(i=0; i<ypix; i+=4) {
		if (verbose && (i%100) == 0)
			fprintf(stderr, "Sub interpolation row %d\n", i);
		if (i & 4)	// alternate even/odd columns
			j = 0;
		else
			j = 2;
		int imap = j + i * xpix;
		for (; j<xpix; j += 4, imap += 4) {
			if (hole_map[imap]) {
				zmap->set_float(interpolate_hole(j,i), j, i, 1);
				hole_map[imap] = 0;
			}
		}
	}
	if (verbose)
		fprintf(stderr, "Subres interpolation done\n");

	// Phase 3: local filtering of remaining holes
	h = hole_map;
	for(i=0; i<ypix; i++) {
		if (verbose && (i%100) == 0)
			fprintf(stderr, "Filtering row %d\n", i);
		for (j=0; j<xpix; j++, h++) {
			if (*h) {
				// need to interpolate
				zmap->set_float(filter_hole(j, i), j, i, 1);
			}
		}
	}

	// Free up allocated data
	Edge_Point *tnext;
	for (tedge = edge_list; tedge; tedge = tnext) {
		tnext = tedge->next;
		delete tedge;
	}
	delete [] hole_map;
}
