// TEST VERSION: interpolate with iterative 3x3 filtering
// %M% %I% %E% %U%
/** \file
 ** Build 2.5D height map from (a subregion) of an octree forest
 ** (or from a single octree). Can also build a corresponding color
 ** or grayscale image from voxel color data.
 **
 ** For VICAR-format output, label fields are included to define the
 ** scaling from pixels to world or site coordinates: 
 ** "X_AXIS_MINIMUM=x Y_AXIS_MINIMUM=y MAP_SCALE=s",
 ** indicating that world (or site) coordinates for pixel(row, col) are
 **   (col*s + x, row*s + y, pixel_value(row,col))
 ** If the world frame has +Z = up, the height map is left-handed.
 **
 ** Region limit coordinates (xmin/xmax) correspond to the centers
 ** of the first and last pixels. Y limits are the same, within the
 ** constraints of integer pixels and equal X/Y scaling.
 **
 ** For better results, and to limit total memory use, one forest
 ** model is loaded and converted at a time, with the resulting
 ** working map merged into a single output map.
 **
 ** If interpolation is requested, a second band is created in the height
 ** map containing interpolated data.
 **/
#include <ctype.h>
#include <float.h>
#include <math.h>
#include "summitt_func.h"

static const char usage[] = 
"Usage: %s [-v] [-x xres] [-g] [-a maxd]\n"
"  [-refsite site] [-r xmin xmax ymin ymax] [-hist max_dz]\n"
"  [-i input_file] [-c output_rgb [rgb_type]] -o output_map [map_type]\n"
"Builds 2.5D height map image and corresponding color image\n"
"from octree or forest 'input_file' (default stdin),\n"
"writing height map to 'output_map'\n"
"in format code 'map_type' (default vicar real).\n"
"Optionally build corresponding color data to 'output_rgb'\n"
"in format code 'rgb_type' (default vicar byte).\n"
"(use '-format' to see available format codes)\n"
"-v = verbose output\n"
"-x = set X resolution of output (Y resolution is set automatically)\n"
"     default is 256\n"
"-g = use georeferencing to build long/lat elevation map\n"
"-a = maximum distance in output pixels to accumulate/interpolate\n"
"     across missing data (default 10)\n"
"-refsite = output relative to specified site frame, -1 = current site\n"
"     (default = site 0 = world frame)\n"
"-r = set output region limits (X/Y or long/lat)\n"
"     (forest world or site frame, or octree object frame)\n"
"-hist = produce diagnostic histogram of delta Z's\n"
"     for 100 bins with last bin at given delta (if multiple input octrees)\n";

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
static int max_interp = 100;	// maximum interpolation iterations
static int refsite;		// reference site number
static float hist_dz;		// max histogram delta Z bin (0=no histogram)
static float scale;		// scale factor world coords to pixel
static float zscale = 1.0;	// scale world coords to output height
static float empty = -FLT_MAX;	// Z value for empty cell
static Summitt_range range;	// region limits (X/Y or lon/lat)

static Image zimg;		// height map data
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

static unsigned char *vmap;	// valid pixel map for interpolation

/// Update lat/long range by checking one bounding XYZ volume point
static void georange_chk(double x, double y, double z, Summitt_range *r)
{
	double lat, lon, el;
	georef->xyztolle(x, y, z, lat, lon, el);
	if (lon < r->xmin) r->xmin = lon;
	else if (lon > r->xmax) r->xmax = lon;
	if (lat < r->ymin) r->ymin = lat;
	else if (lat > r->ymax) r->ymax = lat;
}

/// Convert bounding range in X/Y to lat/long (using current georef)
static void georange(Summitt_range *r)
{
	double lat, lon, el;
	Summitt_range gr;

	georef->xyztolle(r->xmin, r->ymin, r->zmin, lat, lon, el);
	gr.xmin = gr.xmax = lon;
	gr.ymin = gr.ymax = lat;

	georange_chk(r->xmin, r->ymin, r->zmax, &gr);
	georange_chk(r->xmin, r->ymax, r->zmin, &gr);
	georange_chk(r->xmin, r->ymax, r->zmax, &gr);
	georange_chk(r->xmax, r->ymin, r->zmin, &gr);
	georange_chk(r->xmax, r->ymin, r->zmax, &gr);
	georange_chk(r->xmax, r->ymax, r->zmin, &gr);
	georange_chk(r->xmax, r->ymax, r->zmax, &gr);

	*r = gr;
}

/// Find world coordinate limits of patches in "forest", save in "range"
static void forest_range()
{
	for (int tree=0; tree<ntrees; tree++) {
		Patch *p = forest.get_patch(tree);
		georef = forest.get_geo_data(tree);
		
		if (tree == 0) {	// initialize overall range
			range = p->bvol;
			if (geo)
				georange(&range);

		} else {		// update range
			// optionally adjust bounding volume to lat/long
			if (geo) 
				georange(&p->bvol);
			range.include(&p->bvol);
		}
	}

	if (verbose)
		fprintf(stderr, "Output range: X=%f:%f Y=%f:%f\n", 
			range.xmin, range.xmax, range.ymin, range.ymax);
}

/// Recursive image building from octree
static void map_node(Octree_Data *od)
{
	// recursively process each valid child of node
	for(int ref=UPPER_LEFT_BACK; ref<NUMBER_OF_SPACE_PARTITIONS; ref++) {
		Octree_Data *kid = od->get_child((Octree_Child_Ref)ref);
		if (kid)
			map_node(kid);
	}

	// check all voxels at this node
	for (NodeSpec *ns = od->get_node_data(); ns; ns=ns->next) {
		// get voxel center in world coordinates
		double mctr[3], wctr[3];
		ns->get_global_center(mctr);
		MultPoints(mctr, m2w, wctr);

		// optionally convert to lat/long/el
		if (geo) {
			double lat, lon, el;
			georef->xyztolle(wctr[0], wctr[1], wctr[2],
					lat, lon, el);
			wctr[0] = lon;
			wctr[1] = lat;
			wctr[2] = el;
		}

		// corresponding map pixel
		int mapx = ROUND((wctr[0] - range.xmin) * scale);
		int mapy = ROUND((wctr[1] - range.ymin) * scale);

		// ignore if outside requested region
		if (mapx < 0 || mapx >= xres || mapy < 0 || mapy >= yres)
			continue;

		// update color map
		int n = ++VALID(mapx, mapy);
		if (rgbfile) {
			uchar pr, pg, pb;	// previous color
			wrgbmap->get_color(mapx, mapy, &pr, &pg, &pb);
			int nr, ng, nb;	// new color
			ns->get_color(&nr, &ng, &nb);
			wrgbmap->set_color(mapx, mapy, 	// blend colors
				((int)pr*(n-1) + nr) / n,
				((int)pg*(n-1) + ng) / n,
				((int)pb*(n-1) + nb) / n);
		}

		// update height map if this is highest point in pixel so far
		double height = wctr[2] * zscale;
		if (height > wzmap->get_float(mapx, mapy)) {
			wzmap->set_float(height, mapx, mapy);
		}
	} 
}

/// Build working map from one input patch.
static void one_map(int tree)
{
	ObjNode *node = forest.get_child(tree);
	SfcModel *sfc = (SfcModel *)node->get_object();

	if (sfc == NULL) {
		// Forest patch not yet loaded.
		// Skip if it's outside the selected volume
		Patch *p = (Patch *)node;
		if (!range.overlaps(&p->bvol))
			return;

		// okay, load the voxel data
		FILE_Dataport fp;
		p->parse_reference(&fp);
		sfc = (SfcModel *)node->get_object();
	}

	if (verbose)
		fprintf(stderr, "Doing patch %d (%s)\n", 
			tree, node->get_name() ? node->get_name() : "noname");

	// setup node's world and geo transforms
	node->GetTransformationMatrix(m2w);
	georef = forest.get_geo_data(tree);

	// initialize image Z to minimum height
	int x, y;
	for (y=0; y<yres; y++) {
		for (x=0; x<xres; x++)
			wzmap->set_float(empty, x, y);
	}

	// initialize cell count for validation and color averaging
	memset(npoints, 0, xres*yres*sizeof(int));
		
	// fill in image and color data
	map_node(sfc->get_data());		

	// done with this input model
	// (but careful, as forest destructor wants to delete children)
	delete sfc;
	node->set_object(NULL);
}

/// Merge working map from one forest patch into output map
static void merge_map(int tree)
{
	for (int y=0; y<yres; y++) {
		for (int x=0; x<xres; x++) {
			double height = wzmap->get_float(x, y);
			if (height > zmap->get_float(x, y)) {
				// higher, take this Z and color
				zmap->set_float(height, x, y);
				uchar r, g, b;
				wrgbmap->get_color(x, y, &r, &g, &b);
				rgbmap->set_color(x, y, r, g, b);
			}

			// also update lowest point for delta Z histogram
			if (dzmap && height != empty &&
					height < dzmap->get_float(x, y))
				dzmap->set_float(height, x, y);
		}
	}
}

// Compute and output histogram of variation in max Z values,
// as a measure of how well aligned the input octrees are.
static void dz_histogram()
{
	int bin[HIST_BINS];
	int zero_dz = 0;	// count of zero delta Z map cells
	int non_zero_dz = 0;	// total non-zero delta Z cells
	int x, y;

	memset(bin, 0, sizeof(bin));
	for (y=0; y<yres; y++) {
		for (x=0; x<xres; x++) {
			float z = zmap->get_float(x,y);
			if (z == empty)	// empty cell
				continue;
			z -= dzmap->get_float(x,y);
			if (z == 0.0)
				zero_dz++;
			else {
				non_zero_dz++;
				int bnum = int(z * (HIST_BINS-1) / hist_dz);
				if (bnum >= HIST_BINS)
					bin[HIST_BINS-1]++;
				else
					bin[bnum]++;
			}
		}
	}
	fprintf(stderr, "Delta Z histogram, bin 99 = dZ of %f and higher:\n", 
		hist_dz);
	fprintf(stderr, "(%d of the %d covered cells had nonzero dZ)\n",
		non_zero_dz, non_zero_dz + zero_dz);
	for (int i=0; i<HIST_BINS/4; i++)
		fprintf(stderr, "%2d: %6d   %2d: %6d   %2d: %6d   %2d: %6d\n",
			i, bin[i], 
			i+HIST_BINS/4, bin[i+HIST_BINS/4],
			i+HIST_BINS/2, bin[i+HIST_BINS/2], 
			i+3*HIST_BINS/4, bin[i+3*HIST_BINS/4]);

#if 0
	// testing, dump a delta Z image
	for (y=0; y<yres; y++) {
		for (x=0; x<xres; x++) {
			float z = zmap->get_float(x,y);
			if (z == empty)		// empty cell
				z = -0.001;	// mark as empty
			else
				z -= dzmap->get_float(x,y);
			dzmap->set_float(z, x, y);
		}
	}
	Image dzimg;
	dzimg.create_file_type(VIC_REAL_FILE_ID, "deltaz.img");
	dzimg.set_data(dzmap);
	dzimg.write();
#endif
}

/// Accumulate interpolation for one pixel
static void accum(float acc[], int x, int y)
{
	uchar r, g, b;

	acc[0] += zmap->get_float(x, y, 1);
	if (rgbfile) {
		rgbmap->get_color(x, y, &r, &g, &b);
		acc[1] += r;
		acc[2] += g;
		acc[3] += b;
	}
}

// Interpolate 8x8 neighbors to update hole cell.
// Return absolute value of change from previous Z estimate.
// Assuming x/y not at edge of image
static float interpolate(int x, int y)
{
	float acc[4];	// accumulators
	float zold = zmap->get_float(x, y, 1);
	int n = 0;
	acc[0] = acc[1] = acc[2] = acc[3] = 0.0f;

	accum(acc, x-1, y-1);
	accum(acc, x,   y-1);
	accum(acc, x+1, y-1);
	accum(acc, x-1, y);
	accum(acc, x+1, y);
	accum(acc, x-1, y+1);
	accum(acc, x,   y+1);
	accum(acc, x+1, y+1);

	// store accumulated results
	acc[0] /= 8.0f;
	acc[1] /= 8.0f;
	acc[2] /= 8.0f;
	acc[3] /= 8.0f;
	zmap->set_float(acc[0], x, y, 1);
	rgbmap->set_color(x, y, (acc[1] > 255.0) ? 255 : (int)acc[1],
				(acc[2] > 255.0) ? 255 : (int)acc[2],
				(acc[3] > 255.0) ? 255 : (int)acc[3]);
	return fabs(acc[0] - zold);
}

// Build second band of height map as interpolated version of first band.
static void interpolate_map()
{
	int x, y;
	int num_holes = 0;
	int num_interp = 0;

	// Create map of valid/invalid pixels, and
	// initialize filtered map from unfiltered map
	int xmax = xres-1;
	int ymax = yres-1;
	vmap = new unsigned char[(xres-2)*(yres-2)];

	unsigned char *p = vmap;
	for (y=1; y<ymax; y++) {
		for (x=1; x<xmax; x++, p++) {
			float z = zmap->get_float(x,y,0);
			if (z == empty) {
				*p = 0;
				num_holes++;
			} else {
				*p = 1;
				zmap->set_float(z, x, y, 1);
			}
		}
	}

	if (verbose)
		fprintf(stderr, "Interpolating %d hole(s)...\n", num_holes);

	// interpolate across horizontal hole spans
	p = vmap;
	for (y=1; y<ymax; y++) {
		int span_left = 0;	// start of hole span if >= 0
		for (x=1; x<xmax; x++, p++) {
			if (*p) {	// non-hole
				if (span_left > 0) {	
					// ends a proper hole span
					float zleft = 
						zmap->get_float(span_left, y);
					float zright = zmap->get_float(x, y);
					float dz = (zright - zleft) / 
							(x - span_left);
					for (int xx=span_left+1; xx<x; xx++) {
						zleft += dz;
						zmap->set_float(zleft, 
							xx, y, 1);
					}
					span_left = -1;
				} else if (span_left == 0) {
					// hole to left edge
					float zright = zmap->get_float(x, y);
					for (int xx=0; xx<x; xx++)
						zmap->set_float(zright,
							xx, y, 1);
					span_left = -1;
				}
			} else {	// hole
				if (span_left < 0)
					span_left = x-1; // starts a span
			}
		}
		if (span_left > 0) {	// hole to right edge
			float zleft = zmap->get_float(span_left, y);
			for (int xx=span_left+1; xx<xres; xx++)
				zmap->set_float(zleft, xx, y, 1);
		}
	}

	// interpolate across vertical hole spans, merging
	for (x=1; x<xmax; x++) {
		p = vmap + x-1;
		int span_top = 0;	// start of hole span if >= 0
		for (y=1; y<ymax; y++, p += xmax-1) {
			if (*p) {	// non-hole
				if (span_top > 0) {	
					// ends a proper hole span
					float ztop = 
						zmap->get_float(x, span_top);
					float zbot = zmap->get_float(x, y);
					float dz = (zbot - ztop) / 
							(y - span_top);
					for (int yy=span_top+1; yy<y; yy++) {
						ztop += dz;
						zmap->set_float((ztop +
							zmap->get_float(x,yy,1)) / 2.0, 
							x, yy, 1);
					}
					span_top = -1;
				} else if (span_top == 0) {
					// hole to top edge
					float zbot = zmap->get_float(x, y);
					for (int yy=0; yy<y; yy++)
						zmap->set_float((zbot +
							zmap->get_float(x,yy,1)) / 2.0,
							x, yy, 1);
					span_top = -1;
				}
			} else {	// hole
				if (span_top < 0)
					span_top = y-1; // starts a span
			}
		}
		if (span_top > 0) {	// hole to bottom edge
			float ztop = zmap->get_float(x, span_top);
			for (int yy=span_top+1; yy<yres; yy++)
				zmap->set_float((ztop + zmap->get_float(x,yy,1)) / 2.0,
					x, yy, 1);
		}
	}


	// iterate until convergence
	int i;
	for (i=0; i<max_interp; i++) {
fprintf(stderr, "Interp loop %d\n", i);
		float max_dz = 0.0;
		float sum_dz = 0.0;

		// reset edges from next-to-edges
		uchar r, g, b;
		for (y=0; y<yres; y++) {
			zmap->set_float(zmap->get_float(1, y, 1), 0, y, 1);
			zmap->set_float(zmap->get_float(xres-2, y, 1), 
								xres-1, y, 1);
			rgbmap->get_color(1, y, &r, &g, &b);
			rgbmap->set_color(0, y, r, g, b);
			rgbmap->get_color(xres-2, y, &r, &g, &b);
			rgbmap->set_color(xres-1, y, r, g, b);
		}
		for (x=0; x<xres; x++) {
			zmap->set_float(zmap->get_float(x, 1, 1), x, 0, 1);
			zmap->set_float(zmap->get_float(x, yres-2, 1), 
								x, yres-1, 1);
			rgbmap->get_color(x, 1, &r, &g, &b);
			rgbmap->set_color(x, 0, r, g, b);
			rgbmap->get_color(x, yres-2, &r, &g, &b);
			rgbmap->set_color(x, yres-1, r, g, b);
		}

		p = vmap;
		for (y=1; y<ymax; y++) {
			for (x=1; x<xmax; x++, p++) {
				if (*p == 0) {		// was a hole
					float dz = interpolate(x, y);
					sum_dz += dz;
					if (dz > max_dz)
						max_dz = dz;
				}
			}
		}
fprintf(stderr, "Max dz=%f avf=%f\n", max_dz, sum_dz / num_holes);
		if (max_dz <= 0.01)
			break;
	}
}

int main(int argc, char **argv)
{
	int i;
	int x, y;

	for (i=1; i<argc; i++) {
		if(!strcmp(argv[i],"-i") && i+1 < argc) {
			infile = argv[++i];
		} else if(!strcmp(argv[i],"-o") && i+1 < argc) {
			htfile = argv[++i];
			if (i+1<argc && isdigit(argv[i+1][0]))
				ztype = atoi(argv[++i]);
		} else if(!strcmp(argv[i],"-c") && i+1 < argc) {
			rgbfile = argv[++i];
			if (i+1<argc && isdigit(argv[i+1][0]))
				rgbtype = atoi(argv[++i]);
		} else if(!strcmp(argv[i],"-x") && i+1 < argc) {
			xres = atoi(argv[++i]);
		} else if(!strcmp(argv[i],"-g")) {
			geo = TRUE;
		} else if(!strcmp(argv[i],"-v")) {
			verbose = TRUE;
		} else if(!strcmp(argv[i],"-r") && i+4<argc) {
			range.space = SUMMITT_OBJECT_SPACE;
			range.xmin = atof(argv[++i]);
			range.xmax = atof(argv[++i]);
			range.ymin = atof(argv[++i]);
			range.ymax = atof(argv[++i]);
			range.zmin = -FLT_MAX;
			range.zmax = FLT_MAX;
		} else if(!strcmp(argv[i],"-refsite") && i+1 < argc) {
			refsite = atoi(argv[++i]);
		} else if(!strcmp(argv[i],"-a") && i+1 < argc) {
			max_interp = atoi(argv[++i]);
		} else if(!strcmp(argv[i],"-format")) {
			fprintf(stderr, "\nOutput file type options:\n");
			int n=0;
			for (i=1;i<NUM_FILE_TYPES;i++) {
				if (file_type_access[i] & FILE_WRITE) 
					fprintf(stderr, "%3d. %-30s%s",
						i, file_type_name[i],
						(++n) & 1 ? "       " : "\n");
			}
			fprintf(stderr, "\n");
			exit(1);
		} else if(!strcmp(argv[i],"-hist") && i+1 < argc) {
			hist_dz = atof(argv[++i]);
		} else {
			fprintf(stderr, "Unknown argument %s\n", argv[i]);
			fprintf(stderr, usage, argv[0]);
			exit(1);
		}
	}

	if (htfile == NULL) {
		fprintf(stderr, "Required argument missing\n");
		fprintf(stderr, usage, argv[0]);
		exit(1);
	}

	// read in input - octree, surface model, or forest
	FILE_Dataport fp;
	if(infile) {
		if (!fp.ropen(infile)) {
			fprintf(stderr, 
				"%s: Whoops - Can't open %s for reading\n", 
				argv[0], infile);
			exit(1);
		}
	} else {
		fp.open(stdin);
		infile = "stdin";
	}

	if (verbose)
		fprintf(stderr,"Loading input data\n");

 	// check input file type
 	char token[4096];
	Patch *p;
 	get_next_token(&fp, token);
	if (!strcmp(token, "GRP_V1")) {		// input is a forest
		// load patch headers but not voxel data
		if (!forest.parse_in(&fp, FALSE))
			exit(1);
		fp.close();

	} else {	// input must be sfc model or octree
		if (!strcmp(token, "SFC_MODEL_V1"))
		 	get_next_token(&fp, token);
		if (strcmp(token, "OCTREE_V1")) {
			fprintf(stderr, "%s: Whoops - unknown input "
				" type token >%d<\n", argv[0], token);
			exit(1);
		}
		SfcModel *sfc = new SfcModel;
		// read in octree (ignore any mesh)
		if (!sfc->Octree::parse_in(&fp))
			exit(1);
		fp.close();
		// setup Patch (world transform is identity)
		p = new Patch;
		p->set_object(sfc);
		sfc->freeze_xform();	// no changes, cache matrices
		p->update_bvol();
		// put patch into a single-octree forest
		forest.add_child(p);
	}

	ntrees = forest.get_num_children();
	if (verbose)
		fprintf(stderr,"Read %d surface(s)\n", ntrees);

	// don't crash trying to access first patch of empty input forest
	if (ntrees < 1) {
		fprintf(stderr, "%s: Whoops, input forest is empty!\n",
			argv[0]);
		exit(1);
	}

	// if want georeferenced output, make sure the forest is georef'd
	if (geo && forest.get_child(0)->get_geo_data() == NULL) {
		fprintf(stderr, "%s: Whoops - input forest isn't georeferenced\n", 
			argv[0]);
		exit(1);
	}

        if (refsite < 0) {
                // find "current site" = highest site index in octree
                refsite = 0;
                for (i=0; i<ntrees; i++) {
                        p = forest.get_patch(i);
                        if (p->site > refsite)
                                refsite = p->site;
                }
		if (verbose)
			fprintf(stderr, "Current site = site %d\n", refsite);
        }

        double rsvec[3] = { 0.0, 0.0, 0.0 };
        if (refsite) {
                // get world-to-site vector for the requested site
                // (last patch to use that site)
                for (int i=ntrees - 1; i >= 0; i--) {
                        p = forest.get_patch(i);
                        if (p->site == refsite) {
                                vector_copy(p->site_vector, rsvec);
                                break;
                        }
                }
        }

	// if no region specified, use bounding volume of input
	if (range.space == 0) {
		forest_range();		// range of models in world space
	} else if (refsite) {
		// translate specified site range to world range
		range.xmin -= rsvec[0];
		range.xmax -= rsvec[0];
		range.ymin -= rsvec[1];
		range.ymax -= rsvec[1];
	}

	// look at roll pointing in first patch to see if +Z is up or down
	p = forest.get_patch(0);
	if (p->xrot < -90 || p->xrot > 90)
		zscale = -1.0;

	// Determine size and scaling for output images.
	// Mapping is (coord - min) * scale = pixel.
	// Map xmin -> zero, xmax -> xres-1
	scale = (xres-1) / (range.xmax - range.xmin);
	// Map ymin -> zero; choose yres so ymax -> yres-1
	yres = ROUND((range.ymax - range.ymin) * scale) + 1;
	if (verbose)
		fprintf(stderr, "xres=%d yres=%d scale=%f\n", 
			xres, yres, scale);
	// ** disabled - leave Z values in world units
	// ** zscale *= scale;	// merge scaling and flip option

	// allocate output image data, 2 bands if interpolation is desired
	zimg.create_data_type(FLOAT_DATA);
	zmap = zimg.get_data();
	zmap->allocate(xres, yres, max_interp ? 2 : 1);

	rgbimg.create_data_type(UCHAR_DATA);
	rgbmap = rgbimg.get_data();
	rgbmap->allocate(xres, yres, 3);

	// allocate cell counters for validation and color averaging
	npoints = new int[xres*yres];

	// if multiple input models, create working maps
	if (ntrees > 1) {
		wzmap = new floatData;
		wzmap->allocate(xres, yres, 1);
		wrgbmap = new ucharData;
		wrgbmap->allocate(xres, yres, 3);

		// and initialize merged maps
		for (y=0; y<yres; y++) {
			for (x=0; x<xres; x++) {
				zmap->set_float(empty, x, y);
				rgbmap->set_color(x, y, 0, 0, 0);
			}
		}

		// allocate min height map for delta Z histogram
		if (hist_dz) {
			dzmap = new floatData;
			dzmap->allocate(xres, yres, 1);
			for (y=0; y<yres; y++) {
				for (x=0; x<xres; x++)
					dzmap->set_float(FLT_MAX, x, y);
			}
		}

	} else {
		// link work maps to output maps
		wzmap = zmap;
		wrgbmap = rgbmap;
	}

	// build height map for each forest patch
	for (i=0; i<ntrees; i++) {
		one_map(i);
		if (ntrees > 1)
			merge_map(i);
	}

	// optionally generate delta Z histogram 
	// (before mucking with Z values)
	if (dzmap)
		dz_histogram();
	
	// offset Z values from world to site frame
	if (rsvec[2]) {
		for (y=0; y<yres; y++) {
			for (x=0; x<xres; x++) {
				float z = zmap->get_float(x, y);
				if (z != empty)
					zmap->set_float(z + rsvec[2], x, y);
			}
		}
	}

	// if +Z is down, flip Z values to match coordinate frame
	if (zscale < 0.0) {
		for (y=0; y<yres; y++) {
			for (x=0; x<xres; x++) {
				zmap->set_float(-zmap->get_float(x, y), x, y);
			}
		}
		empty = -empty;	// flip "empty" indication
	}

	// optionally interpolate to fill holes in the map
	if (max_interp)
		interpolate_map();

	// write merged output images
	if (verbose) 
		fprintf(stderr, "Writing height map %s\n", htfile);
	// include transform info in (VICAR) header (site frame)
	char buf[100];
	sprintf(buf, "X_AXIS_MINIMUM=%f  Y_AXIS_MINIMUM=%f  MAP_SCALE=%f",
			range.xmin + rsvec[0], range.ymin + rsvec[1], 
			1.0/scale);
	zimg.create_file_type(ztype, htfile);
	zimg.set_comments(buf);
	zimg.write();

	if (rgbfile) {
		if (verbose) 
			fprintf(stderr, "Writing color map %s\n", rgbfile);
		rgbimg.write(rgbfile, rgbtype);
	}

	return 0;
}
