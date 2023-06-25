// fstquad.C 1.5 05/01/25 10:16:00
// Splits a forest into a quadtree list of tiles, creating an
// Inventor-format model referencing the files.

#include "forest.h"

// subvolume IDs
enum {	GROUP_UL, GROUP_UR, GROUP_LR, GROUP_LL,
	LEAF_UL, LEAF_UR, LEAF_LR, LEAF_LL };

// Callback function pointer.
typedef void (*FQCB)(Range *vol, int subvol_id);

// recursive division
static void split(Forest *fst, double tilesize, FQCB callback,
			Range *bvol, double area, int group_id)
{
	// find smallest voxel resolution of an overlapping patch
	double min_res = FLT_MAX;
	for (int i=0; i<fst->get_num_children(); i++) {
		Patch *p = fst->get_patch(i);
		if (bvol->overlaps(&p->bvol)) {
			double r = p->get_res();
			if (r < min_res)
				min_res = r;
		}
	}

	// estimated number of voxels that could go into this tile
	// (may be huge!)
	double nvoxels = area / (min_res * min_res);

	// small enough to output this tile as a quadtree leaf node?
	if (nvoxels <= tilesize) {
		callback(bvol, group_id + LEAF_UL);
		return;
	}

	// need to split this tile into four subtiles
	callback(bvol, group_id);	// group marker

	area /= 4.0;
	double xc = 0.5 * (bvol->xmin + bvol->xmax);
	double yc = 0.5 * (bvol->ymin + bvol->ymax);
	Range subvol = *bvol;
	subvol.xmax = xc;
	subvol.ymax = yc;
	split(fst, tilesize, callback, &subvol, area, GROUP_UL);
	subvol.xmin = xc;
	subvol.xmax = bvol->xmax;
	split(fst, tilesize, callback, &subvol, area, GROUP_UR);
	subvol.ymin = yc;
	subvol.ymax = bvol->ymax;
	split(fst, tilesize, callback, &subvol, area, GROUP_LR);
	subvol.xmin = bvol->xmin;
	subvol.xmax = xc;
	split(fst, tilesize, callback, &subvol, area, GROUP_LL);
}

// Create quadtree list of terrain tiles given a forest and
// a nominal number of voxels per tile.
// For each tile, callback is invoked with the bounding volume,
// and either TRUE for a leaf node or FALSE for a subdivided tile.
// If input bvol is null, use overall forest volume.
//
// (This service could be used independently of the Inventor mesh
// generation application included below.)
void forest_quadtree(Forest *fst, double tilesize, FQCB callback, Range *bvol)
{
	// find overall bounding volume, check for valid resolution data
	Range bv;
	bv.empty();
	for (int i=0; i<fst->get_num_children(); i++) {
		Patch *p = fst->get_patch(i);
		if (p->get_res() <= 0.0) {
			fprintf(stderr, "forest_quadtree: patch %d "
				"has zero res, punting\n", i);
			return;
		}
		bv.include(&p->bvol);
	}
	if (bvol == NULL) 	// use default overall volume?
		bvol = &bv;

	// find region size
	double area = (bvol->xmax - bvol->xmin) * (bvol->ymax - bvol->ymin);

	// start recursive subdivision
	split(fst, tilesize, callback, bvol, area, GROUP_UL);
}

//--------------------------------------
// Sample usage, writes quadtree as Inventor file,
// with bounding volume in comments

static int depth;
static char subvol[100];	// current subvolume ID stack
static int tile_count;		// diagnostics

static void node_cb(Range *r, int id)
{
	while (++subvol[depth] > '4') {	// finish previous groups
		printf("}\n");
		subvol[depth--] = 0;
	}
	if (id < LEAF_UL) {	// new group
		printf("Group { # %s ", subvol);
		subvol[++depth] = '0';
		subvol[depth+1] = 0;
	} else {		// leaf
		printf("File { name \"tile%s.iv\" } # ", subvol);
		tile_count++;
	}
	printf("%f %f %f %f\n", r->xmin, r->xmax, r->ymin, r->ymax);
}

int main(int argc, char **argv)
{
	char *progname = argv[0];
	int  refsite = 0;		// reference site, 0=world, -1=latest

	if (argc > 2 && !strcmp(argv[1], "-refsite")) {
		refsite = atoi(argv[2]);
		argv += 2;
		argc -= 2;
	}

	if (argc != 3 && argc != 7) {
		fprintf(stderr, "usage: %s [-refsite n] forest tilesize "
			"[xmin xmax ymin ymax]\n"
			"refsite  = reference site index, 0=world, -1=current\n"
			"forest   = input forest\n"
			"tilesize = approximate size (# triangles) "
				"in each tile\n"
			"xylimits = bounding volume for selecting wedges\n",
			progname);
		return 1;
	}

	double tilesize = atof(argv[2]);
	if (tilesize < 4.0) {
		fprintf(stderr, "%s: Min tile size is 4\n", progname);
		return 1;
	}

	// read forest (but not octrees)
	Forest *fst = new Forest;
	FILE_Dataport *fp = new FILE_Dataport();
	if (!fp->ropen(argv[1])) {
		fprintf(stderr, "%s: unable to open forest %s for reading\n", 
			progname, argv[1]);
		return 1;
	}
	char token[4096];
	get_next_token(fp, token);
	if (!fst->parse_in(fp, FALSE)) {
		fprintf(stderr, "%s: error reading forest %s\n",
			progname, argv[1]);
		return 1;
	}

	if (refsite < 0) {
		// find "current site" = highest site index in octree
		refsite = 0;
		for (int i=0; i<fst->get_num_children(); i++) {
			Patch *p = fst->get_patch(i);
			if (p->site > refsite)
				refsite = p->site;
		}
	}

	double rsvec[3] = { 0.0, 0.0, 0.0 };
	if (refsite) {
		// get world-to-site vector for the requested site
		// (last patch to use that site)
		for (int i=fst->get_num_children() - 1; i >= 0; i--) {
			Patch *p = fst->get_patch(i);
			if (p->site == refsite) {
				vector_copy(p->site_vector, rsvec);
				break;
			}
		}
	}

	// start output
	printf("#Inventor V2.0 ascii\n# Forest %s quadtree tilesize=%lf\n",
		argv[1], tilesize);
	if (refsite)
		printf("Translation {translation %lf %lf %lf}\n",
			rsvec[0], rsvec[1], rsvec[2]);

	strcpy(subvol, "0");

	if (argc == 7) {	// specified volume
		Range bv;
		bv.xmin = atof(argv[3]);
		bv.xmax = atof(argv[4]);
		bv.ymin = atof(argv[5]);
		bv.ymax = atof(argv[6]);
		bv.zmin = -FLT_MAX;
		bv.zmax = FLT_MAX;
		forest_quadtree(fst, tilesize, node_cb, &bv);
	} else {
		forest_quadtree(fst, tilesize, node_cb, NULL);
	}

	// finish output
	while (depth) {
		printf("}\n");
		depth--;
	}

	fprintf(stderr, "%d tiles created\n", tile_count);
	return 0;
}
