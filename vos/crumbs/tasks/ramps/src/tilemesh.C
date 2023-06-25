// tilemesh.C 1.13 04/07/21 08:28:00
// Build multi-LOD triangle meshes for each tile (quadtree region)
// of a forest. Output is in Inventor format.
// Meshes are built directly from patches' XYZ (range) data
// rather than octrees, to allow clipping to tile edges based on
// range map connectivity. [No longer applies? ...]

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <time.h>
#include <stdlib.h>
#include <image/types/ranfile.h>
#include "summitt_func.h"
#define GLOBAL
#include "tilemesh.h"

static const char usage[] = 
"Usage: %s [-v] -i forest -q quadtree [-res min max]\n"
"  [-rf maxd] [-g maxgap] [-dir dname] [-s lod_scale]\n"
"  [-d discont] [-a maxang] [-rng min max] [-Xrmax max]\n"
"  [-nm] [-n norm_threshold] [-p max_proj_steps] [-acc levels]\n"
"Builds multi-LOD meshes for quadtree tiles of input forest\n\n"
"-v   = verbose output\n"
"-i   = input forest file\n"
"-q   = input quadtree subdivision (Inventor file from fstquad)\n"
"-res = resolution limits for output levels, as approximate\n"
"       number of triangles at lowest and highest detail.\n"
"       Default max = full input resolution, default min = 32.\n"
"-rf  = initial range filtering, max delta range\n"
"-g   = max gap in input range data to interpolate across per LOD, default=2\n"
"-dir = directory containing (links to) XYZ files for each forest\n"
"       patch, where XYZ filename is patch name.\n"
"-s   = set LOD switch threshold to scale / xres (default=400.0)\n"
"XYZ/Range processing options:\n"
"-d   = min normal angle in degrees discarded as discontinuity (default 60)\n"
"-a   = max degrees between face normal and camera direction (default 85)\n"
"-rng = min/max limits for valid range data (default 0-FLT_MAX)\n"
"       if min<0, limits (|min|,max) are normalized for 45deg FOV, and\n"
"       scaled according to the FOV of the current input.\n"
"-Xrmax = override max range limit for instrument X (product ID letter)\n"
"-nm  = no point merging/marching triangles, use simple meshes\n"
"-n   = max allowed angle in degrees between normals (default=90)\n"
"-p   = max steps to project new edge for extension (default=5)\n"
"-acc = accumulate levels on merged octrees before meshing (default=0)\n";

// worst case max LODs for one tile
#define MAX_LOD 20

// min XYZ resolution to be subsampled
#define MINRES 6	// so smallest map is 3x3 = 18 triangles

// Tile LOD resolution limits (# triangles), with defaults.
// Estimate # triangles for an X*Y range image as 1.5*X*Y.
static int minres = 32;
static int maxres = 9999999;

// Max delta range to include in initial range map filtering, 0=no filtering
static float rf_maxd = 0;

// Max XYZ data gap to interpolate across at each LOD, in pixels 
static int maxgap = 2;

// Directory where XYZ data is (or, more likely, containing links 
// to XYZ files). Files must be named the same as patch names.
static char *xyzdir;

// Scale factor from XYZ resolution for LOD switch threshold.
// Maybe ought to take FOV into account... or maybe not.
static double lod_scale = 400.0;

// Range (XYZ) -> octree parameters:

// Minimum cosine of angle between normal vectors. Smaller cosine
// (larger angle) indicates a discontinuity.
static double min_cos_norm = 0.5;	// default = 60 degrees

// Minimum cosine of angle between face normal and eye vector. Smaller
// cosine (larger angle) indicates invalid face triangle.
static double face_threshold = 0.08716;	// default = 85 degrees

// Limits for valid range data from XYZ files (camera to data point distance)
// If min_range is negative, limits are scaled based on current FOV
static double min_range = 0.0;
static double max_range = FLT_MAX;

// List of instrument-specific max range overrides,
// identified by product ID instrument letter code
#define NUM_IRANGE 10
static int num_irange = 0;
static char irange_inst[NUM_IRANGE];
static double irange_max[NUM_IRANGE];

// Marching triangles parameters: 

// Skip point merging, use simple meshes only?
static int no_merge = 0;

// Threshold of cosine of angle between normals to accept candidate
static double cos_threshold = 0.0;

// Max projection steps for candidate delaunay steps
static int max_proj_steps = 5;

// Octree levels to accumulate on merged data before meshing
static int accum_levels = 0;

// Is +Z down in world frame, not up?
int zdown;

// Track dynamic memory usage
void track_memory(const char *checkpoint)
{
	if (!verbose)
		return;
#if 0
	static long maxmem;
	struct mallinfo mi = mallinfo();
	long mem = (mi.usmblks + mi.uordblks) / 1024;
	if (mem > maxmem)
		maxmem = mem;
	fprintf(stderr, "Mem: cur=%8ldK max=%8ldK @ %s\n",
					mem, maxmem, checkpoint);
#endif
}

// Create octree for merging data points within a tile
static SfcModel *setup_octree(Range *bvol, int res)
{
	// depth is based on resolution (nominal number of triangles)
	SfcModel *oct = new SfcModel(int(log(double(res))));

	// transform maps -1:+1 model space to bvol object space
	// (careful, Z bvol is essentially infinite)
	double scale = bvol->xmax - bvol->xmin;
	if (bvol->ymax - bvol->ymin > scale)
		scale = bvol->ymax - bvol->ymin;

	// leave room for Z to fill octree model volume so searches work right
	scale *= 2;

	// Setup model-object transformation matrix
	oct->x = -(bvol->xmax + bvol->xmin) / scale;
        oct->y = -(bvol->ymax + bvol->ymin) / scale;
        oct->z = 0.0;
        oct->xrot = oct->yrot = oct->zrot = 0.0;
	// adjust scale for volume range -1 to +1
	oct->xscale = oct->yscale = oct->zscale = 0.5 * scale;

	return oct;
}

// Resample xyz data at 1/2 resolution, return allocated map; free input map.
// Return NULL if trouble
static ImageData *rng_resample(ImageData *old_xyz)
{
	int xres, yres;
	old_xyz->get_res(&xres, &yres);
	ImageData *new_xyz = NULL;
	if (xres >= MINRES && yres >= MINRES) {	// big enough to divide
		new_xyz = new floatData;
		if (new_xyz->allocate(xres/2, yres/2, 3)) {
			summitt_range_resample(old_xyz, new_xyz);
		} else {		// failed to allocate
			delete new_xyz;
			new_xyz = NULL;
		}
	}
	delete old_xyz;
	return new_xyz;
}

// Copy full-resolution XYZ data
static ImageData *copy_xyz(ImageData *old_xyz)
{
	int xres, yres;
	old_xyz->get_res(&xres, &yres);
	
	ImageData *new_xyz = new floatData;
	if (new_xyz->allocate(xres, yres, 3)) {
		summitt_range_copy(old_xyz, new_xyz);
		delete old_xyz;
		return new_xyz;
	}
	fprintf(stderr, "Can't allocate copy of XYZ map\n");
	delete new_xyz;
	return old_xyz;
}

// Clean up full-resolution XYZ data
static ImageData *filter_xyz(ImageData *old_xyz)
{
	int xres, yres;
	old_xyz->get_res(&xres, &yres);
	
	ImageData *new_xyz = new floatData;
	if (new_xyz->allocate(xres, yres, 3)) {
		summitt_range_filter(old_xyz, new_xyz, rf_maxd);
		delete old_xyz;
		return new_xyz;
	}
	fprintf(stderr, "Can't allocate filtered XYZ map\n");
	delete new_xyz;
	return old_xyz;
}

// Load XYZ range data corresponding to forest patch.
// First check if there's a cached range map,
// otherwise preprocess it (filter and resample to maxres) now. 
static ImageData *get_patch_xyz(Patch *p, int simple_mesh)
{
	char name[1024];
	int xres, yres;

	// cached filename
	sprintf(name, "%s/pp%s.ran", xyzdir, p->get_name());
	GeoImage img(name);
	if (img.get_res() > 0) {
		// got it, detach range data from temporary GeoImage object
		ImageData *xyz = img.get_data();
		img.set_data(NULL);
		return xyz;
	}

	fprintf(stderr, "No cached XYZ %s, preprocessing\n", name);
	img.free_file();

	// original XYZ is named (or linked to) directory + patch name
	sprintf(name, "%s/%s", xyzdir, p->get_name());
	img.read(name);
	if (img.get_res(&xres, &yres) < 1) {
		// try explicit RAN format
		img.read(name, RAN_FILE_ID);
		if (img.get_res(&xres, &yres) < 1) {
			fprintf(stderr, "Can't open patch XYZ file %s\n", name);
			exit(1);
		}
	}

	// detach range data from temporary GeoImage object
	ImageData *xyz = img.get_data();
	img.set_data(NULL);


	// optionally cleanup full-res XYZ data
	if (rf_maxd)
		xyz = filter_xyz(xyz);
    else
        xyz = copy_xyz(xyz);

	// reduce resolution to max output res,
	// allowing for merged data accumulation
	int limres = 2 * maxres;
	if (!simple_mesh && accum_levels > 0)
		limres *= (2 << accum_levels);
	while (xres * yres > limres) {
		if (verbose)
			fprintf(stderr, "Reducing patch %s initial res\n",
				p->get_name());
		if (maxgap)
			summitt_range_interpolate(xyz, maxgap);

		xyz = rng_resample(xyz);
		xres /= 2;
		yres /= 2;
	}

	// cache for reuse
	sprintf(name, "%s/pp%s.ran", xyzdir, p->get_name());
	img.set_data(xyz);
	img.write(name, RAN_FILE_ID);
	img.set_data(NULL);

	return xyz;
}

// Get point from XYZ map, return TRUE if valid
inline int get_point(ImageData *xyz, int x, int y, double p[3])
{
	p[0] = xyz->get_float(x, y, 0);
	if (p[0] < -90000.0)
		return FALSE;
	p[1] = xyz->get_float(x, y, 1);
	p[2] = xyz->get_float(x, y, 2);
	return TRUE;
}

// Update point in XYZ map. If p is NULL, set invalid
inline void set_point(ImageData *xyz, int x, int y, double *p)
{
	if (p == NULL) {
		xyz->set_float(-100000.0, x, y, 0);
	} else {
		xyz->set_float(p[0], x, y, 0);
		xyz->set_float(p[1], x, y, 1);
		xyz->set_float(p[2], x, y, 2);
	}
}

// Transform XYZ points for patch[i] to (corrected) site/world frame.
// XYZ data is either in uncorrected site frame defined by
// patch otrans/orot, or in camera frame (so otrans/orot = 0).
static void xform_world(PInfo *pi)
{
	Patch *p = pi->p;

	// original world-to-camera transform:
	ZMatrix xf1, xf2;
	ObjNode tmp;
	tmp.x = p->otrans[0];
	tmp.y = p->otrans[1];
	tmp.z = p->otrans[2];
	tmp.xrot = p->orot[0];
	tmp.yrot = p->orot[1];
	tmp.zrot = p->orot[2];
	tmp.GetObjToWorldTransform(xf1);
	MatInvert(xf1, xf2);

	// current camera-to-world transform:
	ZMatrix xform;
	p->GetObjToWorldTransform(xform);

	// concatenate
	MatPreMult(xform, xf2);

	int xres, yres;
	ImageData *xyz = pi->xyz;
	xyz->get_res(&xres, &yres);
	for (int y=0; y<yres; y++) {
		for (int x=0; x<xres; x++) {
			double p[3], px[3];
			if (!get_point(xyz, x, y, p))
				continue;	// invalid
			if (p[0]==0.0 && p[1]==0.0 && p[2]==0.0) {
				// replace with simpler "invalid" marker
				set_point(xyz, x, y, NULL);
				continue;
			}
			MultPoints(p, xform, px);
			set_point(xyz, x, y, px);
		}
	}
}

// Clip XYZ data to tile boundary + margin to save meshing time.
// Discarded points are set to invalid.
// ** maybe should clip at each resolution with appropriate margin?
static void clip_patch(ImageData *cur_xyz, Range *bvol)
{
	int xres, yres;
	cur_xyz->get_res(&xres, &yres);

	// expand clip boundaries by 1/4 tile's width/height
	Range cvol = *bvol;
	double margin = 0.25 * (cvol.xmax - cvol.xmin);
	cvol.xmin -= margin;
	cvol.xmax += margin;
	margin = 0.25 * (cvol.ymax - cvol.ymin);
	cvol.ymin -= margin;
	cvol.ymax += margin;

	int nclip = 0;	// count discarded points
	for (int y=0; y<yres; y++) {
		for (int x=0; x<xres; x++) {
			double p[3];
			if (!get_point(cur_xyz, x, y, p))
				continue;	// already invalid
			if (!cvol.in_range(p)) {
				// mark as invalid/clipped
				set_point(cur_xyz, x, y, NULL);
				nclip++;
			}
		}
	}

	if (verbose)
		fprintf(stderr, "Clipped %d pts\n", nclip);
}

// Save instrument-specific XYZ range limit
static void save_range(char id, double rmax)
{
	if (num_irange >= NUM_IRANGE) {
		fprintf(stderr, "tilemesh: too many instrument-specific "
			"range limits, max is %d\nIgnoring %c = %f\n",
			NUM_IRANGE, id, rmax);
		return;
	}
	irange_inst[num_irange] = id;
	irange_max[num_irange++] = rmax;
}

// Lookup instrument-specific max range
static double get_rmax(Patch *p, double def_rmax)
{
	// instrument ID = second letter of patch name (product ID)
	char *name = p->get_name();
	for (int i=0; i<num_irange; i++) {
		if (irange_inst[i] == name[1])
			return irange_max[i];
	}
	// not found, use default
	return def_rmax;
}

// Add points from XYZ range image (now in site frame) to octree.
// i = index in patch/xyz list.
// domesh = TRUE to build a simple triangle mesh using XYZ data connectivity
static void merge_points(SfcModel *sfc, int i, int domesh)
{
	Patch *p = plist[i].p;
	int flags = domesh ? R2O_MESH : 0;

	// tag voxels as coming from patch i using 1x1 RGB image
	ucharData vcolor;
	vcolor.allocate(1, 1, 1);
	vcolor.set_color(0, 0, (uchar)i, 0, 0);

	// determine range limits for this input
	double crmin, crmax;
        if (min_range < 0.0) {          // FOV-dependent
                crmin = -min_range * 45.0 / p->fov;
                crmax = max_range * 45.0 / p->fov;
        } else {                        // fixed limits
                crmin = min_range;
                crmax = max_range;
        }
	crmax = get_rmax(p, crmax);	// maybe instrument-specific

	// no transform, keep data in site frame
	double cam[3] = {p->x, p->y, p->z};
	range2octree(plist[i].xyz, &vcolor, cam, p->fov * TORADIANS, 
		crmin, crmax, NULL,
		flags, min_cos_norm, face_threshold, 1.0, sfc);
}

// Build triangle mesh from octree of merged input data
// using Marching Triangles algorithm. Returns pointer
// to static mesh object (or NULL on failure).
static Triangle_Model *merged_mesh(SfcModel *sfc)
{
	// Put octree into a trivial forest for MT interface
	Forest fst;
	ObjNode *node = new ObjNode;
	node->set_object(sfc);
	fst.add_child(node);

	// Invoke the marching triangles code
	MT_Mesh *mesh = make_triangle_model_from_forest(&fst,
		cos_threshold, max_proj_steps, accum_levels, FALSE, zdown);

	// discard construction edge data, but not faces or vertex list
	if (mesh)
		mesh->clean_up_edges();
	// don't destroy octree when tossing the forest
	node->set_object(NULL);

	return (Triangle_Model *)mesh;
}
// Create one tile's highest LOD mesh tree in OBJ format
static void make_OBJs(FILE *fp, Range *bvol)
{
        int num_tris;
        int i;
        int lod_res;    // X resolution at highest LOD
        int lod = 0;


        //!!!!ozp
        //fprintf(fp, "LOD {\n");

        // create simple mesh for this tile?
        int simple_mesh = npatch==1 || no_merge;

        int LODres = maxres;    // estimated resolution at this LOD

        // for now only highest LOD is being used for OBJ
        // will revisit it later as needed -ozp
        //for (lod=0; lod<MAX_LOD; lod++) {
                if (verbose)
                        fprintf(stderr, "Building OBJ \n");
                track_memory("start OBJ");

                // Create octree for merging data. If only one patch overlaps
                // this tile, a connectivity mesh is built into the sfcmodel
                // mesh member. Otherwise, a separate Marching Triangles mesh
                // will be built.
                SfcModel *sfc = setup_octree(bvol, LODres);

                PInfo *pi = plist;
                for (i=0; i<npatch; i++, pi++) {
                        if (lod == 0) { // first LOD (highest res)
                                pi->xyz = get_patch_xyz(pi->p, simple_mesh);
                                lod_res = pi->xyz->get_res();

                                // transform XYZ's to site frame for clipping
                                xform_world(pi);

                                // clip data to tile boundary + margin
                                // ** possible optimization TBD:
                                // ** if no points survive clipping, remove
                                // ** patch from this tile (or just delete xyz)
                                clip_patch(pi->xyz, bvol);

                        }
                        if (pi->xyz == NULL)    // too small, failure, etc.
                                continue;
                        // resample range data for this LOD
                        if (maxgap)
                                summitt_range_interpolate(pi->xyz, maxgap);

                        // merge to octree, optionally create mesh
                        merge_points(sfc, i, simple_mesh);
                }

                // extract point list (for printing simple mesh,
                // or for marching tris)
                if (sfc->get_data())
                        sfc->add_pt_list();

                Triangle_Model *mesh;
                if (simple_mesh)        // use simple mesh in sfcmodel
                        mesh = &(sfc->mesh);
                else                    // build mesh from merged points
                        mesh = merged_mesh(sfc);

                num_tris = mesh ? mesh->tri_count() : 0;

                if (verbose)
                        fprintf(stderr, "Writing mesh, "
                                "%d triangles before clipping\n", num_tris);
                //fprintf(fp, "Separator { # LevelOfDetail %d\n", lod);
                // output mesh; flip Marching triangles if +Z is down
                if (mesh && mesh->tri_list)
                        write_mesh_obj(fp, mesh, sfc, bvol, !simple_mesh && zdown);
                //fprintf(fp, "}\n");   // end of this level

                if (mesh)
                        mesh->clean_up();
                delete sfc;

        //}

        // clean up
        for (i=0; i<npatch; i++)
                delete plist[i].xyz;
        track_memory("Tile done");
} // end of make_obj

// Create one tile's multi-LOD mesh tree
static void make_LODs(FILE *fp, Range *bvol)
{
	int num_tris;
	int i, lod;
	int lod_res;	// X resolution at highest LOD


        //!!!!ozp
	//fprintf(fp, "LOD {\n");
	fprintf(fp, "LevelOfDetail {\n");
        fprintf(fp, "screenArea [ 256000, 54000, 16000, 4000, 1000 ]\n");


        //!!!!ozp
        /*
	// Need center coordinates of patch for LOD switching to work
	// correctly. For X and Y, use center of tile; for Z,
	// use average center of component patch bounding Z volumes.
	double zcenter = 0.0;
	for (i=0; i<npatch; i++)
		zcenter += plist[i].p->bvol.zmin + plist[i].p->bvol.zmax;
	fprintf(fp, "center %f %f %f\n",
		0.5 * (bvol->xmin + bvol->xmax), 
		0.5 * (bvol->ymin + bvol->ymax),
		0.5 * zcenter / npatch);
        
	// LOD "range" field needs to go before the kids [why?], but
	// I don't have info to build it yet. So leave some space
	// to come back and fill it in later.
	long range_pos = ftell(fp);
	enum { RANGE_LEN = 10*MAX_LOD };
	char range_field[RANGE_LEN+2];
	fseek(fp, RANGE_LEN+2, SEEK_CUR);
        */
	// create simple mesh for this tile?
	int simple_mesh = npatch==1 || no_merge;

	int LODres = maxres;	// estimated resolution at this LOD

	for (lod=0; lod<MAX_LOD; lod++) {
		if (verbose)
			fprintf(stderr, "Building LOD %d\n", lod);
		track_memory("start LOD");

		// Create octree for merging data. If only one patch overlaps
		// this tile, a connectivity mesh is built into the sfcmodel
		// mesh member. Otherwise, a separate Marching Triangles mesh
		// will be built.
		SfcModel *sfc = setup_octree(bvol, LODres);

		PInfo *pi = plist;
		for (i=0; i<npatch; i++, pi++) {
			if (lod == 0) {	// first LOD (highest res)
				pi->xyz = get_patch_xyz(pi->p, simple_mesh);
				lod_res = pi->xyz->get_res();

				// transform XYZ's to site frame for clipping
				xform_world(pi);

				// clip data to tile boundary + margin
				// ** possible optimization TBD:
				// ** if no points survive clipping, remove 
				// ** patch from this tile (or just delete xyz)
				clip_patch(pi->xyz, bvol);
				
			} else if (pi->xyz) {
				pi->xyz = rng_resample(pi->xyz);
			}
			if (pi->xyz == NULL)	// too small, failure, etc.
				continue;

			// resample range data for this LOD
			if (maxgap)
				summitt_range_interpolate(pi->xyz, maxgap);

			// merge to octree, optionally create mesh
			merge_points(sfc, i, simple_mesh);
		}

		if (verbose == 2 && npatch > 1) {	// dump octree
			static int octid;
			char buf[32];
			sprintf(buf, "tm%04d.oct", ++octid);
			FILE_Dataport fp;
			fp.wopen(buf);
			((Octree *)sfc)->parse_out(&fp);
			fp.close();
		}

		// extract point list (for printing simple mesh, 
		// or for marching tris)
		if (sfc->get_data())
			sfc->add_pt_list();

		Triangle_Model *mesh;
		if (simple_mesh)	// use simple mesh in sfcmodel
			mesh = &(sfc->mesh);
		else			// build mesh from merged points
			mesh = merged_mesh(sfc);

		num_tris = mesh ? mesh->tri_count() : 0;
		// don't output bogus LOD below top level
		if (lod > 0 && num_tris < minres/2) {
			if (verbose)
				fprintf(stderr, "Discarding level, "
					"only %d triangles\n", num_tris);
			lod--;
			if (mesh)
				mesh->clean_up();
			delete sfc;
			break;
		}
		if (verbose)
			fprintf(stderr, "Writing mesh, "
				"%d triangles before clipping\n", num_tris);
		fprintf(fp, "Separator { # LevelOfDetail %d\n", lod);
		// output mesh; flip Marching triangles if +Z is down
		if (mesh && mesh->tri_list)
			write_mesh(fp, mesh, sfc, bvol, !simple_mesh && zdown);
		fprintf(fp, "}\n");	// end of this level

		if (mesh)
			mesh->clean_up();
		delete sfc;
		
		// estimate resolution for next level
		LODres = num_tris / 2;
		// reached desired coarsest LOD yet?
		if (LODres < minres)
			break;
	}

//!!!!ozp
/*
	// For consistent LOD switching, set distance thresholds 
	// based purely on XYZ resolution.
	strcpy(range_field, "range [");
	for (i=0; i<lod; i++) {
		char s[64];
		sprintf(s, "%f,", lod_scale / lod_res);
		strcat(range_field, s);
		lod_res /= 2;
	}
	// man page says need (#LODs-1) ranges, but pf converter chokes
	// unless have #LOD...
	strcat(range_field,"1.0E10");
*/
        //!!!!ozp
	// go back and write range field at start of LOD node
	//if (fseek(fp, range_pos, SEEK_SET) < 0)
	//	perror("tilemesh seek failed");
	//fprintf(fp, "%-*s]\n", RANGE_LEN, range_field);
	//fseek(fp, 0, SEEK_END);
	fprintf(fp, "}\n");	// finish LOD node

	// clean up
	for (i=0; i<npatch; i++)
		delete plist[i].xyz;
	track_memory("Tile done");
}

// Create one tile from forest data overlapping bounding volume
static void make_tile(Forest *fst, char *name, char *patch_name, Range *bvol)
{
	// find all the forest patches that overlap this volume
	npatch = 0;
	int i;
        char name_obj[1024];
	for (i=0; i<fst->get_num_children(); i++) {
		Patch *p = fst->get_patch(i);
		if (bvol->overlaps(&p->bvol)) {
			if (npatch >= MAX_PATCH) {
				fprintf(stderr, "Too many patches overlap "
					"volume for %s, only using the first "
					"%d patches\n", name, MAX_PATCH);
				break;
			}
			plist[npatch++].p = p;
		}
	}

	if (verbose) {
		time_t t = time(NULL);
		fprintf(stderr, "\nBuilding tile %s, found %d patch(es) %s", 
			name, npatch, ctime(&t));
		bvol->dump(stderr, "Tile volume");
	}

    //make sure file permissions are set right
    umask(002);
	// start output
	FILE *tfile = fopen(name, "w");
        // construct name for OBJ file
        strcpy(name_obj, patch_name);
        strcat(name_obj, "_");
        // remove .iv extension from filename
        strncat(name_obj, name, strlen(name)-3);
        strcat(name_obj, ".obj");
        //make sure file permissions are set right
        umask(002);
        FILE *tfile_obj = fopen(name_obj,"w");
	if (tfile == NULL) {
		fprintf(stderr, "Can't create output file %s\n", name);
		exit(1);
	}
        if (tfile_obj == NULL) {
                fprintf(stderr, "Can't create OBJ output file %s\n", name);
                exit(1);
        }

        fprintf(tfile_obj, "mtllib %s%s\n", patch_name, ".mtl");
        fprintf(tfile_obj, "usemtl material_1\n");

	fprintf(tfile, "#Inventor V2.0 ascii\n");
		//"ShapeHints { vertexOrdering COUNTERCLOCKWISE shapeType SOLID }\n"
//#ifdef TEX_PER_VERTEX
		// this would be better, but for performer IV loader bug...
                // !!!! We don't do Performer anymore -ozp
		//"TextureCoordinateBinding { value PER_VERTEX }\n");
        // !!!! Works for single patch, add loop for multi patch case -ozp
        fprintf(tfile, "DEF Texture_%s\n Texture2 {filename \"%s.rgb\" "
		"wrapS CLAMP wrapT CLAMP}\n", fst->get_patch(0)->get_name(), fst->get_patch(0)->get_name());
        fprintf(tfile, "ShapeHints { vertexOrdering COUNTERCLOCKWISE shapeType SOLID }\n"
                "TextureCoordinateBinding { value PER_VERTEX }\n");

	if (npatch) {	// tile contains at least one source patch
		make_LODs(tfile, bvol);
                make_OBJs(tfile_obj, bvol);
        }
	fclose(tfile);
        fclose(tfile_obj);
}

int main(int argc, char **argv)
{
	int i;
	char *fstfile = NULL;
	char *quadfile = NULL;

	for (i=1; i<argc; i++) {
		if(!strcmp(argv[i],"-i") && i+1 < argc) {
			fstfile = argv[++i];
		} else if(!strcmp(argv[i],"-q") && i+1 < argc) {
			quadfile = argv[++i];
		} else if (!strcmp(argv[i], "-rf") && i+1 < argc) {
			rf_maxd = atof(argv[++i]);
		} else if(!strcmp(argv[i],"-g") && i+1 < argc) {
			maxgap = atoi(argv[++i]);
		} else if(!strcmp(argv[i],"-res") && i+2 < argc) {
			minres = atoi(argv[++i]);
			maxres = atoi(argv[++i]);
			if (minres < 2) {
				fprintf(stderr, usage, argv[0]);
				return 1;
			}
		} else if(!strcmp(argv[i],"-dir") && i+1 < argc) {
			xyzdir = argv[++i];
		} else if(!strcmp(argv[i],"-s") && i+1 < argc) {
			lod_scale = atof(argv[++i]);
		} else if(!strcmp(argv[i],"-v")) {
			verbose++;

		// range->octree options
		} else if (!strcmp(argv[i], "-a") && i+1 < argc) {
			face_threshold = cos(TORADIANS * atof(argv[++i]));
		} else if (!strcmp(argv[i], "-d") && i+1 < argc) {
			min_cos_norm = cos(TORADIANS * atof(argv[++i]));
		} else if(!strcmp(argv[i], "-rng") && i+2 < argc) {
			min_range = atof(argv[++i]);
			max_range = atof(argv[++i]);
		} else if(argv[i][0] == '-' && !strcmp(argv[i]+2, "rmax") && 
				i+1 < argc) {
			save_range(argv[i][1], atof(argv[++i]));

		// marching triangles options
		} else if(!strcmp(argv[i], "-nm")) {
			no_merge = 1;
		} else if(!strcmp(argv[i], "-n") && i+1 < argc) {
			cos_threshold = cos(TORADIANS * atof(argv[++i]));
		} else if(!strcmp(argv[i], "-p") && i+1 < argc) {
			max_proj_steps = atoi(argv[++i]);
		} else if(!strcmp(argv[i], "-acc") && i+1 < argc) {
			accum_levels = atoi(argv[++i]);

		} else {
			fprintf(stderr, usage, argv[0]);
			return 1;
		}
	}

	if (fstfile == NULL || quadfile == NULL) {
		fprintf(stderr, "Required argument missing\n");
		fprintf(stderr, usage, argv[0]);
		return 1;
	}

	// read forest (but not octrees)
	Forest *fst = new Forest;
	FILE_Dataport *fp = new FILE_Dataport();
	if (!fp->ropen(fstfile)) {
		fprintf(stderr, "%s: Can't open forest input %s\n", 
			argv[0], fstfile);
		return 1;
	}
	char token[4096];
	get_next_token(fp, token);
	if (!fst->parse_in(fp, FALSE)) {
		fprintf(stderr, "%s: error reading forest %s\n",
			argv[0], fstfile);
		return 1;
	}
	fp->close();
	if (verbose)
		fprintf(stderr, "Loaded forest %s, %d models\n",
			fstfile, fst->get_num_children());

	// Be graceful if given an empty input forest
	if (fst->get_num_children() < 1) {
		fprintf(stderr, "%s: input forest %s is empty!\n",
			argv[0], fstfile);
		return 1;
	}

	// Determine whether this site frame uses +Z up or +Z down,
	// based on pointing roll of first model
	Patch *p = fst->get_patch(0);
	zdown = p->xrot < -90 || p->xrot > 90;
	if (verbose)
		fprintf(stderr, "World +Z is %s\n", zdown ? "down" : "up");

	// scan quadtree file for leaf nodes (tiles), build each tile
	FILE *qfp = fopen(quadfile, "r");
	if (qfp == NULL) {
		fprintf(stderr, "%s: Can't open quadtree input %s\n",
			argv[0], quadfile);
		return 1;
	}
	while (fgets(token, sizeof(token), qfp)) {
		// tile lines are in format:
		// 	File { name "tileXXX.iv" } # xmin xmax ymin ymax
		char tilename[256];
                char patchname[1024];
                char name_mtl[1024];
		Range bv;
		if (sscanf(token, "File { name \"%[^\"]\" } # %lf %lf %lf %lf",
				tilename, &bv.xmin, &bv.xmax,
				&bv.ymin, &bv.ymax) != 5)
			continue;		// not a tile line
		bv.zmin = -FLT_MAX;
		bv.zmax = FLT_MAX;

                strcpy(patchname, p->get_name());
                strcpy(name_mtl, p->get_name());
                //make sure file permissions are set right
                umask(002);
                FILE *mtl_file = fopen(strcat(name_mtl, ".mtl"), "w");
                if (mtl_file == NULL) {
                    fprintf(stderr, "Can't create mtl file %s\n", name_mtl);
                    exit(1);
                }
                fprintf(mtl_file, "newmtl material_1\n");
                fprintf(mtl_file, "       Ka 0.2 0.2 0.2 1\n");
                fprintf(mtl_file, "       Kd 0.8 0.8 0.8 1\n");
                fprintf(mtl_file, "       Ks 0 0 0 1\n");
                fprintf(mtl_file, "       map_Kd %s.png\n", patchname);
                fclose(mtl_file);

		make_tile(fst, tilename, patchname, &bv);
	}

	return 0;
}
