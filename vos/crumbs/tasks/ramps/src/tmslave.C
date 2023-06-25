// tmslave.C 1.13 03/04/24 12:18:04
/** \file
// SUMMITT Terrain Merge slave (server) task
//
// This program does the actual work of
//	- converting an XYZ range map (normally in site frame) 
//		to an octree in camera frame
//	- matching the octree to an existing site forest (producing revised
//		camera position and pointing data)
//	- generating a multiresolution VST mesh
//
// It runs as a PVM slave task spawned by the terrain merge master,
// accepting processing requests through PVM messages,
// processing the requested data, and sending a reply when done.
// The advantage of this setup is that forest data can be kept in
// memory rather than being re-read from disk for each patch.
// File I/O uses a shared local disk; the master transfers data
// to and from external file systems if necessary.
//
// #define STANDALONE for standalone (non-PVM) version,
// writes transform data to stdout. Files still come and go to
// fixed names in specified directory.
*/

static const char usage[] = 
"usage:\n"
"%s [-v] [-f <forest-path>] [-rng min max] [-rf maxd maxgap]\n"
"    [-nooct] [-d discont] [-a max_angle] [-cf]\n"
"    [-SMP|FDO|MER] [-m min_points] [-t texturename] [-skip] [-compact]\n"
"    [-b bvol_scale] [-it max_iterations] [-e trans_eps rot_eps]\n"
"    [-tol tolerance] [-n distance] [-str xstr ystr zstr] [-sw width]\n"
#ifdef STANDALONE
"    -name base_request_name [-cmod cam_model_name] [-svf site_vector_file]\n"
"Standalone process to do one terrain merge.\n"
#else
"PVM slave process handling terrain merge requests from tmerge master.\n"
#endif
"General options:\n"
"-v = verbose output\n"
"-f = pathname of forest file\n"
"-rng = min/max range values for validating XYZ input (default 0-FLT_MAX)\n"
"     if min<0, limits (|min|,max) are normalized for 45deg FOV, and\n"
"     scaled according to the FOV of the current input.\n"
"-rf  = range filtering/interpolation with max delta range and max gap\n"
"Octree/mesh generation options:\n"
"-nooct = skip octree output\n"
"-d = min normal angle in degrees discarded as discontinuity (default 60)\n"
"-a = max degrees between face normal and camera direction (default 85)\n"
"-cf = input range maps are already in camera frame\n"
"ViSTa generation options:\n"
"-SMP = create SMP (rover frame) VST\n"
"-FDO = create FIDO (rover frame) VST\n"
"-MER = create MER (camera frame) VST\n"
"-m = # of range map points for coarsest LOD (default 4000)\n"
"-t = texture image filename (default 'left.jpg')\n"
"     can contain '%%s', which is replaced by current request name\n"
"-skip = skip alternate LODs\n"
"-compact = remove duplicate vertices (smaller output, takes longer)\n"
"ICP matcher options:\n"
"-b   = bounding volume scale factor for matching overlaps (default 1.0)\n"
"-it  = max iterations (default 100)\n"
"-e   = epsilon (max abs change) criteria for translation, rotation (def 0)\n"
"-tol = tolerance (max relative change) criterion (default 0)\n"
"-n   = neighborhood error bound (src mdl space) to reject edges (default 0)\n"
"-str = stretch destination in model space for better matching\n"
"-sw  = site width (m) for matcher destination volume (default=10)\n"
"\nTo skip octree writing, specify -nooct\n"
"To skip VST generation, don't specify -SMP/FDO/MER\n"
"To skip ICP matching, don't specify -f\n";

#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#ifndef STANDALONE
#include "net/net_lib.h"
#include "tm_msg.h"
#endif
#include "image/image.h"
#include "image/types/all.h"
#include "tmshare.h"
#include "summitt_func.h"
#include "vista.h"

#define MAX_PATH	1024
#define MAX_LOD		10

// forest pathname from command line
static char *fst_name;

// skip output of fullres octree file?
static int no_octree;

// input range maps already in camera frame?
static int cam_frame;

// Minimum cosine of angle between normal vectors. Smaller cosine
// (larger angle) indicates a discontinuity.
static double min_cos_norm = 0.5;	// default = 60 degrees

// Minimum cosine of angle between face normal and eye vector. Smaller
// cosine (larger angle) indicates invalid face triangle.
static double face_threshold = 0.08716;	// default = 85 degrees

// Valid range limits on XYZ data (camera to data point distance)
// If min_range is negative, limits are scaled based on FOV
static double min_range = 0.0;
static double max_range = FLT_MAX;
static double crmin, crmax;	// for current input

// Max delta range to include in range map filtering, 0=no filtering
static float rf_maxd;

// Max pixel distance for interpolating range map gaps, 0=no interpolation
static int rf_gap;

// For VST LODs, keep reducing XYZ resolution until octree has
// at most this many voxels
static int min_points = 4000;

// VST texture image name
static char *tex_name = (char *)"left.jpg";

// VST implementation generated (if not NULL)
static char *vst_mode;

// skip generation of alternate VST LODs?
static int vst_skip = 0;

// produce compact VST files?
static int vst_compact = 0;

// Scale factor for new model's bounding volume to produce region
// of overlapping forest models loaded for matching
double bvscale = 1.0;

// ICP max iterations
int max_iterations = 100;

// ICP epsilon completion criteria for translation, rotation
double tepsilon, repsilon;

// ICP tolerance completion criteria
double tolerance;

// ICP neighborhood error radius
double err_radius;

// ICP stretch factors in baseline model space.
// Default of (0,0,0) means no stretching. Typical use is
// 1,1,10 for Z-vertical, or 10,1,1 for X-vertical model space.
double stretch[3];

// ICP site width in meters, for destination octree coverage
float  site_width = 10.0;

// Working output patch (for standalone app)
static Patch *p;

// terrain request inputs and outputs
static char *req_name;		// in: base path (in work area)
//static CAHVOR wcmod;		// in: camera model (world frame)
#define wcmod (p->cmod)
static double site[3];		// in: (initial) site vector (world->site)
static double posn[3], rot[3];	// out: corrected camera position/rotation
				// (camera to world frame)
static CAHVOR ccmod;		// camera model in camera frame
static char *req_tail;		// req_name without path prefix

// horizontal field of view in radians
static double fovr;

// array of surface models (meshes) at increasing resolution for VST file
static SfcModel *model[MAX_LOD+1];

// objnode for new (fullres) model
static ObjNode newnode;

// new model bounding volume
static Summitt_range bvol;

// new model voxel resolution
static double resolution;

// preprocessed site vector filename
static char *svf_file;

// transform XYZ data to/from camera frame
static ZMatrix obj2world, world2obj;

// initialize forest at startup
void load_forest(char *name, float site_width);

// run ICP match on new node against forest
int match_model(ObjNode *node, Summitt_range *mbvol);

// setup terrain quad surface model
SfcModel *terrain_quad(ImageData *rmap, SfcModel *base,
	ZMatrix cxform, Boolean xcam);

// Track dynamic memory usage. Also shows general progress of patch.
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
	fprintf(stderr, "== %s: Cur mem=%8ldK max=%8ldK @ %s\n",
		req_tail, mem, maxmem, checkpoint);
#endif
}

// Create full-resolution octree from range data, save to disk.
// Return pointer to octree, or NULL if failed
static SfcModel *build_octree(GeoImage *xyz, GeoImage *rgb)
{
	// Setup object->world transform from camera pointing,
	// as initial guess for matching.
	// The inverse transform is used to map range XYZ data into
	// camera coordinates (camera at 0,0,0 looking down +X, +Z=up).
	// (so for MER, this "world" frame is really "site" frame)
	newnode.x = posn[0];
	newnode.y = posn[1];
	newnode.z = posn[2];
	newnode.xrot = rot[0];
	newnode.yrot = rot[1];
	newnode.zrot = rot[2];
	newnode.GetObjToWorldTransform(obj2world);
	if (!cam_frame) 
		MatInvert(obj2world, world2obj);

	// No grid, eyepoint, or Z stretch; do mesh
	int r2o_flags = R2O_MESH | (cam_frame ? 0 : R2O_XCAM);
	SfcModel *oct = range2octree(xyz->get_data(), rgb->get_data(), 
		ccmod.c, fovr, crmin, crmax, world2obj,
		r2o_flags, min_cos_norm, face_threshold, 1.0);

	if (oct == NULL || !oct->get_data()) {
		fprintf(stderr, "Unable to create full-res octree.\n");
		delete oct;
		return NULL;
	}

	newnode.set_object(oct);
	if (verbose) {
		fprintf(stderr, "Full-res mesh for %s: %d triangles\n",
			req_tail, oct->mesh.tri_count());
	}

	// done if not saving the octree file
	if (no_octree)
		return oct;

	// Write out octree (don't need to save mesh in the file).
        FILE_Dataport fp;
        char name[MAX_PATH];
	sprintf(name, "%s/tm.oct", req_name);
	if (!fp.wopen(name)) {
		fprintf(stderr, "Can't create octree file %s\n", name);
		oct->mesh.clean_up();
		delete oct;
		return NULL;
	}
	oct->Octree::parse_out(&fp);
	fp.close();
	return oct;
}

#ifdef STANDALONE

// Finish setup that tmerge master would have done
// (tmproc.C/setup_request())
static void setup_request(Image *xyz)
{	
	// access XYZ as PDS file
	PDSFile *xyz_file = (PDSFile *)xyz->get_file();
	if (xyz_file == NULL) {
		fprintf(stderr, "XYZ isn't PDS, can't get site number\n");
	} else {

		// get site vector (if SVF provided)
		if (svf_file) {
			p->site = get_pds_site(xyz_file);
			get_site_vector(svf_file, p->site, p->site_vector);
		}
	}
	
	// extract camera model if not given on command line
	if (vector_magnitude_sqr(wcmod.a) == 0.0) {
		if (xyz_file == NULL) {
			fprintf(stderr, "XYZ file isn't PDS and no "
					"camera model file given!\n");
			exit(1);
		}

		// Get (original) CAHVOR camera model from XYZ header
		get_pds_cmod(xyz_file, &wcmod);

		// convert camera model to site frame
		cmod_rov_to_site(xyz_file, &wcmod);
	}

        // get range map resolution
        int xres, yres;
        xyz->get_res(&xres, &yres);
        fprintf(stderr, "xres: %d yres: %d\n", xres, yres);

	// derive field of view
	p->fov = p->cmod.hfov(xres, yres);
	p->set_name(req_tail);
}

// Do wrapup that tmerge master would have done,
// creating single-octree forest.
// (tmproc.C/finish_request())
static void finish_request(GeoImage *rgb)
{
	if (no_octree)		// don't want octree/forest?
		return;

	// setup patch world transform data from result posn/rot
	p->x = posn[0];
	p->y = posn[1];
	p->z = posn[2];
	p->xrot = rot[0];
	p->yrot = rot[1];
	p->zrot = rot[2];
	
	p->bvol = bvol;		// bounding volume in world space
	
	p->obj_res = resolution;	// patch resolution

	// transform patch's camera model to camera frame
	// (extract original pointing, also saved in patch)
	CAHVOR oldcmod = p->cmod;
	summitt_cmod_to_pointing(&oldcmod, p->orot, &p->cmod);
	memcpy(p->otrans, oldcmod.c, sizeof(p->otrans));
	
	// setup reference to octree
	p->set_reference("tm.oct");

	// record RGB dimensions
	rgb->get_res(&p->xres, &p->yres, &p->bands);

	// create single-patch forest
	Forest forest;
	forest.add_child(p, -1);
	char name[MAX_PATH];
	sprintf(name, "%s/tm.fst", req_name);
	FILE_Dataport fp;
	if (!fp.wopen(name))
		fprintf(stderr, "Can't create forest %s\n", name);
	else if (!forest.parse_out(&fp))
		fprintf(stderr, "Error writing forest %s\n", name);
	fp.close();
}

#endif

// construct pathname for VISTA texture for current request
static char *vst_texture(GeoImage *rgb)
{
	static char tname[MAX_PATH];
	char *basename = req_tail;	// default is request name
	
	// if RGB file is PDS format, use its PRODUCT_ID string
	if (rgb->get_file_type() == PDS_FILE_ID) {
		PDSFile *pds_file = (PDSFile *)rgb->get_file();
		char *product_id = pds_file->get_value("PRODUCT_ID");
		if (product_id)
			basename = product_id;
	}

	// format texture name
	sprintf(tname, tex_name, basename);
	return tname;
}

// Build multi-resolution ViSTa terrain model wedge.
// xyz = full-res range map; txres/tyres = texture image resolution
// Return 0 if okay
static int build_vst_wedge(GeoImage *xyz, GeoImage *rgb, int txres, int tyres)
{
	// get range map resolution
	int xres, yres;
	xyz->get_res(&xres, &yres);

	// detach range data from xyz GeoImage object
	ImageData *prev_xyz = xyz->get_data();
	xyz->set_data(NULL);
	
	// Create lower-resolution meshes from downsampled range maps
	int r2o_flags = R2O_MESH | (cam_frame ? 0 : R2O_XCAM);
	int i;
	for (i=MAX_LOD-1; i>=0 && xres > 8 && yres > 8 &&
					xres*yres > min_points; i--) {
		// downsample range map - once or twice (vst_skip = 0 or 1)
		for (int j=0; j<=vst_skip; j++) {
			if (rf_gap)
				summitt_range_interpolate(prev_xyz, rf_gap);
			xres /= 2;
			yres /= 2;
			ImageData *new_xyz = new floatData;
			if (!new_xyz->allocate(xres, yres, 3))
				break;
			summitt_range_resample(prev_xyz, new_xyz);
			delete prev_xyz;
			prev_xyz = new_xyz;
		}

		// create surface model with mesh. No RGB needed
		model[i] = range2octree(prev_xyz, NULL, ccmod.c, fovr,
			crmin, crmax, world2obj,
			r2o_flags, min_cos_norm, face_threshold, 1.0);

		// if failed or no triangles, no more LODs
		if (model[i] == NULL || !model[i]->get_data() ||
					model[i]->mesh.tri_list == NULL)
			break;
		if (verbose) {
			fprintf(stderr, "Next mesh: %d triangles\n", 
				model[i]->mesh.tri_count());
		}
	}

	// create single quadrilateral for lowest LOD
	// (maybe should do this from full-res range map?)
	if (i >= 0) {
		model[i] = terrain_quad(prev_xyz, model[MAX_LOD],
						world2obj, !cam_frame);
		if (model[i])
			i--;
		else
			fprintf(stderr, "Unable to create terrain quad.\n");
	}

	delete prev_xyz;

/*
	// Create ViSTa model
	i++;	// lowest valid LOD
        char name[MAX_PATH];
	sprintf(name, "%s/tm.vst", req_name);

	// For MER ViSTa, camera model should be in Rover frame.
	// Tmerge master didn't save that, but it should still
	// be present in the rgb file PDS header
	// (code duplicated from tmproc.C)
	CAHVOR *vst_cmod = &wcmod;
	if (!strcmp(vst_mode, "MER") && rgb->get_file_type() == PDS_FILE_ID) {
		static CAHVOR rcmod;
		get_pds_cmod((PDSFile *)rgb->get_file(), &rcmod);
		vst_cmod = &rcmod;
	}

	int result = summitt_sfc2vst(vst_mode, 
			MAX_LOD-i+1, model+i, name, vst_texture(rgb), 
			NULL, txres, tyres, fovr, obj2world, &ccmod, 
			vst_cmod, vst_compact);

	// toss out all but full-res model
	for (; i<MAX_LOD; i++) {
		model[i]->mesh.clean_up();
		delete model[i];
	}

	return result;
*/
        return 0;
}

// Process one terrain request, return status bitmap:
// bit 0 = octree created okay
// bit 1 = VST wedge created okay
// bit 2 = forest match successful
static int process_request()
{
	int xres, yres;

	if (verbose)
	fprintf(stderr, "Slave processing request %s\n", req_name);

	// get name without path prefix (patch identifier)
	req_tail = strrchr(req_name, '/');
	if (req_tail)
		req_tail++;
	else
		req_tail = req_name;
	track_memory("start request");

	// Open XYZ range map file. May be RAN, PDS, or VICAR format
	char name[MAX_PATH];
	sprintf(name, "%s/tm.xyz", req_name);
	GeoImage xyz(name);	 // try auto format (but won't grok RAN)
	if (xyz.get_res(&xres, &yres) < 1) {
		xyz.read(name, RAN_FILE_ID);	// try explicit RAN format
		if (xyz.get_res(&xres, &yres) < 1) {
			fprintf(stderr, "Could not open XYZ file %s\n", name);
			return 0;
		}
	}
	
#ifdef STANDALONE
	// finish setup that tmerge master would have done
	setup_request(&xyz);
#endif

	// optionally filter some noise out of XYZ image
	if (rf_maxd) {
		ImageData *fmap = new floatData;
		if(!fmap->allocate(xres, yres, 3))
			fprintf(stderr, "Can't allocate filtered XYZ map\n");
		else {
			summitt_range_filter(xyz.get_data(), fmap, rf_maxd);
			xyz.free_data();
			xyz.set_data(fmap);
		}
	}

	// optionally interpolate across gaps in XYZ image
	if (rf_gap)
		summitt_range_interpolate(xyz.get_data(), rf_gap);

	// Open RGB texture image file
	sprintf(name, "%s/tm.img", req_name);
	GeoImage rgb(name);
	int txres, tyres;
	if (rgb.get_res(&txres, &tyres) < 1) {
		fprintf(stderr, "Could not open image file %s\n", name);
		return 0;
	}

	// convert camera model to (initial) camera position and rotation
	// in world frame, and transform camera model to camera frame
	vector_copy(wcmod.c, posn);
	summitt_cmod_to_pointing(&wcmod, rot, &ccmod);
	if (verbose)
		fprintf(stderr, "initial camera pos: %f %f %f rot: %f %f %f\n",
			posn[0], posn[1], posn[2], rot[0], rot[1], rot[2]);

	track_memory("input data loaded");

	// derive (approximate) field of view from camera model
	fovr = ccmod.hfov(xres, yres) * TORADIANS;

	// determine range limits for this input
	if (min_range < 0.0) {		// FOV-dependent
		crmin = -min_range * (45.0*TORADIANS) / fovr;
		crmax = max_range * (45.0*TORADIANS) / fovr;
	} else {			// fixed limits
		crmin = min_range;
		crmax = max_range;
	}
	
	// build full-res octree and mesh
	SfcModel *fulloct = build_octree(&xyz, &rgb);
	if (fulloct == NULL) {
		track_memory("end request");
		return 0;		// octree build failed
	}

	int status = 1;			// at least got octree built
	model[MAX_LOD] = fulloct;	// save as max LOD model
	track_memory("octree built");

	// get bounding volume in model space
	summitt_octree_extents(fulloct, &bvol);

	// get voxel resolution in model space, convert to object space
	resolution = 4.0 / (1 << fulloct->get_max_levels());
	resolution *= 
		(fulloct->xscale + fulloct->yscale + fulloct->zscale) / 3.0;

	// build VST wedge model unless disabled
	if (vst_mode && !build_vst_wedge(&xyz, &rgb, txres, tyres))
		status |= 2;		// VST built okay
	track_memory("VST built");

	// Update transform to true world frame, not site frame;
	// world+site_vector = site, so world = site - site_vector
	newnode.x = posn[0] -= site[0];
	newnode.y = posn[1] -= site[1];
	newnode.z = posn[2] -= site[2];

	// match new octree to forest to get new world transform result
	if (fst_name && !match_model(&newnode, &bvol)) {	
		// successful match
		posn[0] = newnode.x.get_value();
		posn[1] = newnode.y.get_value();
		posn[2] = newnode.z.get_value();
		rot[0] = newnode.xrot.get_value();
		rot[1] = newnode.yrot.get_value();
		rot[2] = newnode.zrot.get_value();
		status |= 4;		
	}
	track_memory("match finished");

	// convert bounding volume from model to (new) world frame
	world_volume(&newnode, &bvol);

#ifdef STANDALONE
	// followup processing that tmerge master would have done
	finish_request(&rgb);
#endif

	fulloct->mesh.clean_up();
	delete fulloct;			// done with new octree data
	track_memory("end request");
	return status;
}

// Extract and validate command line arguments
static void get_args(int argc, char **argv)
{
	for (int i=1; i<argc; i++) {
		// general options
		if (!strcmp(argv[i], "-f") && i+1 < argc) {
			fst_name = argv[++i];
		} else if (!strcmp(argv[i], "-v")) {
			verbose++;

		// input range map processing
		} else if (!strcmp(argv[i], "-rng") && i+2 < argc) {
			min_range = atof(argv[++i]);
			max_range = atof(argv[++i]);
		} else if (!strcmp(argv[i], "-rf") && i+2 < argc) {
			rf_maxd = atof(argv[++i]);
			rf_gap = atoi(argv[++i]);

#ifdef STANDALONE
		} else if (!strcmp(argv[i], "-name") && i+1 < argc) {
			req_name = argv[++i];
		} else if (!strcmp(argv[i], "-cmod") && i+1 < argc) {
			if (strcmp(argv[++i], "-"))
				wcmod.read(argv[i]);
		} else if (!strcmp(argv[i], "-svf") && i+1 < argc) {
			svf_file = argv[++i];
#endif
		// octree generation
		} else if (!strcmp(argv[i], "-nooct")) {
			no_octree = TRUE;
		} else if (!strcmp(argv[i], "-d") && i+1 < argc) {
			min_cos_norm = cos(TORADIANS * atof(argv[++i]));
		} else if (!strcmp(argv[i], "-a") && i+1 < argc) {
			face_threshold = cos(TORADIANS * atof(argv[++i]));
		} else if (!strcmp(argv[i], "-cf")) {
			cam_frame = TRUE;

		// VST generation
		} else if (!strcmp(argv[i], "-SMP") ||
			   !strcmp(argv[i], "-FDO") ||
			   !strcmp(argv[i], "-MER")) {
			vst_mode = argv[i]+1;
		} else if (!strcmp(argv[i], "-m") && i+1 < argc) {
			min_points = atoi(argv[++i]);
		} else if (!strcmp(argv[i], "-t") && i+1 < argc) {
			tex_name = argv[++i];
		} else if (!strcmp(argv[i], "-skip")) {
			vst_skip = 1;
		} else if (!strcmp(argv[i], "-compact")) {
			vst_compact = 1;

		// ICP matcher
		} else if (!strcmp(argv[i], "-b") && i+1 < argc) {
			bvscale = atof(argv[++i]);
		} else if(!strcmp(argv[i], "-it") && i+1 < argc) {
			max_iterations = atoi(argv[++i]);
		} else if(!strcmp(argv[i], "-e") && i+2 < argc) {
			tepsilon = atof(argv[++i]);
			repsilon = atof(argv[++i]);
		} else if(!strcmp(argv[i], "-tol") && i+1 < argc) {
			tolerance = atof(argv[++i]);
		} else if(!strcmp(argv[i], "-n") && i+1 < argc) {
			err_radius = atof(argv[++i]);
		} else if(!strcmp(argv[i], "-str") && i+3 < argc) {
			stretch[0] = atof(argv[++i]);
			stretch[1] = atof(argv[++i]);
			stretch[2] = atof(argv[++i]);
		} else if(!strcmp(argv[i], "-sw") && i+1 < argc) {
			site_width = atof(argv[++i]);

		// secret bit flags for experimental code - see summitt_func.C
		} else if(!strcmp(argv[i], "-hack")) {
			extern int icp_flags;
			icp_flags = atoi(argv[++i]);
		} else {
			fprintf(stderr, "%s: Unknown argument %s\n", 
				argv[0], argv[i]);
			fprintf(stderr, usage, argv[0]);
			exit(1);
		}
	}
}

// SUMMITT Terrain merge slave main program
int main(int argc, char **argv)
{
	p = new Patch;	// needs to be dynamic to go into forest

	get_args(argc, argv);

	// get base forest voxels and setup base matching octree
	if (fst_name)
		load_forest(fst_name, site_width);	

#ifdef STANDALONE
	if (req_name == NULL) {
		fprintf(stderr, "Need to specify base name\n");
		return 1;
	}
	
	process_request();
	printf("-t %f %f %f -r %f %f %f\n", 
		posn[0], posn[1], posn[2], rot[0], rot[1], rot[2]);
	return 0;
#else

	// tell master PVM task that we're ready for business
	Master_Task master;
	psend_slave_ready(&master);

	// loop processing requests
	for (;;) {
		if (master.recv_buffer() < 0 || precv_slave_request(&master, 
					&req_name, &wcmod, site) < 0) {
			fprintf(stderr, "Can't get xyz request from master\n");
			sleep(5); // probably user/setup goof, don't thrash
			continue;
		}
		int status = process_request();
		psend_terrain_done(&master, req_name, status, 
						posn, rot, &bvol, resolution);
		free(req_name);
	}
#endif
}
