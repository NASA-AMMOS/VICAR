// asdmesh.C 1.10 03/05/01 08:00:18
/** \file
* ASD surface mesh builder
* This program generates a series of triangular meshes from an
* octree representation of a 3D surface, at increasing
* levels of detail, in a manner suitable for an OpenGL Performer
* Active Surface Definition visualization database.
*
* For ASD output without textures, one attribute is written for each vertex. 
* For ASD textured output, attribute records are written as needed,
* at most one per vertex per texture.
*
* The basic algorithm starts with a 2-triangle mesh covering the full surface,
* then creates successive levels of detail:
*	for each edge in the mesh
*		look for point in octree nearest the edge's midpoint
*		if octree point isn't already in mesh
*			split the edge at that point into 2 edges
*	for each triangle in mesh
*		if edges were split
*			replace with multiple triangles at new LOD
*
* A previous version tried to be memory-efficient by outputting LODs
* as they were created - or actually, after the *next* NEXT LOD was
* created, to do the forward linking. Got too messy and fragile.
*/

// #define NODIAG to disable diagnostic-only code

#include <float.h>
#include "image/types/all.h"
#include "summitt_func.h"
#include "patch.h"
#include "asdmesh.h"

static const char usage[] = 
"Usage: %s [-v] [-f] [-e edge_limit] [-m morph_limit]\n"
"  [-d done_factor] [-p prop_limit] [-lod n] [-adj]\n"
"  [-n norm_threshold] [-c color_dist] [-z scale]\n"
"  [-i input_model] [-acc levels] [-s|a output_name]\n"
"  [tex_file[:mdl] [tex_file[:mdl] ...]]\n\n"
"Creates ASD-suitable mesh from input octree on stdin.\n"
"-i = input model (octree or forest), default = standard input\n"
"-acc = accumulate bottom levels of (merged) octree before meshing\n"
"-s = write final surface model to <output_name>\n"
"-a = write multilevel ASD mesh to <output_name>\n\n"
"-f = flip triangles to face -Z (implies model space is Y right/Z down)\n"
"-e = max ratio of edge length to voxel size (default=5.0)\n"
"-m = max ratio of morph distance to edge length (default=0.2)\n"
"-d = 1/fraction of max split edges needed to keep iterating (default=500)\n"
"-p = min ratio of triangle edge to longest edge as splittable (default=0.6)\n"
"-lod = max output levels of detail (default=20)\n"
"-adj = output face adjaceny data (for tri stripping) in ASD output\n"
"-n = max view normal angle in degrees to accept texture (default=90)\n"
"-c = max delta for RGB component between texture and model color;\n"
"      0=ignore model color (default),\n"
"     >0=use base texture color if delta > color_dist\n"
"-z = stretch (scale>1) or squish (scale<1) model space\n"
"-v = verbose output.\n"
"tex_files = texture images, in same order as forest models;\n"
":mdl = filename of corresponding camera model, or range map\n"
"       horizontal field-of-view in degrees, or 0 for RH height map\n"
"       or -1 for LH height map. If ':mdl' isn't given, camera model\n"
"       is taken from forest file, or from image header if the\n"
"       input is a single model.\n";

enum	{ NUM_LOD = 32 };	// hard limit on detail levels

// PDS header fields for camera model components
#define CMOD_C_KEY	"MODEL_COMPONENT_1"
#define CMOD_A_KEY	"MODEL_COMPONENT_2"
#define CMOD_H_KEY	"MODEL_COMPONENT_3"
#define CMOD_V_KEY	"MODEL_COMPONENT_4"
#define CMOD_O_KEY	"MODEL_COMPONENT_5"
#define CMOD_R_KEY	"MODEL_COMPONENT_6"

// maximum allowed ratio of mesh edge length to sum of the voxel sizes
// of its endpoints (squared)
double edge_limit = (5.0 * 5.0);

// maximum allowed ratio of morph distance to edge length (squared)
double morph_limit = (0.2 * 0.2);

// minimum ratio of given triangle edge to max triangle edge (squared)
// to allow splitting (to reduce chances of thin triangles)
double short_edge_limit = (0.6 * 0.6);

// exit refinement loop when new level produces less than
// max_split/done_factor split edges, where max_split is the max number
// of split edges for any previous level.
int done_factor = 500;

double cos_threshold;	// cosine view normal angle threshold
int color_dist;		// color distance (model vs. texture color)

// make this a command-line option? (grow levels on merge)
static Boolean level_grow = TRUE;

static char **texname;		// texture filenames

extern int verbose;

// Input octree forest.
// Used for model input, and to keep transforms for texture mapping.
static Forest fst;

// Input octree model and output surface model.
// sfc->mesh.pt_list = list of all voxel nodes;
//	nodespec id = 0 for nodes not assimilated into mesh (yet).
//	nodespec id = pointer to allocated Vertex object when added to mesh
static SfcModel *sfc;

static Edge *elist[NUM_LOD];		// edge list for each LOD
static Triangle *tlist[NUM_LOD];	// triangle list for each LOD

static enum { ASD, SFC } format;	// output format
static Model *model;			// component models (textures)
static int max_split;			// max edge split per level
static int flip;			// flip output ordering?
static int adjacency;			// output face adjacency lines?
static int endgame;			// mesh resolution near model res?
static int asd_vertex_id;		// next ASD file vertex ID
static int asd_face_id;			// next ASD file face ID
static int asd_attr_id;			// next ASD file textured attr ID
static ZMatrix mdlwld0;			// base model to world transform
static double zscale = 1.0;		// model space height scaling

// diagnostic-only counters
static int reject1;	// edges discarded for being too long vs. voxel edge
static int reject2;	// split discarded for new point closer to other edges
static int reject3;	// midpoint search found edge point
static int reject4;	// midpoint search found other point already used
static int reject5;	// texture-voxel color distance too high
static int fcid0, fcid1;	// find_closest edge endpoint IDs

#ifndef NODIAG		// function for dbx aid only
void show_edge(Edge *e)
{
	double V1[3], V2[3];
	e->p1->get_global_center(V1);
	e->p2->get_global_center(V2);
	fprintf(stderr, "(%f %f %f) - (%f %f %f)\n", 
		V1[0], V1[1], V1[2], V2[0], V2[1], V2[2]);
}
#endif

// Build ideal camera model in camera frame 
// (unflipped: +X=fwd, +Y=left, +Z=up; flipped: +X=fwd, +Y=right, +Z=down)
// given horizontal field of view in degrees. 
// Perspective range image has FOV>0, result has C = (0,0,0).
// FOV=0 indicates RH height map, recorded as C = (1,0,0)
// FOV<0 means LH height map, recorded as C = (-1,0,0)
void cmod_gen(CAHVOR *cmod, double fov, int xres, int yres)
{
	static double forward[]	= { 1.0, 0.0, 0.0 };
	static double right[]	= { 0.0, -1.0, 0.0 };
	static double down[] 	= { 0.0, 0.0, -1.0 };
	static double fright[]	= { 0.0, 1.0, 0.0 };
	static double fdown[] 	= { 0.0, 0.0, 1.0 };

	memset(cmod, 0, sizeof(*cmod));
	vector_copy(forward, cmod->a);

	// note special cases, won't actually use cahvor model
	if (fov < 0.0) {
		cmod->c[0] = -1.0;
		return;
	} else if (fov == 0.0) {
		cmod->c[0] = 1.0;
		return;
	}

	// convert field of view to radians
	double xfov = fov * PI/180.0;
	// assume square pixels
	double yfov = xfov * yres / xres;

	/* Compute the internal model parameters */
	cmod->hs = xres / (2.0 * tan(0.5*xfov));
	cmod->vs = yres / (2.0 * tan(0.5*yfov));
	cmod->hc = 0.5 * (xres - 1);
	cmod->vc = 0.5 * (yres - 1);
	cmod->theta = -0.5 * PI;

	/* Create the rest of the linear model */
	vector_scale(flip ? fright : right, cmod->hs, cmod->h);
	double u[3];
	vector_scale(forward, cmod->hc, u);
	vector_sum(u, cmod->h, cmod->h);

	vector_scale(flip ? fdown : down, cmod->vs, cmod->v);
	vector_scale(forward, cmod->vc, u);
	vector_sum(u, cmod->v, cmod->v);

	vector_copy(cmod->a, cmod->o);
}

// Extract camera model component from PDS file header string,
// list is in the form "(val, val, val)".
// Return 0 if okay
static int extract_cmod(PDSFile *f, char *key, double a[3])
{
	char *val = f->get_value(key);
	if (val == NULL)
		return -1;
	sscanf(val, "(%lf,%lf,%lf", &a[0], &a[1], &a[2]);
	return 0;
}

// extract camera model from PDS file header, return 0 if okay
// (promote CAHV model to CAHVOR if O & R are missing)
static int extract_cahvor(CAHVOR *cmod, PDSFile *img)
{
	if (extract_cmod(img, CMOD_C_KEY, cmod->c) ||
	    extract_cmod(img, CMOD_A_KEY, cmod->a) ||
	    extract_cmod(img, CMOD_H_KEY, cmod->h) ||
	    extract_cmod(img, CMOD_V_KEY, cmod->v))
	    	return -1;
	if (extract_cmod(img, CMOD_O_KEY, cmod->o))
		vector_copy(cmod->a, cmod->o);
	if (extract_cmod(img, CMOD_R_KEY, cmod->r))
		memset(cmod->r, 0, sizeof(cmod->r));
	return 0;
}

// load model i's texture map and 
// setup camera model for texture coordinate mapping
static void get_tex(int i)
{
	Model *m = &model[i];
	Patch *p = fst.get_patch(i);
	CAHVOR *cmod = &(p->cmod);
	CAHVOR tcmod;			// temp model, world frame
	double rot[3];

	// explicit camera model or field of view?
	char *c = strrchr(texname[i], ':');
	if (c) 				// yes
		*c++ = 0;		// separate :xxx from name

	// load texture image, get dimensions
	m->tex.read(texname[i]);
	if (m->tex.get_res(&m->xres, &m->yres) <= 0) {
		fprintf(stderr, "Can't load texture %s\n", texname[i]);
		exit(1);
	}

	// setup camera model for texture mapping
	if (c) {
		if (isdigit(*c) || *c == '-') {	
			// build model from field of view
			cmod_gen(cmod, atof(c), m->xres, m->yres);
		} else {		// load named camera model file
			if (tcmod.read(c)) {
				fprintf(stderr, "Can't read camera model %s"
					" for model %d (texture %s)\n",
					c, i, texname[i]);
				exit(1);
			}
			// transform model to camera frame
			summitt_cmod_to_pointing(&tcmod, rot, cmod);
		}
	}

	if (cmod->a[0] == 0.0 && cmod->a[1] == 0.0 && cmod->a[2] == 0.0) {
		// camera model not defined yet, try PDS header
		if (m->tex.get_file_type() != PDS_FILE_ID ||
				extract_cahvor(&tcmod, 
					(PDSFile *)m->tex.get_file())) {
			fprintf(stderr, "No camera model found for "
				"model %d (texture %s)\n",
				i, texname[i]);
			exit(1);
		}
		// transform model to camera frame
		summitt_cmod_to_pointing(&tcmod, rot, cmod);
	}
	
	if (verbose)
		fprintf(stderr, "Model %d: xres=%d yres=%d\n",
			i, m->xres, m->yres);

	// m->xform maps a vertex from sfc 0 model coords
	// back to this surface's object coords:
	// world-to-object[i] . model-to-world[0]
	ZMatrix objwldi, invxform;
	p->GetObjToWorldTransform(objwldi);
	MatInvert(objwldi, m->xform);
	MatMult(m->xform, mdlwld0);

	// use inverse transform, objX to model0, to
	// compute surface's eye point (camera model C vector)
	// in base model coords, for texture map selection
	MatInvert(m->xform, invxform);
	MultPoints(p->cmod.c, invxform, m->eye);
}

// Replace base (first) model of forest with a version scaled in
// model Z coordinates (zscale > 1 stretches height, 
// zscale < 1 squishes height).
// Returns new base node surface model.
static SfcModel *scale_forest(Forest *forest, double zscale)
{    
    // find max octree depth for forest
    ObjNode *base_node = forest->get_child(0);
    Octree *base_oct = (Octree *)(base_node->get_object());
    int levels = base_oct->get_max_levels();

    for (int i=1; i<forest->get_num_children(); i++) {
	Octree *oct = (Octree *)(forest->get_child(i)->get_object());
	if (oct->get_max_levels() > levels)
		levels = oct->get_max_levels();
    }

    // create new scaled octree for merging
    SfcModel *mso = new SfcModel(levels);
    mso->init_data();
    // replace forest base ObjNode octree with new scaled octree
    // ** would be nice to have copy/assignment for Obj's and ObjNode's -
    // ** this isn't really safe...
    ObjNode *old_base = new ObjNode;
    *old_base = *base_node;	// save copy for merge
    base_node->set_object(mso);
    
    // model->object transform is same as base model except Z-scaled
    mso->x.set_value(base_oct->x.get_value()); 
    mso->y.set_value(base_oct->y.get_value());
    mso->z.set_value(base_oct->z.get_value());
    mso->xrot.set_value(base_oct->xrot.get_value());
    mso->yrot.set_value(base_oct->yrot.get_value());
    mso->zrot.set_value(base_oct->zrot.get_value());
    mso->xscale.set_value(base_oct->xscale.get_value());
    mso->yscale.set_value(base_oct->yscale.get_value());
    mso->zscale.set_value(base_oct->zscale.get_value() / zscale);

    // merge (move) original base octree voxels to new octree
    summitt_merge_octrees(old_base, base_node, FALSE, level_grow);

    return mso;
}

// Load input model (forest, surface model, or octree).
// If forest, merge data into single octree at 'sfc'
static void load_model(char *infile)
{
	int i;

	FILE_Dataport fp;
	if (infile) {
		if (!fp.ropen(infile)) {
			fprintf(stderr, "Can't open %s for reading\n",
				infile);
			exit(1);
		}
	} else {
		fp.open(stdin);
		infile = (char *)"stdin";
	}
	if (verbose)
		fprintf(stderr, "Loading model data from %s\n", infile);

	char token[4096];
	get_next_token(&fp, token);
	if (!strcmp(token, "GRP_V1")) {		// input is a forest
		if (!fst.parse_in(&fp))
			exit(1);
		sfc = (SfcModel *)fst.get_child(0)->get_object();
		// discard any input mesh data
		sfc->mesh.clean_up();

	} else {				// input is single octree
		if (!strcmp(token, "SFC_MODEL_V1"))
			get_next_token(&fp, token);
		if (!strcmp(token, "OCTREE_V1")) {
			sfc = new SfcModel;
			// (use Octree::parse to ignore any mesh)
			if (!sfc->Octree::parse_in(&fp))
				exit(1);
		} else {
			fprintf(stderr, "Unknown input format %s\n",
				infile);
			exit(1);
		}

		// setup as 1-tree forest, empty camera model
		Patch *p = new Patch;
		memset(&p->cmod, 0, sizeof(p->cmod));
		p->set_object(sfc);
		fst.add_child(p);
	}

	// get base model-to-world transform (for ASD)
	fst.get_child(0)->GetTransformationMatrix(mdlwld0);

	// texture file setup
	// Need to do this *before* merging octrees to get proper ranges
	if (format != ASD)
		ntexture = 0;
	if (ntexture > 0) {
		model = new Model[ntexture];

		if (fst.get_num_children() != ntexture) {
			fprintf(stderr, "Wrong number of texture files given\n"
				"(got %d, but input %s has %d models)\n",
				ntexture, infile, fst.get_num_children());
			exit(1);
		}

		// okay, setup model list and texture mapping
		for (i=0; i<ntexture; i++)
			get_tex(i);
	}

	// optionally setup Z-scaled base model space for merging
	if (zscale != 1.0) {
		sfc = scale_forest(&fst, zscale);
		// get revised transform
		fst.get_child(0)->GetTransformationMatrix(mdlwld0);
	}
		
	// Merge all voxels to a single octree.
	// First octree is assumed to be "base" low-res model.
	for (i=1; i<fst.get_num_children(); i++) {
		if (verbose)
			fprintf(stderr, "merging model %d "
				"to base octree\n", i);
		SfcModel *s = (SfcModel *)fst.get_child(i)->get_object();
		s->mesh.clean_up();
		summitt_merge_octrees(fst.get_child(i),
					fst.get_child(0), FALSE, level_grow);
	}
}

// get squared distance between two nodes
double ndist2(NodeSpec *n1, NodeSpec *n2)
{
	double V1[3], V2[3];
	n1->get_global_center(V1);
	n2->get_global_center(V2);
	return distance_sqr(V1, V2);
}

// get squared distance from point P to edge e=AB
// d = len((P-A)x(B-A)) / len(B-A)
double edist2(NodeSpec *p, Edge *e)
{
	double P[3], A[3], B[3];
	p->get_global_center(P);
	e->p1->get_global_center(A);
	e->p2->get_global_center(B);
	vector_diff(P, A, P);		// P <- P-A
	vector_diff(B, A, B);		// B <- B-A
	cross_product(P, B, A);		// A <- (P-A)x(B-A)
	return vector_magnitude_sqr(A) / vector_magnitude_sqr(B);
}

// Find closest point in octree (to split an edge).
// Return NULL if result is a point already in the mesh.
// Otherwise, allocate a corresponding Vertex object and point
// at it with nodespec id field.
static NodeSpec *find_closest(double V[])
{
	NodeSpec *ns;
	sfc->get_data()->fast_find_closest(sfc->get_max_levels(), 
			FLT_MAX, V, V, &ns);
	if (ns->id) {		// this node already in use?
#ifndef NODIAG
		if (ns->id == fcid0 || ns->id == fcid1)
			reject3++;	// found edge vertex
		else
			reject4++;	// some other point
#endif
		return NULL;
	}

	// Allocate a Vertex for additional info.
	ns->id = (int)(new Vertex);
	return ns;
}

// Alternate version
static NodeSpec *find_closest(double x, double y, double z)
{
	double V[3] = {x, y, z};
	return find_closest(V);
}

// Compute location to search for model point to split edge
static void get_search_point(Edge *e, double *V)
{
	double V1[3], V2[3];
	e->p1->get_global_center(V1);
	e->p2->get_global_center(V2);
	// midpoint of edge
	// ** this tends to produce a flat model, skipping over
	// ** vertical features!
	V[0] = 0.5*(V1[0] + V2[0]);
	V[1] = 0.5*(V1[1] + V2[1]);
	V[2] = 0.5*(V1[2] + V2[2]);
	
	if (!endgame)
		return;

	// near final resolution, adjust based on surface normals
	double N1[3], N2[3], N[3];
	e->p1->get_normal(N1);
	e->p2->get_normal(N2);
	
	// if normals are 90 degrees apart or more, we really can't make
	// a reasonable guess about what the surface is doing...
	if (dot_product(N1, N2) <= 0.0)
		return;

	// averaged normal
	N[0] = 0.5*(N1[0] + N2[0]);
	N[1] = 0.5*(N1[1] + N2[1]);
	N[2] = 0.5*(N1[2] + N2[2]);
	normalized_vector(N, N);
	
	double tlen1, tlen2;
	double cdot = dot_product(N1, N);
	if (cdot)
		// length along averaged normal from midpoint to
		// intersection with point 1 tangent plane
		tlen1 = (dot_product(N1, V1) - dot_product(N1, V)) / cdot;
	else
		tlen1 = 0.0;
	cdot = dot_product(N2, N);
	if (cdot)
		tlen2 = (dot_product(N2, V2) - dot_product(N2, V)) / cdot;
	else
		tlen2 = 0.0;
	tlen1 = 0.5 * (tlen1 + tlen2);
		
	// project from midpoint along averaged normal
	V[0] += N[0] * tlen1;
	V[1] += N[1] * tlen1;
	V[2] += N[2] * tlen1;
}

#if 0
// Check if distance from edge to morph point is reasonable
// compared to edge length
static int morph_ratio_ok(Edge *e, NodeSpec *mp, double *mid)
{
	// for now at least, just use distance to midpoint
	double morph[3];
	mp->get_global_center(morph);
	return distance_sqr(mid, morph) < morph_limit * e->len;
}
#endif

// Check length of unsplittable edge - is it too long
// compared to edge_lengths of connected voxels?
// (probably a bogus edge spanning a gap in data).
static int long_edge(Edge *e)
{
	double vlen = e->p1->edge_length + e->p2->edge_length;
	if (e->len < edge_limit*vlen*vlen)
		return FALSE;

	reject1++;
	return TRUE;
}

// Check split point, should be closer to original edge than it is
// to other edges of triangle. Return 1 if split undone, else 0.
// Intended to avoid skinny and back-facing triangles.
static int check_split(Triangle *t, Edge *e, Edge *oe1, Edge *oe2)
{
	// no problem if edge wasn't split
	if (!e->is_split())
		return 0;

	double ed = edist2(e->p2, e);
	if (ed <= edist2(e->p2, oe1) && ed <= edist2(e->p2, oe2)) 
		return 0;	// okay

	// Too far, cancel the split.
	// Mark split edges as invalid, so they can be removed
	// from active list.
	// Also free vertex at split point (allocated in find_closest())
	reject2++;
	delete e->d1->v2(); 
	e->d1->set_invalid();
	e->d2->set_invalid();
	e->d1 = NULL;		// edge is no longer divided
	return 1;
}

// First phase for next higher level of detail - split edges
// from previous level, return number of new edges.
// This and the next function hold the key logic for the mesh builder.
int split_edges(Edge *pelist)
{
	int tsplit = 0;
	reject1 = reject2 = reject3 = reject4 = 0;

	// to avoid thin triangles, mark short edges to skip 
	// splitting this cycle (set d2=NULL to not split)
	Edge *e;
	for (e=pelist; e; e=e->next) 	// first mark all okay
		e->d2 = e;

	Triangle *t;			// now scan prev LOD triangles
	for (t=tri_list; t; t=t->next) {
		float maxlen = t->e1->len;	// find longest edge
		if (t->e2->len > maxlen)
			maxlen = t->e2->len;
		if (t->e3->len > maxlen)
			maxlen = t->e3->len;
		maxlen *= short_edge_limit;	// convert to threshold
		if (t->e1->len < maxlen)
			t->e1->d2 = NULL;	// don't divide
		if (t->e2->len < maxlen)
			t->e2->d2 = NULL;	// don't divide
		if (t->e3->len < maxlen)
			t->e3->d2 = NULL;	// don't divide
	}

	// split edges near midpoint
	// note new edges are added to active edge_list
	for (e=pelist; e; e=e->next) {
		if (e->d2 == NULL) { 		// marked to not split
			new Edge(e);		// just copy for new LOD
			continue;
		}

		double P[3];
		get_search_point(e, P);		// edge midpoint
#ifndef NODIAG
		fcid0 = e->p1->id;
		fcid1 = e->p2->id;
#endif
		NodeSpec *mp = find_closest(P);
		if (mp) {	// found a point not already in mesh
			// ** && morph_ratio_ok(e, mp, P)
			tsplit++;
			e->split(mp);
		} else {		// don't split this edge
			// if edge is too long compared to voxels,
			// leave it out of remaining LODs, otherwise
			// copy to new LOD
			if (!long_edge(e))
				new Edge(e);
		}
	}

	// Undo edge splits that might cause bad divided triangles.
	// (tri_list = prev LOD triangles, pointing to prev LOD edges)
	for (t=tri_list; t; t=t->next) {
		tsplit -= check_split(t, t->e1, t->e2, t->e3);
		tsplit -= check_split(t, t->e2, t->e3, t->e1);
		tsplit -= check_split(t, t->e3, t->e1, t->e2);
	}

	// discard rejected split edges on active list
	Edge *eprev = NULL;
	Edge *enext;
	for (e=edge_list; e; e=enext) {
		enext = e->next;
		if (e->is_valid()) {
			eprev = e;
		} else {
			if (eprev)
				eprev->next = enext;
			else
				edge_list = enext;
			delete e;
		}
	}
	
	if (verbose)
		fprintf(stderr, "Split %d edges, rej: %d %d %d %d\n",
			tsplit, reject1, reject2, reject3, reject4);

	return tsplit;
}

// Finish next higher level of detail by building triangles from split edges.
// This is the rest of the key logic for the mesh builder.
void build_level(Triangle *ptlist)
{
	// Build current LOD triangles (on tri_list) from previous LOD 
	// triangles (on ptlist) and split edges (on edge_list).
	// New edges are created as needed to connect to split points.
	Triangle *tnext;
	for (Triangle *t=ptlist; t; t=tnext) {
		tnext = t->next;

		// how many of my edges were split?
		int nsplit = 0;
		if (t->e1->is_split())
			nsplit++;
		if (t->e2->is_split())
			nsplit++;
		if (t->e3->is_split())
			nsplit++;

		Edge *en1, *en2, *en3;		// new edges
		double d1, d2;

		switch (nsplit) {
		case 0:	// not split, end of the line for this guy
			if (format != ASD) {	// move to current list
				t->next = tri_list;
				tri_list = t;
			}
			break;

		case 1:	// one edge split, replace tri with 2
			// rotate triangle edges so e1 is the split one
			if (t->e2->is_split())
				t->rotrvs();
			else if (t->e3->is_split())
				t->rotfwd();
			en1 = new Edge(t->e1->d1->p2, t->p3());

			t->k1 = new Triangle(t->rvs1 ? t->e1->d2 : t->e1->d1,
					t->rvs1, en1, 0, t->e3, t->rvs3);
			t->k2 = new Triangle(t->rvs1 ? t->e1->d1 : t->e1->d2,
					t->rvs1, t->e2, t->rvs2, en1, 1);
			break;

		case 2:	// two edges split, replace tri with 3
			// rotate triangle edges so e1 is the UNsplit one
			if (!t->e2->is_split())
				t->rotrvs();
			else if (!t->e3->is_split())
				t->rotfwd();

			en1 = new Edge(t->e2->d1->p2, t->e3->d1->p2);
			t->k1 = new Triangle(t->rvs2 ? t->e2->d1 : t->e2->d2, 
					t->rvs2, t->rvs3 ? t->e3->d2 : t->e3->d1, 
					t->rvs3, en1, 1);

			// shorter one splits remaining quad more evenly???
			d1 = ndist2(t->e2->p2, t->p1());
			d2 = ndist2(t->e3->p2, t->p2());
			if (d1 < d2) {
				en2 = new Edge(t->e2->d1->p2, t->p1());

				t->k2 = new Triangle(en2, 1, en1, 0, 
					t->rvs3 ? t->e3->d1 : t->e3->d2, t->rvs3);
				t->k3 = new Triangle(t->e1, t->rvs1,
					t->rvs2 ? t->e2->d2 : t->e2->d1, t->rvs2,
					en2, 0);
			} else {
				en2 = new Edge(t->e3->d1->p2, t->p2());

				t->k2 = new Triangle(en1, 0, en2, 0,
					t->rvs2 ? t->e2->d2 : t->e2->d1, t->rvs2);
				t->k3 = new Triangle(t->e1, t->rvs1, en2, 1,
					t->rvs3 ? t->e3->d1 : t->e3->d2, t->rvs3);
			}
			break;
		case 3: // all edges split, replace tri with 4
			en1 = new Edge(t->e2->d1->p2, t->e3->d1->p2);
			en2 = new Edge(t->e3->d1->p2, t->e1->d1->p2);
			en3 = new Edge(t->e1->d1->p2, t->e2->d1->p2);

			t->k1 = new Triangle(en3, 1, t->rvs1 ? t->e1->d1 : 
				t->e1->d2, t->rvs1,
				t->rvs2 ? t->e2->d2 : t->e2->d1, t->rvs2);
			t->k2 = new Triangle(en1, 0, en2, 0, en3, 0);
			t->k3 = new Triangle(en1, 1, t->rvs2 ? t->e2->d1 : 
				t->e2->d2, t->rvs2,
				t->rvs3 ? t->e3->d2 : t->e3->d1, t->rvs3);
			t->k4 = new Triangle(t->rvs1 ? t->e1->d2 : t->e1->d1, 
				t->rvs1, en2, 1, 
				t->rvs3 ? t->e3->d1 : t->e3->d2, t->rvs3);
		}
	}
}

// Build initial mesh (look for extreme points).
// Assume surface is mostly normal to one axis, but figure out
// which axis that is.
static void initial_mesh(Summitt_range *bvol)
{
	// take normal axis as the one with smallest bounding extent
	double dx, dy, dz;
	NodeSpec *p1, *p2, *p3, *p4;
	bvol->size(&dx, &dy, &dz);
	if (dz < dx && dz < dy) {
		// data is aligned with XY plane
		dz = 0.5 * (bvol->zmin + bvol->zmax);
		p1 = find_closest(2.0*bvol->xmin, 2.0*bvol->ymin, dz);
		p2 = find_closest(2.0*bvol->xmax, 2.0*bvol->ymin, dz);
		p3 = find_closest(2.0*bvol->xmax, 2.0*bvol->ymax, dz);
		p4 = find_closest(2.0*bvol->xmin, 2.0*bvol->ymax, dz);
	} else if (dx < dy && dx < dz) {
		// data is aligned with YZ plane
		dx = 0.5 * (bvol->xmin + bvol->xmax);
		p1 = find_closest(dx, 2.0*bvol->ymin, 2.0*bvol->zmin);
		p2 = find_closest(dx, 2.0*bvol->ymax, 2.0*bvol->zmin);
		p3 = find_closest(dx, 2.0*bvol->ymax, 2.0*bvol->zmax);
		p4 = find_closest(dx, 2.0*bvol->ymin, 2.0*bvol->zmax);
	} else {
		// data is aligned with XZ plane
		dy = 0.5 * (bvol->ymin + bvol->ymax);
		p1 = find_closest(2.0*bvol->xmin, dy, 2.0*bvol->zmin);
		p2 = find_closest(2.0*bvol->xmax, dy, 2.0*bvol->zmin);
		p3 = find_closest(2.0*bvol->xmax, dy, 2.0*bvol->zmax);
		p4 = find_closest(2.0*bvol->xmin, dy, 2.0*bvol->zmax);
	}

	// check that all searches found unique nodes
	if (!p1 || !p2 || !p3 || !p4) {
		fprintf(stderr, "Corner vertices not unique!\n");
		exit(1);
	}

	// these points don't morph to lower LOD
	nsv(p1)->r1 = nsv(p2)->r1 = nsv(p3)->r1 = nsv(p4)->r1 = NULL;
	
	// build triangles from these points
	Edge *e1 = new Edge(p1, p4);
	Edge *e2 = new Edge(p4, p3);
	Edge *e3 = new Edge(p3, p2);
	Edge *e4 = new Edge(p2, p1);
	Edge *e5 = new Edge(p1, p3);

	(new Triangle(e1, 0, e2, 0, e5, 1))->fid = 1;
	(new Triangle(e3, 0, e4, 0, e5, 0))->fid = 0;
	asd_face_id = 2;
}

// write surface model (octree+mesh)
static void write_model(char *name)
{
	FILE_Dataport fp;
	if (!fp.wopen(name)) {
		fprintf(stderr, "Can't create %s\n", name);
		exit(1);
	}
	if (verbose)
		fprintf(stderr, "Creating %s\n", name);

	// convert mesh data to sfcmodel structure
	for (Triangle *t=tri_list; t; t=t->next) {
		if (!t->is_valid())
			continue;
		if (flip)
			sfc->mesh.add_triangle(new Mesh_Triangle(t->p1(), 
					t->p2(), t->p3()));
		else
			sfc->mesh.add_triangle(new Mesh_Triangle(t->p1(),
					t->p3(), t->p2()));
	}

	sfc->parse_out(&fp);
}

// (Re-)Write ASD mesh file header.
// Header size is fixed so the allocation sizes can be revised.
static void asd_header(FILE *fp)
{
	int i;
	
	fprintf(fp, "#ASDMesh V1.0 ascii\n");
	if (ntexture) {
		fprintf(fp, "p normal texture\n");
		fprintf(fp, "n %8d %8d %8d %8d\n", 
			asd_vertex_id, asd_face_id, asd_attr_id, ntexture);
		for (i=0; i<ntexture; i++)
			fprintf(fp, "t %s\n", texname[i]);
	} else {	// non-textured
		fprintf(fp, "p color normal\n");
		fprintf(fp, "n %8d %8d %8d 1\n", 
			asd_vertex_id, asd_face_id, asd_vertex_id);
	}
	
	// model-to-world coordinate transform
	fputc('x', fp);
	for (i=0; i<4; i++)
		fprintf(fp, " %f %f %f %f", mdlwld0[i][0],
			mdlwld0[i][1], mdlwld0[i][2], mdlwld0[i][3]);
	fputc('\n', fp);

	// triangle strip adjacency flag
	fprintf(fp, "s %d\n", adjacency);
}

// extract stuff about a voxel node (coordinates, normal, color)
static void voxel_info(NodeSpec *ns, double V[], double N[], double rgb[])
{
	ns->get_global_center(V);
	ns->get_normal(N);
	ns->get_color(&rgb[0], &rgb[1], &rgb[2]);
}

// Compute texture coordinates for vertex in each model space
static void vertex_tcoord(Vertex *v, double vc[3])
{
	Model *m = model;

	for (int i=0; i<ntexture; i++, m++) {
		CAHVOR *cmod = &fst.get_patch(i)->cmod;
		double vo[3];	// vertex in model i's object coords
		MultPoints(vc, m->xform, vo);

		if (cmod->c[0] == 0.0) {		// range map
			// use camera model to project 3D point to image
			double st[2];
			cmod->To_2D(vo, st);
			// convert double -> float, pixel->fraction;
			// flip Y so that (0,0) is lower left
			v->u[i] = st[0] / m->xres;
			v->v[i] = (m->yres - 1 - st[1]) / m->yres;

		} else if (cmod->c[0] > 0.0) {		// RH image map
			// object coords = pixel index+0.5
			// map (0.5,0.5) to (0,0)
			// and (xres+0.5, yres+0.5) to (1,1)
			v->u[i] = (vo[0] - 0.5) / m->xres;
			v->v[i] = (vo[1] - 0.5) / m->yres;
		} else {				// LH image map
			// object coords = pixel index+0.5
			// map (0.5,yres-0.5) to (0,0)
			// and (xres+0.5, -0.5) to (1,1)
			v->u[i] = (vo[0] - 0.5) / m->xres;
			v->v[i] = (m->yres - 0.5 - vo[1]) / m->yres;
		}
	}
}

// Write one mesh vertex and attribute for ASD mesh format.
// Possible cases:
//	morphing vertex, RGB:
//		v vertex-xyz dvertex-xyz
//		a normal-xyz dnormal-xyz color-rgb dcolor-rgb
//	morphing vertex, explicit texture coordinates:
//		v vertex-xyz dvertex-xyz
//		(attributes written elsewhere)
//	morphing vertex, texgen texture coordinates: (not supported)
//		v vertex-xyz dvertex-xyz
//		a normal-xyz dnormal-xyz
//	non-morphing vertex, RGB:
//		v vertex-xyz
//		a normal-xyz color-rgb
//	non-morphing vertex, explicit texture coordinates:
//		v vertex-xyz
//		(attributes written elsewhere)
//	non-morphing vertex, texgen texture coordinates: (not supported)
//		v vertex-xyz
//		a normal-xyz
// Returns file index.
static int asd_vertex(FILE *fp, NodeSpec *ns)
{
	Vertex *v = nsv(ns);
	if (v->fid < 0) {	// not written yet?

		// assign next file vertex ID
		v->fid = asd_vertex_id++;

		// mark all attribute file ID's as 'not yet'
		for (int i=0; i<ntexture; i++)
			v->afid[i] = -1;

		// get voxel's final coordinates, normal, and color
		double V[3], N[3], C[3];
		voxel_info(ns, V, N, C);
			
		// compute texture coordinates
		vertex_tcoord(v, V);

		// write vertex, and possibly attribute, lines
		if (v->r1) {	// morphing vertex
			double V1[3], V2[3], N1[3], N2[3], C1[3], C2[3];
			voxel_info(v->r1, V1, N1, C1);
			voxel_info(v->r2, V2, N2, C2);
				
			// find interpolation factor for reference
			// point P = point on V1-V2 nearest V
			// using: V1V.V1V2 = |V1V|*|V1V2|*cos alpha
			//        cos alpha = |V1P| / |V1V|
			//        f = |V1P| / |V1V2|
			// where: f=0 at V1, f=1 at V2, f=f at P
			double V1V2[3], V1V[3];
			vector_diff(V2, V1, V1V2);
			vector_diff(V, V1, V1V);
			double f = dot_product(V1V, V1V2) /
					vector_magnitude_sqr(V1V2);
					
			// interpolate vertex at reference point
			int i;
			for (i=0; i<3; i++)
				V1[i] += f * V1V2[i];

			// write final and morph delta vertex (ref-final)
			fprintf(fp, "v %f %f %f %g %g %g\n",
				V[0], V[1], V[2],
				V1[0]-V[0], V1[1]-V[1], V1[2]-V[2]);

			// likewise for attributes
			if (!ntexture) {
				// normal and color
				for (i=0; i<3; i++) {
					N1[i] += f * (N2[i] - N1[i]);
					C1[i] += f * (C2[i] - C1[i]);
				}
				fprintf(fp, "a %f %f %f %g %g %g "
					"%.3f %.3f %.3f %g %g %g\n",
					N[0], N[1], N[2],
					N1[0]-N[0], N1[1]-N[1], N1[2]-N[2],
					C[0], C[1], C[2],
					C1[0]-C[0], C1[1]-C[1], C1[2]-C[2]);
			}

		} else {		// non-morphing vertex
			fprintf(fp, "v %f %f %f\n", V[0], V[1], V[2]);
			if (!ntexture) {
				fprintf(fp, "a %f %f %f %.3f %.3f %.3f\n",
					N[0], N[1], N[2], C[0], C[1], C[2]);
			}
		}
	}
	return v->fid;
}

// Convert triangle pointer to face ID, handle NULL => -1
static int face_id(Triangle *t)
{
	return t ? t->fid : -1;
}

// is this texture coordinate inside the texture?
static int inside(double uv[])
{
	return uv[0]>=0.0 && uv[0]<=1.0 && uv[1]>=0.0 && uv[1]<=1.0;
}

// 2D distance
static double dist2d(double a[2], double b[2])
{
	double dx = a[0] - b[0];
	double dy = a[1] - b[1];
	return sqrt(dx*dx + dy*dy);
}

// 3D integer squared distance (color distance)
static int idsq(int r1, int g1, int b1, int r2, int g2, int b2)
{
	r1 -= r2;
	g1 -= g2;
	b1 -= b2;
	return r1*r1 + g1*g1 + b1*b1;
}

// compute squared area of face in 2D texture space (using Heron's forumula)
static double texture_area(double uv1[2], double uv2[2], double uv3[2], int i)
{
	double a = dist2d(uv2, uv3);
	double b = dist2d(uv1, uv3);
	double c = dist2d(uv1, uv2);
	double s = 0.5 * (a + b + c);
	return (s * (s-a) * (s-b) * (s-c)) * model[i].xres * model[i].yres;
}

// Choose the best model to texture this triangle face.
// Texture must fully cover the face.
// If multiple models have full coverage, choose the one with the
// largest area in texture space (the one with the most detail).
// Returns 0 (base model) if nothing better is found.
static int choose_tex(Triangle *t)
{
	int best_model = 0;
	
	// best model coverage, face area in texture coords squared
	double best_area = 0.0;

	// get face vertices, normal, colors
	Vertex *v[3];
	int r[3], g[3], b[3];
	double V1[3], V2[3], V3[3];

	NodeSpec *ns = t->p1();
	v[0] = nsv(ns);
	ns->get_global_center(V1);
	ns->get_color(&r[0], &g[0], &b[0]);

	ns = t->p2();
	v[1] = nsv(ns);
	ns->get_global_center(V2);
	ns->get_color(&r[1], &g[1], &b[1]);

	ns = t->p3();
	v[2] = nsv(ns);
	ns->get_global_center(V3);
	ns->get_color(&r[2], &g[2], &b[2]);

	double normal[3];
	surface_normal(V1, V2, V3, normal);

	double a;
	int i;
	for (i=1; i<ntexture; i++) {
		// get face vertex texture coords for this model
		double uv1[2], uv2[2], uv3[2];
		uv1[0] = v[0]->u[i];
		uv1[1] = v[0]->v[i];
		uv2[0] = v[1]->u[i];
		uv2[1] = v[1]->v[i];
		uv3[0] = v[2]->u[i];
		uv3[1] = v[2]->v[i];

		// does this model's texture fully cover the face?
		if (inside(uv1) && inside(uv2) && inside(uv3)) {
			// yes, check angle between face and view direction
			double view[3];
			vector_diff(V1, model[i].eye, view);
			normalize_vector(view);
			// dot product = cosine
			if (dot_product(view, normal) < cos_threshold)
				continue;	// don't use this one

			// measure texture area
			a = texture_area(uv1, uv2, uv3, i);

			// best detail so far?
			if (a > best_area) {
				best_model = i;
				best_area = a;
			}
		}			
	}				

	// no valid non-base textures?
	if (best_area == 0.0)
		return 0;		// use base texture
	
	// Optionally check distance between texture color and model color.
	// This semi-kludge handles problems such as triangles
	// that aren't really visible from any model.
	if (color_dist) {
		int dsq = color_dist * color_dist;
		Model *m = &model[best_model];
		for (i=0; i<3; i++) {	// each face vertex
			// note texture v[0]=0.0 at bottom,
			// but map index 0 is at top
			double tx = v[i]->u[best_model]*m->xres;
			double ty = (m->yres-1) - v[i]->v[best_model]*m->yres;
			uchar tr, tg, tb;	// texture map color
			m->tex.get_data()->iget_color(tx, ty, &tr, &tg, &tb);
			if (idsq(tr, tg, tb, r[i], g[i], b[i]) > dsq) {
				// color too far off, use base texture
				reject5++;	// diagnostic count
				return 0;
			}
		}
	}

	return best_model;	
}

// Get file ID for ASD format textured vertex attribute,
// writing the attribute line if not yet output for specified texture number.
static int tex_attr(FILE *fp, NodeSpec *ns, int tnum)
{
	Vertex *v = nsv(ns);
	if (v->afid[tnum] >= 0)		// already generated?
		return v->afid[tnum];
		
	// get normal and texture coords for this vertex
	double V[3], N[3], C[3], T[2];
	voxel_info(ns, V, N, C);
	T[0] = v->u[tnum];
	T[1] = v->v[tnum];
	
	if (v->r1) {	// morphing vertex
		
		// get normal and texture coords at reference point
		double V1[3], V2[3], N1[3], N2[3], T1[2], T2[2];
		ns = v->r1;
		voxel_info(ns, V1, N1, C);
		T1[0] = nsv(ns)->u[tnum];
		T1[1] = nsv(ns)->v[tnum];
		
		ns = v->r2;
		voxel_info(ns, V2, N2, C);
		T2[0] = nsv(ns)->u[tnum];
		T2[1] = nsv(ns)->v[tnum];

		// interpolation factor (see asd_vertices())
		double V1V2[3], V1V[3];
		vector_diff(V2, V1, V1V2);
		vector_diff(V, V1, V1V);
		double f = dot_product(V1V, V1V2) / vector_magnitude_sqr(V1V2);

		for (int i=0; i<3; i++)
			N1[i] += f * (N2[i] - N1[i]);
		T1[0] += f * (T2[0] - T1[0]);
		T1[1] += f * (T2[1] - T1[1]);

		fprintf(fp, "a %f %f %f %g %g %g %f %f %g %g\n",
			N[0], N[1], N[2], N1[0]-N[0], N1[1]-N[1], N1[2]-N[2],
			T[0], T[1], T1[0]-T[0], T1[1]-T[1]);

	} else {
		fprintf(fp, "a %f %f %f %f %f\n",
			N[0], N[1], N[2], T[0], T[1]);
	}

	// record file ID
	return v->afid[tnum] = asd_attr_id++;
}

// Get ID for adjacent face (-1 if none)
static int adj_face(Triangle *t, Edge *e)
{
	return face_id(e->t1 == t ? e->t2 : e->t1);
}

// Write ASD data for one level of detail for ASD mesh format
static void write_lod(FILE *fp, int lod)
{
	Triangle *t;

	reject5 = 0;

	fprintf(fp, "l %d\n", lod);
	
	// set next LOD's triangle face IDs
	for (t=tlist[lod+1]; t; t=t->next)
		t->fid = asd_face_id++;

	// If want adjacency info, link edges to the triangles
	// that use them. It would be harder to do this on the fly, 
	// what with splitting, unsplitting, and pipeline issues
	if (adjacency) {
		for (t=tlist[lod]; t; t=t->next)
			t->e1->t1 = t->e1->t2 = t->e2->t1 = t->e2->t2 = 
				t->e3->t1 = t->e3->t2 = NULL;

		for (t=tlist[lod]; t; t=t->next) {
			t->e1->set_adj(t);
			t->e2->set_adj(t);
			t->e3->set_adj(t);
		}
	}

	// output this LOD's triangles
	for (t=tlist[lod]; t; t=t->next) {
		// face vertices
		NodeSpec *p1 = t->p1();
		NodeSpec *p2 = t->p2();
		NodeSpec *p3 = t->p3();
		int v1 = asd_vertex(fp, p1);
		int v2 = asd_vertex(fp, p2);
		int v3 = asd_vertex(fp, p3);

		// reference vertices
		int r1 = t->e1->is_split() ? asd_vertex(fp, t->e1->d1->p2) : -1;
		int r2 = t->e2->is_split() ? asd_vertex(fp, t->e2->d1->p2) : -1;
		int r3 = t->e3->is_split() ? asd_vertex(fp, t->e3->d1->p2) : -1;

		int tbest;		// best texture map for this face
		int a1, a2, a3;		// face vertex attribute ID's
		int ra1, ra2, ra3;	// ref vertex attribute ID's
		if (ntexture) {
			tbest = choose_tex(t);
			// write attribute for each vertex
			a1 = tex_attr(fp, p1, tbest);
			a2 = tex_attr(fp, p2, tbest);
			a3 = tex_attr(fp, p3, tbest);
			// and attributes for reference vertices
			ra1 = (t->e1->is_split()) ? 
				tex_attr(fp, t->e1->d1->p2, tbest) : -1;
			ra2 = (t->e2->is_split()) ? 
				tex_attr(fp, t->e2->d1->p2, tbest) : -1;
			ra3 = (t->e3->is_split()) ? 
				tex_attr(fp, t->e3->d1->p2, tbest) : -1;
		}

		if (flip)	// clockwise
			fprintf(fp, "f %d %d %d %d %d %d",
				v1, v2, v3, r1, r2, r3);
		else		// counterclockwise
			fprintf(fp, "f %d %d %d %d %d %d",
				v1, v3, v2, r3, r2, r1);

		// child faces
		fprintf(fp, " %d %d %d %d", 
			face_id(t->k1), face_id(t->k2),
			face_id(t->k3), face_id(t->k4));

		if (ntexture) {
			// texture #, vertex attributes, ref attributes
			if (flip)
				fprintf(fp, " %d %d %d %d %d %d %d",
					tbest, a1, a2, a3, ra1, ra2, ra3);
			else
				fprintf(fp, " %d %d %d %d %d %d %d",
					tbest, a1, a3, a2, ra3, ra2, ra1);
		} // (else 1 attribute per vertex, attr ID = vertex ID)

		fputc('\n', fp);

		// optional adjacency info
		if (adjacency)
			fprintf(fp, "%d %d %d\n", adj_face(t, t->e1),
				adj_face(t, t->e2), adj_face(t, t->e3));
	}

	if (verbose)
		fprintf(stderr, "lod %d rej5: %d\n", lod, reject5);
}

// output result in intermediate ASD text format
static void write_asd(char *name, int nlod)
{
	// start ASD file
	FILE *asd_fp = NULL;
    //make sure output file will have proper permissions
    umask(002);
	if ((asd_fp = fopen(name, "w")) == NULL) {
		fprintf(stderr, "can't create ASD file %s\n", name);
		exit(1);
	}

	// placeholder file header
	asd_header(asd_fp);

	// write each LOD
	for (int i=0; i<nlod; i++)
		write_lod(asd_fp, i);

	// revise header with final allocations
	rewind(asd_fp);
	asd_header(asd_fp);
}

int main(int argc, char **argv)
{
	int i;
	int accum_levels = 0;
	int max_lod = 20;
	char *infile = NULL, *outfile = NULL;

	for (i=1; i<argc; i++) {
		if (!strcmp(argv[i], "-i")) {
			infile = argv[++i];
		} else if (!strcmp(argv[i], "-acc")) {
			accum_levels = atoi(argv[++i]);
		} else if (!strcmp(argv[i], "-s")) {
			outfile = argv[++i];
			format = SFC;
		} else if (!strcmp(argv[i], "-a")) {
			outfile = argv[++i];
			format = ASD;
		} else if (!strcmp(argv[i], "-f")) {
			flip = 1;
		} else if (!strcmp(argv[i], "-d")) {
			done_factor = atoi(argv[++i]);
		} else if (!strcmp(argv[i], "-e")) {
			edge_limit = atof(argv[++i]);
			edge_limit *= edge_limit;
		} else if (!strcmp(argv[i], "-m")) {
			morph_limit = atof(argv[++i]);
			morph_limit *= morph_limit;
		} else if (!strcmp(argv[i], "-p")) {
			short_edge_limit = atof(argv[++i]);
			short_edge_limit *= short_edge_limit;
		} else if (!strcmp(argv[i], "-lod")) {
			max_lod = atoi(argv[++i]);
			if (max_lod >= NUM_LOD) {
				fprintf(stderr, "Limited to %d LOD's\n", 
					NUM_LOD);
				max_lod = NUM_LOD-1;
			}
		} else if (!strcmp(argv[i], "-adj")) {
			adjacency = 1;
		} else if (!strcmp(argv[i], "-v")) {
			verbose = 1;
		} else if (!strcmp(argv[i],"-n")) {
			cos_threshold = cos(TORADIANS * atof(argv[++i]));
		} else if (!strcmp(argv[i],"-c")) {
			color_dist = atoi(argv[++i]);
		} else if (!strcmp(argv[i],"-z")) {
			zscale = atof(argv[++i]);
		} else if (argv[i][0] == '-') {
			fprintf(stderr, "Unknown argument %s\n", argv[i]);
			fprintf(stderr, usage, argv[0]);
			exit(1);
		} else {	// end of options
			break;
		}
	}

	if (outfile == NULL) {
		fprintf(stderr, "Output format and filename not specified\n");
		fprintf(stderr, usage, argv[0]);
		exit(1);
	}

	// remaining args are texture files
	texname = argv + i;
	ntexture = argc - i;

	// read forest/sfcmodel/octree 3D model to 'sfc'
	load_model(infile);

	// optionally accumulate lower octree levels to simplify
	if (accum_levels) {
		int new_levels = sfc->get_max_levels() - accum_levels;
		if (verbose)
			fprintf(stderr, 
				"Accumulating merged tree to %d levels\n",
				new_levels);
		sfc->set_max_levels(new_levels, TRUE);
	}	

	// setup point list, mark all point ID's as "not in mesh",
	// and find bounding volume (in model space)
	if (verbose)
		fprintf(stderr, "Determining bounding volume\n");
	sfc->add_pt_list();
	Summitt_range bvol;
	bvol.init();
	for (NodeSpec_List_Element *p = sfc->mesh.pt_list; p; p=p->next) {
		p->nodespec->id = 0;
		double V[3];
		p->nodespec->get_global_center(V);
		bvol.include(V);
	}
	if (verbose)
		bvol.dump(stderr, "of input mesh");

	// create LOD zero mesh
	initial_mesh(&bvol);
	tlist[0] = tri_list;

	// loop for each mesh refinement level
	for (i=1; i<max_lod; i++) {
		if (verbose)
			fprintf(stderr, "Building level %d\n", i);

		// advance active edge list
		elist[i-1] = edge_list;
		edge_list = NULL;

		// split edges for level i, count number of splits
		int n = split_edges(elist[i-1]);
		if (n > max_split)
			max_split = n;
		if (n == 0 || n * done_factor < max_split)
			break;		// no more significant improvement

		// If got less than half the max split count, 
		// the mesh must be getting close to model resolution -
		// adjust splitting strategy.
		if (!endgame && n < max_split/2) {
			if (verbose)
				fprintf(stderr, 
					"Mesh nearing model resolution\n");
			endgame = 1;
			short_edge_limit = 0.8 * 0.8; 
		}
		
		// create triangles for level i
		tri_list = NULL;
		build_level(tlist[i-1]);
		tlist[i] = tri_list;
	}
	
	if (format == ASD)
		write_asd(outfile, i);
	else
		write_model(outfile);	// final LOD, sfcmodel format
	return 0;
}

