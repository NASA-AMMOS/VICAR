// ran2asd.C 1.3 03/05/01 07:59:17
/** \file
* Range map to ASD surface mesh builder
*
* This program generates a series of triangular meshes from a range
* (XYZ) map of a 3D surface, at increasing levels of detail, in a 
* manner suitable for an OpenGL Performer Active Surface Definition 
* visualization database.
*
* The basic algorithm starts with a 2-triangle mesh covering the full surface,
* then creates successive levels of detail:
*	for each edge in the mesh
*		look for point in range map nearest the edge's midpoint
*		if point isn't already in mesh
*			split the edge at that point into 2 edges
*	for each triangle in mesh
*		if edges were split
*			replace with multiple triangles at new LOD
*
* To keep the logic simpler, all LODs are built in memory
* before starting file output.
*/

#include <float.h>
#include "summitt_func.h"
#include "cahvor.h"

#include "ran2asd.h"

// ASD algorithm parameters (**move declaration to include file)
struct ASD_Parms {
	int flip;			// flip triangle winding?
	int adjacency;			// output triangle strip adjacency?
	int max_lod;			// maximum output levels

	// 1/fraction of max split edges needed to keep iterating
	int done_factor;
	
	// maximum allowed edge length in range map image coords (squared)
	// for edges to cover data gaps
	int edge_limit;

	// maximum distance in pixels to search for valid split point
	int max_search_dist;

	// Minimum cosine of angle between adjacent normal vectors. 
	// Smaller cosine (larger angle) indicates a discontinuity.
	double min_cos_norm;

	// cosine view normal angle threshold for visible faces
	double cos_threshold;

	// minimum ratio of given triangle edge to max triangle edge (squared)
	// to allow splitting (to reduce chances of thin triangles)
	// ** not currently used; default = 0.6*0.6
	double short_edge_limit;
	
	float range_low, range_high;	// valid input range limits (squared)
};

enum { NUM_LOD = 32 };			// hard limit on detail levels

extern int verbose;

static ImageData *imap;			// input range map
static int xres, yres;

static ZMatrix obj2world;		// transform camera to site frame
static ZMatrix world2obj;		// transform site to camera frame

static Edge *elist[NUM_LOD];		// edge list for each LOD
static Triangle *tlist[NUM_LOD];	// triangle list for each LOD

static int max_split;			// max edge split per level
static int asd_vertex_id;		// next ASD file vertex ID
static int asd_face_id;			// next ASD file face ID

static ASD_Parms *parms;		// algorithm parameters

// Setup site/camera frame transforms from camera model
static void extract_transform(CAHVOR *cmod)
{
	// no camera model means data is already in camera frame
	if (cmod == NULL) {
		MatIdent(obj2world);
		MatIdent(world2obj);
		return;
	}

	// convert camera model to camera position and rotation
	double posn[3], rot[3];
	vector_copy(cmod->c, posn);
	summitt_cmod_to_pointing(cmod, rot, NULL);

	// convert to object->world transform
	ObjNode node;
	node.x = posn[0];
	node.y = posn[1];
	node.z = posn[2];
	node.xrot = rot[0];
	node.yrot = rot[1];
	node.zrot = rot[2];
	node.GetObjToWorldTransform(obj2world);

	// The inverse transform is used to map range XYZ data into
	// camera coordinates (camera at 0,0,0 looking down +X, +Z=up).
	MatInvert(obj2world, world2obj);
}

// Transform range map XYZ data to camera frame, and mark invalid
// cells in cmap
static void transform_xyz()
{
	for (int y=0; y<yres; y++) {
		for (int x=0; x<xres; x++) {
			double p[3], cp[3];
			p[0] = imap->get_float(x, y, 0);
			if (p[0] < -90000.0) {
				cmap->set_invalid(x, y);
				continue;
			}
			p[1] = imap->get_float(x, y, 1);
			p[2] = imap->get_float(x, y, 2);
			if (p[0] == 0.0 && p[1] == 0.0 && p[2] == 0.0) {
				cmap->set_invalid(x, y);
				continue;
			}

			// transform to camera frame
			MultPoints(p, world2obj, cp);

			// validate range (and check that point is not
			// behind the camera)
			double range2 = vector_magnitude_sqr(cp);
			if (range2 < parms->range_low ||
				range2 > parms->range_high || cp[0] <= 0.0) {
				cmap->set_invalid(x, y);
				continue;
			}

			// valid point, store back in map
			imap->set_float(cp[0], x, y, 0);
			imap->set_float(cp[1], x, y, 1);
			imap->set_float(cp[2], x, y, 2);
		}
	}
}

// Get one XYZ point from range map, return TRUE if valid
static int get_range(double *p, int x, int y)
{
	if (!cmap->is_valid(x, y))
		return FALSE;
	p[0] = imap->get_float(x, y, 0);
	p[1] = imap->get_float(x, y, 1);
	p[2] = imap->get_float(x, y, 2);
	return TRUE;
}

// Compute normal for one triangle in XYZ image
// (2-1-3 = counterclockwise corner in left-handed image), accumulate
// for average cell normal. Return 1 if okay (all points are valid)
static int add_one_normal(int x1, int y1, int x2, int y2,
				int x3, int y3, int valid, double *norm)
{
	double p1[3], p2[3], p3[3], tnorm[3];

	if (!get_range(p1, x1, y1) || !get_range(p2, x2, y2) || 
					!get_range(p3, x3, y3))
		return 0;
	surface_normal(p1, p3, p2, tnorm);

	if (!valid) {		// first normal for this cell
		memcpy(norm, tnorm, sizeof(tnorm));

	// Accumulate, unless apparently a discontinuity. 
	} else if (fabs(dot_product(tnorm, norm)) >= parms->min_cos_norm) {
		vector_sum(norm, tnorm, norm);
	}
	return 1;
}

// Estimate surface normal for XYZ image at cell x, y by averaging
// normals of the 4 triangles using that cell (or fewer, at the edges
// and adjacent to invalid points).
static void range_normal(int x, int y, double *norm)
{
	int valid = 0;			// found a valid normal yet?

	if (x > 0 && y > 0)  		// upper left triangle
		valid += add_one_normal(x, y, x-1, y, x, y-1, valid, norm);
	if (x+1 < xres && y > 0) 	// upper right triangle
		valid += add_one_normal(x, y, x, y-1, x+1, y, valid, norm);
	if (x > 0 && y+1 < yres) 	// lower left triangle
		valid += add_one_normal(x, y, x, y+1, x-1, y, valid, norm);
	if (x+1 < xres && y+1 < yres) 	// lower right triangle
		valid += add_one_normal(x, y, x+1, y, x, y+1, valid, norm);

	if (!valid) {	// none valid, set default vertical normal
		norm[0] = norm[1] = 0.0;
		norm[2] = 1.0;
	} else {	// normalize to unit vector
		normalize_vector(norm);
	}

	// flip normal direction if -Z is up
	if (parms->flip)
		vector_scale(norm, -1.0, norm);
}

// get squared distance between two vertices (XYZ space)
double vdist2(Vertex *v1, Vertex *v2)
{
	double V1[3], V2[3];
	v1->get_xyz(V1);
	v2->get_xyz(V2);
	return distance_sqr(V1, V2);
}

// get squared distance from point P to edge e=AB
// d = len((P-A)x(B-A)) / len(B-A)
double edist2(Vertex *p, Edge *e)
{
	double P[3], A[3], B[3];
	p->get_xyz(P);
	e->p1->get_xyz(A);
	e->p2->get_xyz(B);
	vector_diff(P, A, P);		// P <- P-A
	vector_diff(B, A, B);		// B <- B-A
	cross_product(P, B, A);		// A <- (P-A)x(B-A)
	return vector_magnitude_sqr(A) / vector_magnitude_sqr(B);
}

// Look for unused range map point near midpoint of existing mesh edge.
// Return allocated Vertex, or NULL if search failed
static Vertex *find_split(Edge *e)
{
	// edge midpoint in image coords
	int mid_x = (e->p1->ix + e->p2->ix) / 2;
	int mid_y = (e->p1->iy + e->p2->iy) / 2;
	
#define TEST_POINT(px,py) \
	if (cmap->in_use(px, py)) return NULL; \
	if (cmap->is_valid(px, py)) return new Vertex(px, py);
	
	// search outward from midpoint for valid range cell
	TEST_POINT(mid_x, mid_y);

	// how far to search
	int maxd = MIN(xres, parms->max_search_dist);
	for (int d=1; d<maxd; d++) {
		int xleft = MAX(mid_x - d, 0);
		int xright = MIN(mid_x + d, xres-1);
		int ytop = MAX(mid_y - d, 0);
		int ybottom = MIN(mid_y + d, yres-1);

		for (int x=xleft; x<=xright; x++) {
			TEST_POINT(x, ytop);
			TEST_POINT(x, ybottom);
		}
		for (int y=ytop+1; y<ybottom; y++) {
			TEST_POINT(xleft, y);
			TEST_POINT(xright, y);
		}
	}
	return NULL;
}

// Check length of unsplittable edge. If too long in range map image
// space, it's probably a bogus edge spanning a gap in data.
static int long_edge(Edge *e)
{
	int dx = e->p1->ix - e->p2->ix;
	int dy = e->p1->iy - e->p2->iy;
	return (dx*dx + dy*dy) > parms->edge_limit;
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
	// Also free vertex at split point (allocated in find_split())
	delete e->d1->p2; 
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
	Edge *e;
	Triangle *t;
	int tsplit = 0;

#if 0
	// to avoid thin triangles, mark short edges to skip 
	// splitting this cycle (set d2=NULL to not split)
	for (e=pelist; e; e=e->next) 	// first mark all okay
		e->d2 = e;

	// now scan prev LOD triangles
	for (t=tri_list; t; t=t->next) {
		float maxlen = t->e1->len;	// find longest edge
		if (t->e2->len > maxlen)
			maxlen = t->e2->len;
		if (t->e3->len > maxlen)
			maxlen = t->e3->len;
		maxlen *= parms->short_edge_limit; // convert to threshold
		if (t->e1->len < maxlen)
			t->e1->d2 = NULL;	// don't divide
		if (t->e2->len < maxlen)
			t->e2->d2 = NULL;	// don't divide
		if (t->e3->len < maxlen)
			t->e3->d2 = NULL;	// don't divide
	}
#endif

	// split edges near midpoint
	// note new edges are added to active edge_list
	for (e=pelist; e; e=e->next) {
#if 0
		if (e->d2 == NULL) { 	// marked to not split
			new Edge(e);	// just copy for new LOD
			continue;
		}
#endif

		Vertex *mp = find_split(e);
		if (mp) {		// found a point not already in mesh
			tsplit++;
			e->split(mp);
		} else {		// don't split this edge
			// if edge is too long (spans a hole),
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
		fprintf(stderr, "Split %d edges\n", tsplit);

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

			// shorter one splits remaining quad more evenly?
			d1 = vdist2(t->e2->p2, t->p1());
			d2 = vdist2(t->e3->p2, t->p2());
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

// look for "corners" of range map, return TRUE if okay
static int corners(int *c0, int *c1, int *c2, int *c3)
{
	int clim = (xres < yres ? xres : yres) / 2;
	int i, j;
	
#define TEST_CELL(ix,iy,a)	\
	if (cmap->is_valid(ix, iy)) { a[0] = ix; a[1] = iy; break; }

	// initialize to "not found"
	c0[0] = c1[0] = c2[0] = c3[0] = -1;
	
	// upper left
	for (i=0; i<clim; i++) {
		TEST_CELL(i, i, c0);
		for (j=0; j<i; j++) {
			TEST_CELL(i, j, c0);
			TEST_CELL(j, i, c0);
		}
	}
	if (c0[0] < 0)	// never found a valid point
		return FALSE;

	// upper right
	for (i=0; i<clim; i++) {
		int x = xres-1 - i;
		TEST_CELL(x, i, c1);
		for (j=0; j<i; j++) {
			TEST_CELL(x, j, c1);
			TEST_CELL(xres-1-j, i, c1);
		}
	}
	if (c1[0] < 0)
		return FALSE;

	// lower left
	for (i=0; i<clim; i++) {
		int y = yres-1 - i;
		TEST_CELL(i, y, c2);
		for (j=0; j<i; j++) {
			TEST_CELL(i, yres-1-j, c2);
			TEST_CELL(j, y, c2);
		}
	}
	if (c2[0] < 0)
		return FALSE;

	// and lower right
	for (i=0; i<clim; i++) {
		int x = xres-1 - i;
		int y = yres-1 - i;
		TEST_CELL(x, y, c3);
		for (j=0; j<i; j++) {
			TEST_CELL(x, yres-1-j, c3);
			TEST_CELL(xres-1-j, y, c3);
		}
	}
	
	return c3[0] >= 0;
}

/// Create coarsest level, return TRUE if okay
// v0---v1
// |   /|
// |  / |
// | /  |
// v2---v3
static int initial_mesh()
{
	// find "corners" of range map
	int c0[2], c1[2], c2[2], c3[2];
	if (!corners(c0, c1, c2, c3))
		return FALSE;

	// setup mesh vertices for corners
	Vertex *v0 = new Vertex(c0[0], c0[1]);
	Vertex *v1 = new Vertex(c1[0], c1[1]);
	Vertex *v2 = new Vertex(c2[0], c2[1]);
	Vertex *v3 = new Vertex(c3[0], c3[1]);
	
	// these points don't morph to lower LOD
	v0->r1 = v1->r1 = v2->r1 = v3->r1 = NULL;

	// setup two triangles from these corners,
	// initializing list of edges to split for next LOD
	Edge *e1 = new Edge(v2, v0);
	Edge *e2 = new Edge(v0, v1);
	Edge *e3 = new Edge(v1, v3);
	Edge *e4 = new Edge(v3, v2);
	Edge *e5 = new Edge(v2, v1);

	(new Triangle(e1, 0, e2, 0, e5, 1))->fid = 1;
	(new Triangle(e3, 0, e4, 0, e5, 0))->fid = 0;
	asd_face_id = 2;

	return TRUE;
}

// (Re-)Write ASD mesh file header.
// Header size is fixed so the allocation sizes can be revised.
static void asd_header(FILE *fp, const char *tex)
{
	fprintf(fp, "#ASDMesh V1.0 ascii\n");
	fprintf(fp, "p normal texture\n");
	fprintf(fp, "n %8d %8d %8d 1\nt %s\n", 
		asd_vertex_id, asd_face_id, asd_vertex_id, tex);

	// model-to-world coordinate transform
	fputc('x', fp);
	for (int i=0; i<4; i++)
		fprintf(fp, " %f %f %f %f", obj2world[i][0],
			obj2world[i][1], obj2world[i][2], obj2world[i][3]);
	fputc('\n', fp);
	
	// triangle strip adjacency flag
	fprintf(fp, "s %d\n", (parms->adjacency) ? 1 : 0);
}

// extract stuff about a vertex (coordinates, normal, texture coords)
static void vertex_info(Vertex *v, double V[3], double N[3], double T[2])
{
	v->get_xyz(V);

	vector_copy(v->norm, N);

	// for texture, scale range map image coords to (0..1),
	// flipping Y axis
	T[0] = (double)v->ix / xres;
	T[1] = (double)(yres-1 - v->iy) / yres;
}

// Convert triangle pointer to face ID, handle NULL => -1
static int face_id(Triangle *t)
{
	return t ? t->fid : -1;
}

// Get ID for adjacent face (-1 if none)
static int adj_face(Triangle *t, Edge *e)
{
	return face_id(e->t1 == t ? e->t2 : e->t1);
}

// Write one ASD mesh vertex and corresponding attribute record.
//	morphing vertex:
//		v vertex-xyz dvertex-xyz
//	non-morphing vertex:
//		v vertex-xyz
// Returns file index.
static int asd_vertex(FILE *fp, Vertex *v)
{
	if (v->fid < 0) {	// not written yet?

		// assign next file vertex ID
		v->fid = asd_vertex_id++;

		// get vertex's final coordinates, normal, and texture coords
		double V[3], N[3], T[2];
		vertex_info(v, V, N, T);
			
		// write vertex line
		if (v->r1) {	// morphing vertex
			// reference point interpolated from parent edge
			double V1[3], V2[3], N1[3], N2[3], T1[2], T2[2];
			vertex_info(v->r1, V1, N1, T1);
			vertex_info(v->r2, V2, N2, T2);
				
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

			// in range map, center of 2D line could be
			// outside edge of 3D line. Trying this hack...
			if (f < 0.2 || f > 0.8)
				f = 0.5;

			// interpolate vertex at reference point
			int i;
			for (i=0; i<3; i++) {
				V1[i] += f * V1V2[i];
				N1[i] += f * (N2[i] - N1[i]);
			}
			normalize_vector(N1);
			T1[0] += f * (T2[0] - T1[0]);
			T1[1] += f * (T2[1] - T1[1]);

			// write final and morph delta vertex (ref-final)
			fprintf(fp, "v %f %f %f %g %g %g\n",
				V[0], V[1], V[2],
				V1[0]-V[0], V1[1]-V[1], V1[2]-V[2]);
			fprintf(fp, "a %f %f %f %g %g %g %f %f %g %g\n",
				N[0], N[1], N[2], 
				N1[0]-N[0], N1[1]-N[1], N1[2]-N[2],
				T[0], T[1], T1[0]-T[0], T1[1]-T[1]);


		} else {		// non-morphing vertex
			fprintf(fp, "v %f %f %f\n", V[0], V[1], V[2]);
			fprintf(fp, "a %f %f %f %f %f\n",
				N[0], N[1], N[2], T[0], T[1]);
		}
	}
	return v->fid;
}

// Is triangle face visible from camera? (if not, mark to not render)
static int is_visible(Triangle *t)
{
	// get triangle surface normal
	double V1[3], V2[3], V3[3], N[3];
	t->p1()->get_xyz(V1);
	t->p2()->get_xyz(V2);
	t->p3()->get_xyz(V3);
	if (parms->flip)	// clockwise
		surface_normal(V1, V3, V2, N);
	else
		surface_normal(V1, V2, V3, N);

	// camera is at 0,0,0; take V1-cam = V1 as view direction
	normalize_vector(V1);
	// view dot normal = cosine of face angle
	// Large enough cosine means small enough angle
	return dot_product(V1, N) > parms->cos_threshold;
}

// Write ASD data for one level of detail for ASD mesh format
static void write_lod(FILE *fp, int lod)
{
	Triangle *t;

	fprintf(fp, "l %d\n", lod);
	
	// set next LOD's triangle face IDs
	for (t=tlist[lod+1]; t; t=t->next)
		t->fid = asd_face_id++;

	// If want adjacency info, link edges to the triangles
	// that use them. It would be harder to do this on the fly, 
	// what with splitting, unsplitting, and pipeline issues
	if (parms->adjacency) {
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
		Vertex *p1 = t->p1();
		Vertex *p2 = t->p2();
		Vertex *p3 = t->p3();
		int v1 = asd_vertex(fp, p1);
		int v2 = asd_vertex(fp, p2);
		int v3 = asd_vertex(fp, p3);

		// reference vertices
 		int r1 = t->e1->is_split() ? asd_vertex(fp, t->e1->d1->p2) : -1;
		int r2 = t->e2->is_split() ? asd_vertex(fp, t->e2->d1->p2) : -1;
		int r3 = t->e3->is_split() ? asd_vertex(fp, t->e3->d1->p2) : -1;

		if (parms->flip)	// clockwise
			fprintf(fp, "f %d %d %d %d %d %d",
				v1, v2, v3, r1, r2, r3);
		else		// counterclockwise
			fprintf(fp, "f %d %d %d %d %d %d",
				v1, v3, v2, r3, r2, r1);

		// child faces
		fprintf(fp, " %d %d %d %d", 
			face_id(t->k1), face_id(t->k2),
			face_id(t->k3), face_id(t->k4));

		// texture #, vertex attributes, ref attributes
		int texnum = is_visible(t) ? 0 : -1;
		if (parms->flip)
			fprintf(fp, " %d %d %d %d %d %d %d",
				texnum, v1, v2, v3, r1, r2, r3);
		else
			fprintf(fp, " %d %d %d %d %d %d %d",
				texnum, v1, v3, v2, r3, r2, r1);

		fputc('\n', fp);

		// optional adjacency info
		if (parms->adjacency)
			fprintf(fp, "%d %d %d\n", adj_face(t, t->e1),
				adj_face(t, t->e2), adj_face(t, t->e3));
	}
}

// output result in intermediate ASD text format
static void write_asd(const char *name, const char *tex, int nlod)
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
	asd_header(asd_fp, tex);

	// write each LOD
	for (int i=0; i<nlod; i++)
		write_lod(asd_fp, i);

	// revise header with final allocations
	rewind(asd_fp);
	asd_header(asd_fp, tex);
}

/// Convert XYZ map to ASD model, return TRUE if successful.
// rmap = input XYZ map
// asd_name = output ASD ASCII format filename
// texname = texture map filename
// pptr = pointer to algorithm parameters structure
// cmod = transform camera model (camera to site frame, 
//	NULL if range map is already in camera frame)
int range2asd(ImageData *rmap, const char *asd_name, const char *tex_name,
	ASD_Parms *pptr, CAHVOR *cmod)
{
	// setup
	parms = pptr;
	imap = rmap;
	imap->get_res(&xres, &yres);
	cmap = new CMap(xres, yres);

	extract_transform(cmod);	// get site-to-camera transform
	transform_xyz();		// transform XYZ data to camera frame

	// create LOD zero mesh (coarsest LOD)
	if (!initial_mesh())
		return FALSE;
	tlist[0] = tri_list;

	if (parms->max_lod >= NUM_LOD) {
		fprintf(stderr, "Limited to %d LOD's\n", NUM_LOD);
		parms->max_lod = NUM_LOD-1;
	}

	// divide and cover (divide and conquer? duck and cover?!)
	// loop for each mesh refinement level
	int i;
	for (i=1; i<parms->max_lod; i++) {
		if (verbose)
			fprintf(stderr, "Building level %d\n", i);

		// advance active edge list
		elist[i-1] = edge_list;
		edge_list = NULL;

		// split edges for level i, count number of splits
		int n = split_edges(elist[i-1]);
		if (n > max_split)
			max_split = n;
		if (n == 0 || n * parms->done_factor < max_split)
			break;		// no more significant improvement

		// create triangles for level i
		tri_list = NULL;
		build_level(tlist[i-1]);
		tlist[i] = tri_list;
	}
	
	if (verbose)
		fprintf(stderr, "Writing ASD file %s\n", asd_name);
	write_asd(asd_name, tex_name, i);
	
	delete cmap;
	// ** also clean up vertices, edges, triangles!
	
	return TRUE;
}

//-----------------
// standalone main - move to another file to allow above function
// to be used in parallel processing slave.

static const char usage[] = 
"Usage: %s [-v] -i xyz_file -c rgb_file -o asd_file\n"
"  [-m cmod_file] [-lod n] [-min range] [-max range] [-d done_factor]\n"
"  [-e edge_limit] [-n norm_thresh] [-s sch_dist] [-sm thresh] [-a] [-f]\n\n"
"Creates ASD model 'asd_file' from XYZ (range) image 'xyz_file'\n"
"-v = verbose output\n"
"-c = RGB/grayscale texture map file\n"
"-m = camera model for world-to-camera transform\n"
"-d = 1/fraction of max split edges needed to keep iterating (default=500)\n"
"-e = max edge length in range image space (default=8)\n"
"-n = max view normal angle in degrees (default=90)\n"
"-s = max distance to search for split point in range image (default=100)\n"
"-a = output triangle adjacency info for stripping\n"
"-f = flip triangles to face -Z (implies model space is Y right/Z down)\n"
"-lod = maximum output levels of detail (default=10)\n"
"-sm  = smoothness threshold, max angle between input faces in degrees for\n"
"	averaging normals (default = 60)\n"
"-min/max = limits for valid range data\n";

int verbose = 1;

int main(int argc, char **argv)
{
	char *xyz_name = NULL;
	char *rgb_name = NULL;
	char *asd_name = NULL;
	char *cmod_name = NULL;
	ASD_Parms p;
	int 	i;
	
	// default parameters
	memset(&p, 0, sizeof(p));
	p.range_high = FLT_MAX;
	p.max_lod = 10;
	p.done_factor = 500;
	p.max_search_dist = 100;
	p.min_cos_norm = 0.5;	// default 60 degrees
	p.cos_threshold = 0.0;	// 90 degrees
	p.edge_limit = 8 * 8;

	for (i=1; i<argc; i++) {
		if (!strcmp(argv[i], "-v")) {
                        verbose = TRUE;
		} else if(!strcmp(argv[i], "-min")) {
			p.range_low = atof(argv[++i]);
			p.range_low *= p.range_low;	// squared range
		} else if(!strcmp(argv[i], "-max")) {
			p.range_high = atof(argv[++i]);
			p.range_high *= p.range_high;	// squared
		} else if(!strcmp(argv[i], "-lod")) {
			p.max_lod = atoi(argv[++i]);
		} else if(!strcmp(argv[i], "-sm")) {
			p.min_cos_norm = cos(TORADIANS * atof(argv[++i]));
		} else if(!strcmp(argv[i], "-a")) {
			p.adjacency = 1;
		} else if(!strcmp(argv[i], "-c")) {
			rgb_name = argv[++i];
		} else if(!strcmp(argv[i], "-d")) {
			p.done_factor = atoi(argv[++i]);
		} else if (!strcmp(argv[i], "-e")) {
			p.edge_limit = atoi(argv[++i]);
			p.edge_limit *= p.edge_limit;	// squared
		} else if(!strcmp(argv[i], "-f")) {
			p.flip = 1;
		} else if(!strcmp(argv[i], "-i")) {
			xyz_name = argv[++i];
		} else if(!strcmp(argv[i], "-m")) {
			cmod_name = argv[++i];
		} else if(!strcmp(argv[i], "-n")) {
			p.cos_threshold = cos(TORADIANS * atof(argv[++i]));
		} else if(!strcmp(argv[i], "-o")) {
			asd_name = argv[++i];
		} else if(!strcmp(argv[i], "-s")) {
			p.max_search_dist = atoi(argv[++i]);
		} else {
			fprintf(stderr, "Argument %s unrecognized.\n", argv[i]);
			fprintf(stderr, usage, argv[0]);
		        exit(1);
		}
	}

	// check required args
	if (!xyz_name || !rgb_name || !asd_name) {
		fprintf(stderr, "Required arguments missing\n");
		fprintf(stderr, usage, argv[0]);
		exit(1);
	}

	Image *rmap = new Image(xyz_name);
	if (rmap->get_data() == NULL) {
		fprintf(stderr, "error reading range map %s\n", xyz_name);
		return 1;
	}

	CAHVOR xcmod;
	if (cmod_name && xcmod.read(cmod_name)) {
		fprintf(stderr, "error reading camera model %s\n", cmod_name);
		return 1;
	}

	range2asd(rmap->get_data(), asd_name, rgb_name, &p,
			cmod_name ? &xcmod : NULL);
	return 0;
}
