// marchtri.C 1.3 05/01/26 09:40:34
/** \file
* Function to build triangle mesh from octree or forest of octrees,
* based on marching triangles algorithm and Delaunay constraint.
*/
#include <float.h>
#include "summitt_func.h"

enum {	USED_POINT = -2,	// nodespec ID for point used in the mesh
	MAX_BAD_SEEDS = 100	// max bad seed triangles before giving up
};

static Forest *forest;		// the input octree forest
static Octree *moct;		// merged octree model
static int ntrees;		// number of trees in forest
static ZMatrix *m2w, *w2m;	// model->world and world->model xform for each
static double *w2m_length;	// world->model length scaling
static Summitt_range *bvol;	// bounding volume of each octree
// temp: gdb has trouble with object same name as a type
static MT_Mesh gtm;		// generated triangle mesh
#define tm gtm
static int edge_count;		// number of new edges tested
static NodeSpec_List_Element *seed;	// seed triangle search point

static Boolean level_grow = TRUE; // ** not user-configurable?

// threshold of cosine of angle between normals to accept candidate
static double cos_threshold;

// max projection steps for candidate delaunay steps
static int max_proj_steps;

// number of bottom octree levels to accumulate after merging
static int accum_levels;

// validate input triangles against all other trees (including low-res)?
static int check_all_levels;

// +Z down (-Z up) for input data?
static int zdown;

// debug event counters
static const char *dcname[] = {
	"normal reject 1 (vertex)",		 	// 0
	"normal reject 2 (triangle)", 			// 1
	"normal points down",				// 2
	"new triangle edge used twice",			// 3
	"new triangle edge used once same dir",		// 4
	"delaunay search re-found initial points",	// 5
	"inner search found more points",		// 6
	"input triangle discarded",			// 7
	"edge projection didn't find new points",	// 8
	"no triangle from delaunay search",		// 9
	"colinear test triangle",			// 10
};
#define NDBG_CNT 11
int dbg_cnt[NDBG_CNT];
#define DCNT(i)	dbg_cnt[i]++
#define DCSHOW	for (int idc=0;idc<NDBG_CNT;idc++) \
			printf("dbg count %2d = %5d: %s\n", \
				idc, dbg_cnt[idc], dcname[idc]);

/// Perform delaunay test on a candidate triangle against a specific octree.
// Input vertices are in world coordinates.
// Return number of other points inside sphere, -1 if invalid triangle.
static int delaunay_test(double *V1, double *V2, double *V3, int tree, 
			NodeSpec_List_Element **nodelist)
{
	double VC[3], VCM[3];

	// find circumcenter and radius of trial triangle
	double radius = circumcenter(V1, V2, V3, VC);

	// check for colinearity
	if(radius < 0.0)
		return -1;

	// transform VC and radius to model coords
	MultPoints(VC, w2m[tree], VCM);
	radius *= w2m_length[tree];

	// use squared radius for faster testing, stay inside circle
	radius *= radius * 0.99;

	// get list of points within circumsphere, return count
	Octree *oct = (Octree *)forest->get_child(tree)->get_object();
	*nodelist = NULL;
	return oct->get_data()->find_points_within_radius(
			oct->get_max_levels(), VCM, VCM, radius, nodelist);
}

/// Same thing, but input vertices are already in model coordinates;
// search merged octree.
static int delaunay_test(double *V1, double *V2, double *V3,
	NodeSpec_List_Element **nodelist)
{
	double VC[3];

	// find circumcenter and radius of trial triangle
	double radius = circumcenter(V1, V2, V3, VC);

	// check for colinearity
	if(radius < 0.0)
		return -1;

	// use squared radius for faster testing, stay inside circle
	radius *= radius * 0.99;

	// get list of points within circumsphere, return count
	*nodelist = NULL;
	return moct->get_data()->find_points_within_radius(
			moct->get_max_levels(), VC, VC, radius, nodelist);
}

/// Add valid *connected* triangle to the mesh.
static void add_ctriangle(Mesh_Triangle *mt)
{
	NodeSpec *V1 = mt->get_vertex_1();	// was end 2 of edge
	NodeSpec *V2 = mt->get_vertex_2();	// was end 1 of edge
	NodeSpec *V3 = mt->get_vertex_3();	// is new vertex

	// previously verified that new edges aren't used already
	Mesh_Edge *me2 = new Mesh_Edge(V2, V3, V1);
	Mesh_Edge *me3 = new Mesh_Edge(V3, V1, V2);
	tm.add_edge_pair(me2, me3);
	// add triangle to list (must be unique)
	tm.add_triangle(mt);
	// mark nodes as used in mesh
	V1->id = V2->id = V3->id = USED_POINT;
}

/// Add valid *unconnected* triangle and edges to the mesh.
// Triangle should be unique and no edges should already be used twice.
static void add_triangle(Mesh_Triangle *mt)
{
	NodeSpec *V1 = mt->get_vertex_1();
	NodeSpec *V2 = mt->get_vertex_2();
	NodeSpec *V3 = mt->get_vertex_3();

	tm.add_edge_if_unique(new Mesh_Edge(V1, V2, V3));
	tm.add_edge_if_unique(new Mesh_Edge(V2, V3, V1));
	tm.add_edge_if_unique(new Mesh_Edge(V3, V1, V2));
	tm.add_triangle(mt);

	// mark nodes as used in mesh
	V1->id = V2->id = V3->id = USED_POINT;
}

/// Search merged octree for triangle meeting delaunay test, given 
// candidate triangle. Returns mesh triangle or NULL.
// Vertices 1 and 2 are prechosen, vertex 3 is the trial one
// Pnorm = projected normal
static Mesh_Triangle *delaunay_search(NodeSpec *vertex1, NodeSpec *vertex2, 
		NodeSpec *vertex3, double *Pnorm, int top_level = TRUE)
{
	double	V1[3], V2[3], V3[3];
	double	test_norm[3];
	NodeSpec_List_Element *nodelist, *tmp_node;

	vertex1->get_global_center(V1);
	vertex2->get_global_center(V2);
	vertex3->get_global_center(V3);

	// get list of points inside circumsphere of candidate triangle
	int count = delaunay_test(V1, V2, V3, &nodelist);

	if (count != 0) {
		if (count < 0)  {	// invalid candidate triangle
			DCNT(10);
			return NULL;
		}
		if (!top_level) {
			nodelist->cleanup();
			return NULL;
		}
		if (count >= 20) {	// oops, bad choice
			DCNT(6);
			nodelist->cleanup();
			return NULL;
		}

		// recursively try the inside points
		for (tmp_node=nodelist; tmp_node; tmp_node=tmp_node->next) {
			// already tried this point for this edge?
			if (tmp_node->nodespec->id == edge_count)
				continue;	// yes, don't test again
			// no, tag it so it's only tried once
			tmp_node->nodespec->id = edge_count;

			Mesh_Triangle *mt = delaunay_search(vertex1, vertex2, 
				tmp_node->nodespec, Pnorm, FALSE);
			if (mt) {	// found valid triangle
				nodelist->cleanup();
				return mt;
			}
		}

		nodelist->cleanup();
	}

	// the incoming points are the best
		
	// check point normal versus expected surface normal
	if (vertex3->get_normal(test_norm)) {
		double dp = dot_product(Pnorm, test_norm);
		if (zdown)		// flip sign
			dp = -dp;
		if (dp < cos_threshold) {
			DCNT(0);	// reject
			return NULL;
		}
	}

	// check triangle normal versus expected surface normal
	surface_normal(V1, V2, V3, test_norm);
	if (test_norm[2] < 0.0)
		DCNT(2);		// face points down?
	if (dot_product(Pnorm, test_norm) < cos_threshold) {
		DCNT(1);
		return NULL;
	}

	// check previous usage of potential new edges
	if (tm.is_old_edge(vertex2, vertex3) || 
				tm.is_old_edge(vertex3, vertex1)) {
		DCNT(3);
		return NULL;
	}
	if (tm.is_dup_edge(vertex2, vertex3) ||
				tm.is_dup_edge(vertex3, vertex1)) {
		DCNT(4);
		return NULL;
	}
	
	// This triangle is acceptable
	return new Mesh_Triangle(vertex1, vertex2, vertex3);
}

/// Determine whether input triangles in "tree" should be tested
/// against points in "otree".
static int dont_test(int otree, int tree)
{
	if (otree == tree)	// never test against self
		return TRUE;
	if (check_all_levels)	// check all others?
		return FALSE;
	// return TRUE (don't test) if otree/to is lower res (less levels)
	Octree *to   = (Octree *)forest->get_child(otree)->get_object();
	Octree *from = (Octree *)forest->get_child(tree)->get_object();
	return to->get_max_levels() < from->get_max_levels();
}

/// Is triangle vertex outside the bounding volume 
/// of the specified tree (all done in world coordinates)?
static int is_outside(double v[3], int tree)
{
	return !bvol[tree].in_range(v);
}

/// Scan any triangles already in forest surface nodes
// (they might have been created using model connectivity when
// octrees where built, more efficiently than searching for
// triangles later).
// Check them with Delaunay constraint against other nodes
// in the forest. Setup master mesh data with the valid ones.
static void validate_triangles()
{
	int tri_count = 0;

	for (int tree=0; tree<ntrees; tree++) {
		SfcModel *sfc = (SfcModel *)forest->get_child(tree)->get_object();
		// if going to accumulate merged tree levels,
		// can't use any input mesh data (accumulation messes
		// up previous connectivity)
		if (accum_levels) {
			sfc->mesh.clean_up();
			continue;
		}

		if (verbose)
			fprintf(stderr, 
				"validating model %d's triangles\n", tree);
		// move tree's point list to master mesh
		NodeSpec_List_Element *last = sfc->mesh.pt_list;
		if (last) {
			while (last->next)	// find end of this tree's list
				last = last->next;
			last->next = tm.pt_list; // append rest of master list
			tm.pt_list = sfc->mesh.pt_list;
			tm.pt_count += sfc->mesh.pt_count;
		}

		// ** maybe build list of which other trees have
		// ** intersecting bounding volumes, to shortcut
		// ** triangle testing

		// scan tree's triangle list
		Mesh_Triangle *tri;
		while ((tri = sfc->mesh.tri_list) != NULL) {
			if (verbose && (++tri_count % 5000) == 0)
				fprintf(stderr, "Input tri ct = %d\n", tri_count);
			// remove from input mesh
			sfc->mesh.tri_list = tri->next;

			// is this still a good triangle?
			double mv[3], V1[3], V2[3], V3[3];
			tri->get_vertex_1()->get_global_center(mv);
			MultPoints(mv, m2w[tree], V1);
			tri->get_vertex_2()->get_global_center(mv);
			MultPoints(mv, m2w[tree], V2);
			tri->get_vertex_3()->get_global_center(mv);
			MultPoints(mv, m2w[tree], V3);

			// Do delaunay test against selected other octrees.
			// Only care whether or not any points exist inside sphere.
			NodeSpec_List_Element *nodelist = NULL;	// start list empty
			for (int otree=0; otree<ntrees; otree++) {
				if (dont_test(otree, tree))
					continue;	// skip this tree?

				// bounding volume shortcut test
				// (assuming triangle doesn't completely enclose
				// the other tree!)
				if (is_outside(V1, otree) && 
						is_outside(V2, otree) && 
						is_outside(V3, otree))
					continue;
				
				int n = delaunay_test(V1, V2, V3, otree, &nodelist);
				if (n != 0) {		// found points (or bad triangle)
					nodelist->cleanup();
					delete tri;
					tri = NULL;
					DCNT(7);
					break;
				}
				// else keep checking
			}

			if (tri)			// still okay
				add_triangle(tri);
		}
	}
}

// Search for new triangle starting with given point (not in mesh yet).
// Return TRUE if successful.
static int new_triangle_with(NodeSpec *N1)
{
	double v1[3], v2[3], v3[3];
	N1->get_global_center(v1);
	double radius = N1->edge_length / 1000.0;

	// search wider and wider until found at least 3 unused points
	int i, n;
	NodeSpec_List_Element *nodelist;
	for (i=0; ; i++) {
		nodelist = NULL;
		n = moct->get_data()->find_points_within_radius(
				moct->get_max_levels(), v1, v1, 
				radius, &nodelist);
		if (n >= 3)
			break;	// found something (besides start point)
		nodelist->cleanup();

		if (i > 20)	// not enough unused points left?
			return FALSE;
		radius *= 2.0;	// expand search
	}

	// second point is unused point closest to first one
	double min_d = FLT_MAX;
	NodeSpec *N2 = NULL;
	NodeSpec_List_Element *nle;
	for (nle=nodelist; nle; nle = nle->next) {
		NodeSpec *ns = nle->nodespec;
		if (ns != N1 && ns->id != USED_POINT) {
			// new point, is it closest?
			ns->get_global_center(v2);
			double d = distance_sqr(v1, v2);
			if (d < min_d) {	// yes, keep it
				min_d = d;
				N2 = ns;
			}
		}
	}

	if (N2 == NULL) {
		// none of the nearby points were unused. If we were
		// desparate to avoid holes, we might continue with
		// the next wider search, but probably would be a time waste
		nodelist->cleanup();
		return FALSE;
	}

	// now search the rest of the nearby points for something that
	// makes a valid delaunay triangle with N1 and N2. Looking for a
	// reasonably upward surface normal.
	for (nle=nodelist; nle; nle = nle->next) {
		NodeSpec *N3 = nle->nodespec;
		if (N3 == N1 || N3 == N2 || N3->id == USED_POINT)
			continue;

		double Pnorm[3];
		N3->get_global_center(v3);
		surface_normal(v1, v2, v3, Pnorm);
		if (Pnorm[2] < -0.5) {		// okay but flipped
			// swap N1 and N2
			NodeSpec *t = N1;
			N1 = N2;
			N2 = t;
			vector_scale(Pnorm, -1.0, Pnorm);
		} else if (Pnorm[2] < 0.5) {
			// face is too vertical, try new 3rd point
			continue;
		}

		// normal is okay, look for valid mesh triangle
		Mesh_Triangle *mt;
		if ((mt = delaunay_search(N1, N2, N3, Pnorm)) != NULL) {
			if (verbose)
				fprintf(stderr, "Seed triangle found\n");
			add_triangle(mt);
			nodelist->cleanup();
			return TRUE;
		}
	}

	// no valid triangle found from nearby points. If we were
	// desparate to avoid holes, we might continue with
	// the next wider search, but probably would be a time waste
	nodelist->cleanup();
	return FALSE;
}

// Search for a seed triangle using points not yet in the mesh.
// Return TRUE if found one.
static int seed_triangle()
{
	if (verbose)
		fprintf(stderr, "Seed triangle search, edge_count=%d\n",
				edge_count);

	// find a point not used in the mesh
	for (; seed; seed = seed->next) {
		if (seed->nodespec->id != USED_POINT) {
			// look for nearby points to make a triangle
			if (new_triangle_with(seed->nodespec))
				return TRUE;
		}
	}

	if (verbose)
		fprintf(stderr, "Seed triangle search failed\n");
	return FALSE;
}

// Try to project existing edge into new mesh triangle.
// Return new triangle pointer, or NULL if failed.
static Mesh_Triangle *project_edge(Mesh_Edge *me)
{
	double	P1[3], P2[3], P3[3], *PO, step_size;
	double	Pnorm[3];

	me->get_end_1(P1);
	me->get_end_2(P2);
	me->get_other(P3);
	step_size = distance(P1,P2) * 0.3;
	surface_normal(P1, P2, P3, Pnorm);

	NodeSpec *ns = NULL;
	for (int step_count=0; ; step_count++) {
		PO = project_point(P1, P2, P3, step_size);
		// find closest model point to projection
		moct->get_data()->fast_find_closest(moct->get_max_levels(), 
			FLT_MAX, PO, PO, &ns);
		free(PO);

		// is closest point not one of edge endpoints?
		if (ns != me->get_end_1() && ns != me->get_end_2() &&
					ns != me->get_other()) {
			// it's not, so we have valid new point
			Mesh_Triangle *mt = delaunay_search(me->get_end_2(), 
				me->get_end_1(), ns, Pnorm);
			if (!mt) {
				DCNT(9);	// search failed
			} else {
				add_ctriangle(mt);
				// move first edge to "old" list
				tm.add_old_edge(me);
				return mt;
			}

			// For less chance of leaving holes,
			// keep looping here. But slows things down, so
			return NULL;
		} 

		// need to look farther
		if (step_count >= max_proj_steps)
			break;
		step_size *= 2.0;
	}

	return NULL;	// failed
}

/// Build triangle mesh model
// Returns pointer to static MT_Mesh object, or NULL if failed.
// See make_tris.C for explanation of arguments.
MT_Mesh *make_triangle_model_from_forest(Forest *fst,
		double cos_thresh, int max_proj, int acc_levels,
		int check_all, int z_is_down, int skip_new_mesh)
{
	// copy args to static variables
	forest = fst;
	cos_threshold = cos_thresh;
	zdown = z_is_down;
	max_proj_steps = max_proj;
	accum_levels = acc_levels;
	check_all_levels = check_all;

	// initialize static data
	edge_count = 0;
	memset(dbg_cnt, 0, sizeof(dbg_cnt));
	tm.clean_up();	// in case stuff left over from previous call

	// build transforms for model->world and back for each tree
	ntrees = forest->get_num_children();
	m2w = new ZMatrix[ntrees];
	w2m = new ZMatrix[ntrees];
	w2m_length = new double[ntrees];
	bvol = new Summitt_range[ntrees];
	int tree;
	for (tree=0; tree<ntrees; tree++) {
		ObjNode *node = forest->get_child(tree);
		SfcModel *sfc = (SfcModel *)node->get_object();
		
		sfc->freeze_xform();	// no changes, cache matrices
		node->GetTransformationMatrix(m2w[tree]);	
		MatInvert(m2w[tree], w2m[tree]);
		
		// compute world-to-model length scale factor
		// assuming X, Y, Z scale factors are about the same
		w2m_length[tree] = 1.0 / (node->xscale * sfc->xscale);		
		// determine bounding volume in world space
		// (skip this to save time if only one input tree)
		if (ntrees > 1)
			summitt_bounding_volume(node, &bvol[tree]);
	}
	
	// validate and copy input triangles, if any; also merge point lists
	// (if going to accumulate levels, this frees up any input meshes)
	validate_triangles();

	// merge all input points to a single octree for building rest of mesh.
	// First octree is assumed to be "base" low-res model
	for (tree=1; tree<ntrees; tree++) {
		if (verbose)
			fprintf(stderr, 
				"merging model %d to base octree\n", tree);		
		summitt_merge_octrees(forest->get_child(tree), 
				forest->get_child(0), FALSE, level_grow);
	}
	if (verbose)
		fprintf(stderr, "Merging complete.\n");

	// the merged octree	
	moct = (Octree *)forest->get_child(0)->get_object();

	// optionally accumulate lower levels to smooth things out
	if (accum_levels) {
		int new_levels = moct->get_max_levels() - accum_levels;
		if (verbose)
			fprintf(stderr, 
				"Accumulating merged tree to %d levels\n",
				new_levels);
		moct->set_max_levels(new_levels, TRUE);
		// initialize mesh with new point list
		tm.add_pt_list(moct);
	}	

	if (verbose) {
		fprintf(stderr, "Initial mesh stats:\n");
		tm.stats();
	}

	// test option to just look at validated/merged input
	if (skip_new_mesh)
		return &tm;

	// mark all points as not used in mesh yet
	NodeSpec_List_Element *nle = tm.pt_list;
	while (nle) {
		nle->nodespec->id = -1;	// not USED_POINT and not edge_count
		nle = nle->next;
	}

	int badseeds = 0;
	seed = tm.pt_list;		// initialize search

	// loop until nothing left to mesh
	for (;;) {
		Mesh_Edge *me;

		// process "new edge" list, adding more triangles as we go
		int seed_start = edge_count;
		while (me = tm.next_new_edge()) {
			edge_count++;
			if (verbose && (edge_count % 5000) == 0 )
				fprintf(stderr, "Edges: %d\n", edge_count);
			if (!project_edge(me)) {
				// edge projection didn't find anything
				DCNT(8);
				tm.save_unextended_edge(me);
			}
 		}

		// look for a next seed triangle, unless last several
		// seeds have been unproductive
		if (edge_count - seed_start < 15) {
			if (++badseeds > MAX_BAD_SEEDS) {
				if (verbose)
					fprintf(stderr, "Too many bad seeds\n");
				break;
			}
		} else {
			badseeds = 0;	// last one was okay, restart count
		}

		if (!seed_triangle())
			break;		// no more
	}
	
	if (verbose) {
		fprintf(stderr,"Processed %d edges; final stats:\n", 
			edge_count);
		tm.stats();
		DCSHOW;
	}

	// clean up
	delete [] m2w;
	delete [] w2m;
	delete [] w2m_length;
	delete [] bvol;

	if (edge_count)
		return &tm;
	return NULL;			// else failed
}
