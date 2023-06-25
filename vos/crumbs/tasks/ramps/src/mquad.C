// Marching quadrilaterals mesher
// 9/02 Jack Morrison
// Logic adapted from "marching triangles" algorithm, but
// producing quads suitable for Performer ASD mesh.

#include <stdio.h>
#include "summitt_func.h"

// diagnostic counters
static int reject1;	// projection found existing vertex
static int reject2;	// projection didn't find anything
static int num_quad;	// number of quadilaterals built

int verbose = 1;

static Octree_Data *od;		// input model
static MT_Mesh tm;		// generated mesh

// configuration
double start_proj = 0.25;	// start projection factor for new point
double end_proj = 5.0;		// end projection factor

struct Vertex;
struct Edge;
struct Quad;

struct Vertex {
	NodeSpec *ns;		// voxel with coordinates, normal
	Edge *e[4];		// up to 4 edges using me

	Vertex(NodeSpec *n) {
		ns = n;
		ns->id = (int)this;	// point nodespec back to me
		e[0] = e[1] = e[2] = e[3] = NULL;
	}

	// record a new edge that's using this vertex
	void add_edge(Edge *edge) {
		if (!e[1])
			e[1] = edge;
		else if (!e[2])
			e[2] = edge;
		else if (!e[3])
			e[3] = edge;
		else {
			fprintf(stderr, "Too many edges for vertex!\n");
			exit(1);
		}
	}

	// number of edges using this vertex so far
	int num_edges() {
		if (e[3])
			return 4;
		if (e[2])
			return 3;
		if (e[1])
			return 2;
		return 1;
	}

	int has_4_edges() {
		return e[3] != NULL;
	}
};

struct Edge {
	Edge *next;		// list link
	Vertex *v[2];		// vertices of this edge
	Quad *q[2];		// quadrilaterals using this edge
				// q[0] uses edge in order v[0]-v[1] (CW)
				// q[1] is initially unknown

	static Edge *elist;	// list of new edges

	Edge(Vertex *v0, Vertex *v1) {
		v[0] = v0;		// store fields
		v[1] = v1;
		q[0] = q[1] = NULL;
		v0->add_edge(this);	// link back
		v1->add_edge(this);
		next = elist;		// add to list
		elist = this;
	}

	void add_quad(Quad *quad) {	// connect into a quad
		if (q[0] == NULL)
			q[0] = quad;
		else if (q[1]) {
			fprintf(stderr, "Edge used by 3rd quad\n");
			exit(1);
		} else
			q[1] = quad;
	}
};

struct Quad {
	Quad *next;		// list link
	Edge *e[4];		// edges making up the quad
	// ** maybe only need to store 3 of them?

	static Quad *qlist;	// list of created quadrilaterals

	Quad(Edge *e0, Edge *e1, Edge *e2, Edge *e3) {
		e[0] = e0;		// store fields
		e[1] = e1;
		e[2] = e2;
		e[3] = e3;
		e0->add_quad(this);	// link back
		e1->add_quad(this);
		e2->add_quad(this);
		e3->add_quad(this);
		next = qlist;		// add to list
		qlist = this;
	}
};

// define statics
Edge *Edge::elist;
Quad *Quad::qlist;

// get next edge off elist to be extended
Edge *next_edge()
{
	Edge *e = Edge::elist;
	if (e)
		Edge::elist = e->next;	// remove from list
	return e;
}

// Find existing vertex v such that
// we already have an edge going from v to other
// and that edge is not part of quad q.
// Also set ep to that existing edge.
Vertex *find_vert1(Vertex *other, Quad *q, Edge **ep)
{
	// must be one of the edges using 'other'
	for (int i=0; i<4; i++) {
		Edge *e = other->e[i];
		if (!e)
			break;
		Vertex *v = e->v[0];	// first vertex of this edge
		if (v == other)
			continue;	// edge is wrong direction
		if (e->q[0] == q || e->q[1] == q)
			continue;	// edge part of existing quad
		*ep = e;
		return v;		// found it
	}
	fprintf(stderr, "Can't find vertex 1\n");
	exit(1);
	return NULL;
}

// Find existing vertex v such that
// we already have an edge going from other to v
// and that edge is not part of quad q.
// Also set ep to that existing edge.
Vertex *find_vert2(Vertex *other, Quad *q, Edge **ep)
{
	// must be one of the edges using 'other'
	for (int i=0; i<4; i++) {
		Edge *e = other->e[i];
		if (!e)
			break;
		Vertex *v = e->v[1];	// second vertex of this edge
		if (v == other)
			continue;	// edge is wrong direction
		if (e->q[0] == q || e->q[1] == q)
			continue;	// edge part of existing quad
		*ep = e;
		return v;		// found it
	}
	fprintf(stderr, "Can't find vertex 2\n");
	exit(1);
	return NULL;
}

// Look for new vertex by projecting from existing edge a->b
Vertex *find_vert3(Vertex *b, Vertex *a)
{
	double pa[3], pb[3], pp[3];
	a->ns->get_global_center(pa);
	b->ns->get_global_center(pb);

	// project out from b
	for (double proj=start_proj; proj<=end_proj; proj *= 2.0) {
		pp[0] = pb[0] + proj*(pb[0] - pa[0]);
		pp[1] = pb[1] + proj*(pb[1] - pa[1]);
		pp[2] = pb[2] + proj*(pb[2] - pa[2]);

		NodeSpec *ns;
		od->fast_find_closest(od->get_level(),
    			(double)FLT_MAX, pp, pp, &ns);

		if (ns == b->ns)	// closest is still b?
			continue;

		// found another point already in the mesh?
		if (ns->id) {
			// ** might still be able to hook to it...
			reject1++;
			return NULL;
		}
		// ** check normal vector

		// okay, create vertex
		return new Vertex(ns);
	}

	// didn't find anything
	reject2++;
	return NULL;
}

// Find existing vertex that precedes v in quad q
Vertex *prev_vert(Quad *q, Vertex *v)
{
	for (int i=0; i<4; i++) {
		if (q->e[i]->v[1] == v)
			return q->e[i]->v[0];
	}
	fprintf(stderr, "Can't find prev vertex\n");
	exit(1);
	return NULL;
}

// Find existing vertex that follows v in quad q
Vertex *next_vert(Quad *q, Vertex *v)
{
	for (int i=0; i<4; i++) {
		if (q->e[i]->v[0] == v)
			return q->e[i]->v[1];
	}
	fprintf(stderr, "Can't find next vertex\n");
	exit(1);
	return NULL;
}

// Find possibly existing edge going from v0 to v1
Edge *find_edge(Vertex *v0, Vertex *v1)
{
	for (int i=0; i<4; i++) {
		Edge *e = v0->e[i];
		if (e == NULL)
			return NULL;
		if (e->v[1] == v1)
			return e;
	}
	return NULL;
}

// Try to extend this edge into a new quadrilateral.
// Looking for vertices va0 (adjacent to v[0]) and va1 (adjacent to v[1])
void extend_edge(Edge *e)
{
	Vertex *v0 = e->v[0];
	Vertex *v1 = e->v[1];
	Vertex *va0, *va1;

	// Look for vertex a off v0. Possibilities are:
	//
	//   (|)
	// ---+v0		nothing there, may have edge going other way
	//   e|
	// ---+
	//
	//    |       |
	// ---+v0-e0--+a	already have edge
	//   e|
	// ---+

	Edge *e0 = NULL;
	if (v0->has_4_edges()) {	// already have edge
		va0 = find_vert1(v0, e->q[0], &e0);
	} else {
		va0 = find_vert3(v0, prev_vert(e->q[0], v0));
		if (va0 == NULL)
			return;
	}

	// Look for vertex b off v1. Possibilities are:
	//
	// ---+			nothing there, may have edge going other way
	//   e|
	// ---+v1
	//   (|)
	//
	// ---+			already have edge
	//   e|
	// ---+v1-e1--+b
	//    |       |

	Edge *e1 = NULL;
	if (v1->has_4_edges()) {
		va1 = find_vert2(v1, e->q[0], &e1);
	} else {
		va1 = find_vert3(v1, next_vert(e->q[0], v1));
		if (va1 == NULL) {
			// release va0
			va0->ns->id = 0;
			delete va0;
			return;
		}
	}

	// okay, build the new quadrangle and link everything up
	if (e0 == NULL)	// edge is new
		e0 = new Edge(v0, va0);
	if (e1 == NULL)
		e1 = new Edge(va1, v1);
	// new edge va0-va1 may already exist in opposite direction
	Edge *e2 = find_edge(va1, va0);
	if (e2 == NULL)
		e2 = new Edge(va0, va1);
	Quad *q = new Quad(e, e0, e2, e1);
	num_quad++;
}

// Scan nodespec list for unused point closest to v
static NodeSpec *closest_unused(NodeSpec_List_Element *list, double *v)
{
	double min_d = FLT_MAX;
	NodeSpec *min_n = NULL;
	NodeSpec_List_Element *nle;
	for (nle=list; nle; nle = nle->next) {
		NodeSpec *ns = nle->nodespec;
		if (ns->id == 0) {
			// new point, is it closest?
			double nv[3];
			ns->get_global_center(nv);
			double d = distance_sqr(v, nv);
			if (d < min_d) {	// yes, keep it
				min_d = d;
				min_n = ns;
			}
		}
	}
	return min_n;
}

// Search for new quad starting with given point (not in mesh yet).
// Return TRUE if successful.
static int new_quad_with(NodeSpec *N1)
{
	double v[3];
	N1->get_global_center(v);
	double radius = N1->edge_length / 1000.0;

	// search wider and wider until found at least 4 unused points
	int i, n;
	NodeSpec_List_Element *nodelist;
	for (i=0; ; i++) {
		nodelist = NULL;
		n = od->find_points_within_radius(od->get_level(),
				v, v, radius, &nodelist);
		if (n >= 4)
			break;	// found something (besides start point)
		nodelist->cleanup();

		if (i > 20)	// not enough unused points left?
			return FALSE;
		radius *= 2.0;	// expand search
	}

	// temporarily reserve N1
	N1->id = -1;

	// other points are closest unused ones
	NodeSpec *N2 = closest_unused(nodelist, v);
	if (N2 == NULL) {
		// none of the nearby points were unused. If we were
		// desperate to avoid holes, we might continue with
		// the next wider search, but probably would be a time waste
		nodelist->cleanup();
		N1->id = 0;
		return FALSE;
	}
	N2->id = -1;
	NodeSpec *N3 = closest_unused(nodelist, v);
	if (N3 == NULL) {
		nodelist->cleanup();
		N1->id = N2->id = 0;
		return FALSE;
	}
	N3->id = -1;
	NodeSpec *N4 = closest_unused(nodelist, v);
	if (N4 == NULL) {
		nodelist->cleanup();
		N1->id = N2->id = N3->id = 0;
		return FALSE;
	}

	// ** check normals for reasonableness

	// okay, build the new quadrangle and link everything up
	if (verbose)
		fprintf(stderr, "Seed quad found\n");

	nodelist->cleanup();

	Vertex *v1 = new Vertex(N1);
	Vertex *v2 = new Vertex(N2);
	Vertex *v3 = new Vertex(N3);
	Vertex *v4 = new Vertex(N4);
	Edge *e1 = new Edge(v1, v2);
	// ** should choose preferred order on 3/4
	Edge *e2 = new Edge(v2, v3);
	Edge *e3 = new Edge(v3, v4);
	Edge *e4 = new Edge(v4, v1);
	Quad *q = new Quad(e1, e2, e3, e4);
	num_quad++;

	return TRUE;
}

// Look for next seed quadrilateral using points not yet in the mesh.
// Return TRUE if found one.
static int initial_quad()
{
	if (verbose)
		fprintf(stderr, "Seed quad search\n");

	// find a point not used in the mesh
	static NodeSpec_List_Element *nle;
	if (nle == NULL)	// first time
		nle = tm.pt_list;
	for (; nle; nle = nle->next) {
		if (nle->nodespec->id == 0) {
			// look for nearby points to make a quad
			if (new_quad_with(nle->nodespec))
				return TRUE;
		}
	}

	if (verbose)
		fprintf(stderr, "Seed quad search failed\n");
	return FALSE;
}

// load input octree
static void load_model()
{
	FILE_Dataport fp;
	fp.open(stdin);

 	// check input file type
 	char token[4096];
 	get_next_token(&fp, token);
	if (strcmp(token, "OCTREE_V1")) {
		fprintf(stderr, "Invalid input type token %d\n", token);
		exit(1);
	}
	Octree *oct = new Octree;
	if (!oct->parse_in(&fp))
		exit(1);
	od = oct->get_data();

	// get point list, mark all voxels as not used in mesh (id=0)
	tm.add_pt_list(oct);
	for (NodeSpec_List_Element *p = tm.pt_list; p; p=p->next)
		p->nodespec->id = 0;
	if (verbose)
		fprintf(stderr,"%d voxels read\n", tm.pt_count);
}

int main(int argc, char **argv)
{
	load_model();

	for (;;) {
		// look for seed quadrilateral, adding 4 edges
		if (!initial_quad())
			break;

		// extend those edges
		for (;;) {
			Edge *e = next_edge();
			if (e == NULL)	// no more to extend?
				break;
			extend_edge(e);
		}

		break;	// ** for now, just one seed
	}

	if (verbose) {	// diagnostics
		fprintf(stderr, "%6d quads created\n", num_quad);
		fprintf(stderr, "%6d proj found existing vertex\n", reject1);
		fprintf(stderr, "%6d proj found nothing\n", reject2);
	}
	
	// write triangle mesh (splitting quads) (ASD file?)
	// ** temp, write quads in crude .obj file to stdout
	int vnum = 1;
	for (Quad *q = Quad::qlist; q; q=q->next) {
		Vertex *v[4];
		v[0] = q->e[0]->v[0];
		v[1] = q->e[0]->v[1];
		v[2] = q->e[1]->v[1];
		if (v[2] == v[1])
			v[2] = q->e[1]->v[0];
		v[3] = q->e[2]->v[1];
		if (v[3] == v[2])
			v[3] = q->e[2]->v[0];
		// ** may be wrong winding!
		for (int i=0; i<4; i++) {
			double p[3];
			v[i]->ns->get_global_center(p);
			printf("v %f %f %f\n", p[0], p[1], p[2]);
		}
		printf("f %d %d %d %d\n", vnum, vnum+1, vnum+2, vnum+3);
		vnum += 4;
	}
}
