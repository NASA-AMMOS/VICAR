// Test to flip edges in triangle mesh in order to allow
// decomposition into ASD levels. The ASD model requires
// at most 6 edges connecting to each vertex, so that 2 edges
// can be combined to form a lower LOD edge and the other 4 edges
// form internal triangles of the lower LOD triangles on either side
// of the combined edge.

#include <stdio.h>
#include "grape/sfcmodel.h"

enum { MAX_EDGE	= 15 };		// max edges expected at any one vertex

class Vertex;

class Edge {
public:
	Vertex *v1, *v2;	// vertices connected by this edge

	// create edge
	Edge(Vertex *a, Vertex *b) : v1(a), v2(b) {}

	// is this me?
	int matches(Vertex *a, Vertex *b) {
		return ((a==v1 && b==v2) || (a==v2 && b==v1)); }
};

// Create new edge if not already defined
class Vertex {
public:
	NodeSpec *ns;		// point to model data (coordinates, etc.)
	Edge *e[MAX_EDGE];	// edges connected to this vertex
	int nedge;		// number of connected edges

	// create new vertex
	Vertex() : nedge(0) {}

	// add edge to vertex
	void add_edge(Edge *ne) {
		// is vertex full?
		if (nedge >= MAX_EDGE) {
			fprintf(stderr, "Too many edges for vertex (max %d)\n",
				MAX_EDGE);
			exit(1);
		}
		// sanity check
		if (ne->v1 != this && ne->v2 != this) {
			fprintf(stderr, "Adding invalid edge!\n");
			exit(1);
		}
		e[nedge++] = ne;
	}

	// return vertex on other end of specified edge
	Vertex *other(Edge *se) {	// by edge pointer
		if (se->v1 == this)
			return se->v2;
		return se->v1;
	}

	Vertex *other(int i) {		// by index
		return other(e[i]);
	}

	// remove edge, specified by index
	void remove_edge(int i) {
		if (--nedge > i)
			memmove(&e[i], &e[i+1], (nedge-i) * sizeof(e[0]));
	}

	// remove edge, specified by pointer
	void remove_edge(Edge *se) {
		for (int i=0; i<nedge; i++) {
			if (e[i] == se) {
				remove_edge(i);
				return;
			}
		}
		fprintf(stderr, "Bogus edge remove!\n");
		exit(1);
	}
};

// debug - show vertices connected to me, and their vertices
void dump_others(Vertex *v, int top=1)
{
	if (top)
		printf("Connections to %X:\n", (int)v);
	for (int i=0; i<v->nedge; i++) {
		Vertex *c = v->other(i);
		if (top) {
			printf("%X\n", (int)c);
			dump_others(c, 0);
		} else {
			printf(" %X\n", (int)c);
		}
	}
}

// Is there an existing edge between these vertices?
int have_edge(Vertex *v1, Vertex *v2)
{
	for (int i=0; i<v1->nedge; i++) {
		if (v1->e[i]->matches(v1, v2))
			return TRUE;
	}
	return FALSE;
}

// Add new edge, if not a duplicate (in other direction)
Edge *new_edge(Vertex *v1, Vertex *v2)
{
	if (have_edge(v1, v2))
		return NULL;	// already have it

	// okay, create edge and add to vertices
	Edge *e = new Edge(v1, v2);
	v1->add_edge(e);
	v2->add_edge(e);
	return e;
}

// find vertices attached to both v1 and v2, return number found (0-2)
int find_adjacent(Vertex *v1, Vertex *v2, Vertex **a1, Vertex **a2)
{
	int found = 0;
	for (int e1=0; e1<v1->nedge; e1++) {
		Vertex *other = v1->other(e1);
		if (other == v2)	// skip edge v1-v2
			continue;
		for (int e2=0; e2<v2->nedge; e2++) {
			// other side of e1 = other side of e2?
			if (other == v2->other(e2)) {
				if (++found == 1)
					*a1 = other;	// first one
				else if (found == 2) {
					*a2 = other;
					return found;	// can't be more
				}
			}
		}
	}
	return found;
}

// check that quadrilateral is convex so an edge flip is reasonable
int is_convex(Vertex *v, Vertex *a1, Vertex *a2, Vertex *other)
{
	// check that midpoint of a1-a2 is closer to v than other is
	// (not rigourous, but then 3d quadrilateral isn't planar)
	double pv[3], p1[3], p2[3], po[3];
	v->ns->get_global_center(pv);
	a1->ns->get_global_center(p1);
	other->ns->get_global_center(po);
	a2->ns->get_global_center(p2);
	p1[0] = 0.5 * (p1[0] + p2[0]);	// p1 <- midpoint(p1,p2)
	p1[1] = 0.5 * (p1[1] + p2[1]);
	p1[2] = 0.5 * (p1[2] + p2[2]);
	double d1 = distance_sqr(pv, p1);	// distance v to midpoint
	double d2 = distance_sqr(pv, po);	// distance v to other
	return d1 < d2;
}

// Find best edge on this vertex to flip, and flip it.
// Want one with highest edge count on other side (which loses
// an edge in flip) and lowest edge counts on other side of 
// two adjacent edges (which gain an edge in the flip).
//
//       a1                a1
//     /   \             / | \
//   v ---- other  ==>  v  |  other
//     \   /             \ | /
//       a2                a2
//
void flip_edge(Vertex *v)
{
	int best = 0;		// index of best so far
	int score = -99;	// score of best so far (higher = better)
	Vertex *ba1 = NULL;	// adjacent vertices of best edge
	Vertex *ba2 = NULL;
	Vertex *other;

	for (int i=0; i<v->nedge; i++) {
		other = v->other(i);
		// find adjacent vertices
		Vertex *a1, *a2;
		if (find_adjacent(v, other, &a1, &a2) != 2)
			continue;	// can't flip this edge!
		if (have_edge(a1, a2))
			continue;	// already have the flipped version!
		if (!is_convex(v, a1, a2, other))
			continue;	// bad choice for flip
		// determine score (desirability) for this flip
		int s = other->nedge - a1->nedge - a2->nedge;
		// if at last level, change the logic...
		if (v->nedge == 7 && a1->nedge < 6 && a2->nedge < 6)
			s = 999;
		if (s > score) {
			best = i;
			score = s;
			ba1 = a1;
			ba2 = a2;
		}
	}

	if (ba1 == NULL) {
		fprintf(stderr, "No valid edge to flip!\n");
		dump_others(v);
		exit(1);
	}

	// remove edge from v1 & other
	Edge *fe = v->e[best];
	other = v->other(fe);
	v->remove_edge(best);
	other->remove_edge(fe);

	// change edge to go from a1 to a2
	fe->v1 = ba1;
	fe->v2 = ba2;

	// add edge to a1, a2
	ba1->add_edge(fe);
	ba2->add_edge(fe);

	// ** also fix sfcmodel triangles!!!
}

// Flip quad edges until no vertex has more than 6 edges
void fix_model(Vertex *vlist, int nvert)
{
	int cutoff = MAX_EDGE;

	for (;;) {
		// do one pass, flip edges for vertices with >= cutoff edges
		printf("Flip pass, cutoff = %d\n", cutoff);
		int nflip = 0;
		Vertex *v;
		int i;
		for (i=0, v=vlist; i<nvert; i++, v++) {
			if (v->nedge >= cutoff) {
				flip_edge(v);
				nflip++;
			}
		}
		printf("%d flips\n", nflip);
		if (nflip == 0) {	// none left, reduce cutoff
			if (--cutoff == 6)
				return;	// done!
		}
	}
}

int main()
{
	// load triangle mesh from stdin
	FILE_Dataport fp;
	fp.open(stdin);
	SfcModel sfc;
	sfc.parse_in(&fp);
	fp.close();
	printf("%d vertices loaded\n", sfc.mesh.pt_count);

	// build vertex/edge info for mesh fixer
	Vertex *vlist = new Vertex[sfc.mesh.pt_count];
	int i=0;
	for (NodeSpec_List_Element *nle=sfc.mesh.pt_list; nle; nle=nle->next) {
		// point vertex to node, and node back to vertex
		vlist[i].ns = nle->nodespec;
		vlist[i].ns->id = i;
		i++;
	}
	i = 0;
	for (Mesh_Triangle *tmp = sfc.mesh.tri_list; tmp; tmp=tmp->next) {
		i++;
		Vertex *t1 = &vlist[tmp->get_vertex_1()->id];
		Vertex *t2 = &vlist[tmp->get_vertex_2()->id];
		Vertex *t3 = &vlist[tmp->get_vertex_3()->id];
		new_edge(t1, t2);
		new_edge(t2, t3);
		new_edge(t3, t1);
	}
	printf("%d triangles loaded\n", i);

	// fix it
	fix_model(vlist, sfc.mesh.pt_count);

	// write it out to stdout
//	fp.open(stdout);
//	sfc.parse_out(&fp);
}
