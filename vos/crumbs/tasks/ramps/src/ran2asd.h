// ran2asd.h 1.2 02/11/05 08:37:19
/** \file
 ** Range map-to-ASD surface mesh builder definitions
 **/

#ifndef MAX
#define MAX(a,b)	((a)>(b) ? (a) : (b))
#define MIN(a,b)	((a)<(b) ? (a) : (b))
#endif

class Edge;			// forward declarations
class Triangle;
class Vertex;
Edge *edge_list;		// head of active (cur LOD) edge list
Triangle *tri_list;		// head of active (cur LOD) triangle list
static int get_range(double *p, int x, int y);
static void range_normal(int x, int y, double *norm);
double vdist2(Vertex *v1, Vertex *v2);

// flags for each range map cell: 1=invalid, 2=valid/in use
class CMap {
	char *map;
	int  xres;

public:
	CMap(int nx, int ny) : xres(nx) {
		map = new char [nx*ny];
		memset(map, 0, nx*ny);
	}
	
	~CMap() { delete[] map; }
	
	// mark cell 
	void set_invalid(int ix, int iy) {
		map[iy*xres + ix] = 1;
	}

	void set_use(int ix, int iy) {
		map[iy*xres + ix] = 2;
	}
	
	// is range map point valid?
	int is_valid(int ix, int iy) {
		return map[iy*xres + ix] != 1;
	}

	// is range map point in use in mesh (and valid)?
	int in_use(int ix, int iy) {
		return map[iy*xres + ix] == 2;
	}
};

static CMap *cmap;

// mesh vertex info
class Vertex {
public:
	int ix, iy;		// range map image coordinates
	int fid;		// sequential output file vertex ID
	Vertex *r1, *r2;	// reference/morph edge points at coarser LOD
	double norm[3];		// unit surface normal vector

	// constructor - initialize, update map, compute normal
	Vertex(int x, int y) : ix(x), iy(y), fid(-1) {
		cmap->set_use(x, y);
		range_normal(x, y, norm);
	}

	// get vertex XYZ coordinates
	void get_xyz(double *p) {
		get_range(p, ix, iy);
	}
};

// Mesh edge
class Edge {
public:
	Edge *next;		// list linkage
	Vertex *p1, *p2;	// edge vertices
	Edge *d1, *d2;		// corresponding divided (split) edges
//	float len;		// squared edge length
	Triangle *t1, *t2;	// faces using this edge (for adjacency)

	// constructor
	Edge(Vertex *a, Vertex *b) : p1(a), p2(b) {
//		len = vdist2(a, b);
		d1 = NULL;	// not split yet
		t1 = t2 = NULL;
		// add to front of active edge list
		next = edge_list;
		edge_list = this;
	}

	Edge(Edge *e) : p1(e->p1), p2(e->p2) {
//		len = e->len;
		d1 = NULL;
		t1 = t2 = NULL;
		next = edge_list;
		edge_list = this;
	}

	void set_invalid()	{ p1 = NULL; }
	int  is_valid()		{ return p1 != NULL; }

	// split edge at midpoint into 2 child edges
	void split(Vertex *mp) {
		d1 = new Edge(p1, mp);
		d2 = new Edge(mp, p2);
		// link midpoint back to edge
		mp->r1 = p1;
		mp->r2 = p2;
	}

	int is_split()		{ return d1 != NULL; }

	// Convert edge pointer to (split point) reference vertex ID,
	// or -1 if edge isn't split
	int ref_id() {
		return (d1) ? d1->p1->fid : -1;
	}

	// store adjacency info for triangle using this edge
	// (each edge belongs to one or two triangles)
	void set_adj(Triangle *t)
	{
		if (t1)
			t2 = t;
		else
			t1 = t;
	}
};

class Triangle {	// mesh triangle
public:
	Triangle *next;		// list linkage
	Edge *e1, *e2, *e3;	// triangle edges, clockwise order
	char rvs1, rvs2, rvs3;	// edges reversed (p2->p1)?
	int fid;		// output file ID

	// children (subdivided triangles at next higher LOD)
	Triangle *k1, *k2, *k3, *k4;

	// parent triangle at previous LOD
	// ** Triangle *parent; ** not needed?

	// constructor
	Triangle(Edge *a, int ra, Edge *b, int rb, Edge *c, int rc) :
			e1(a), e2(b), e3(c), rvs1(ra), rvs2(rb), rvs3(rc),
			k1(NULL), k2(NULL), k3(NULL), k4(NULL) {
		e1->set_adj(this);		// link edges to me
		e2->set_adj(this);
		e3->set_adj(this);
		next = tri_list;		// store on list
		tri_list = this;
	}

	Vertex *p1() 		// first point
		{ return rvs1 ? e1->p2 : e1->p1; }

	Vertex *p2() 		// second point
		{ return rvs2 ? e2->p2 : e2->p1; }

	Vertex *p3() 		// third point
		{ return rvs3 ? e3->p2 : e3->p1; }

	void rotrvs() {		// rotate edges 1->3, 2->1, 3->2
		Edge *te = e1;
		int tr = rvs1;
		e1 = e2; rvs1 = rvs2;
		e2 = e3; rvs2 = rvs3;
		e3 = te; rvs3 = tr;
	}

	void rotfwd() {		// rotate edges 1->2, 2->3, 3->1
		Edge *te = e3;
		int tr = rvs3;
		e3 = e2; rvs3 = rvs2;
		e2 = e1; rvs2 = rvs1;
		e1 = te; rvs1 = tr;
	}

	int is_valid() {	// points to valid edges?
		return e1->is_valid() && e2->is_valid() && e3->is_valid();
	}
};
