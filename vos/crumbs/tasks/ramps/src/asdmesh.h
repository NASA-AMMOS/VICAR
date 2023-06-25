#ifndef _ASDMESH_H_
#define _ASDMESH_H_
// asdmesh.h 1.2 02/10/11 08:48:52
/** \file
* ASD surface mesh builder definitions
*/

#ifndef PI
#define PI 3.141592654
#endif

extern double ndist2(NodeSpec *n1, NodeSpec *n2);

class Edge;			// forward declarations
class Triangle;

Edge *edge_list;		///< head of active (cur LOD) edge list
Triangle *tri_list;		///< head of active (cur LOD) triangle list
int ntexture;			///< # texture maps (0 or # models in forest)

/// Vertex info (supplement to octree NodeSpecs).
// NodeSpec is linked back to Vertex using its ID field.
class Vertex {
public:
	int fid;		///< sequential output file vertex ID
	NodeSpec *r1, *r2;	///< reference/morph points at lower LOD
	float *u, *v;		///< allocated texture coords for each model
	int *afid;		///< allocated attribute file ID for each model

	/// constructor
	Vertex() {
		fid = -1;	// not written to file yet
		if (ntexture) {
			u = new float[ntexture];
			v = new float[ntexture];
			afid = new int[ntexture];	// (non-texgen case)
		}
	}
	
	~Vertex() {
		if (ntexture) {
			delete[] afid;
			delete[] v;
			delete[] u;
		}
	}
};

/// get vertex corresponding to a voxel nodespec (that's used in the mesh)
inline Vertex *nsv(NodeSpec *ns)
{
	return (Vertex *)(ns->id);
}

/// Mesh edge
class Edge {
public:
	Edge *next;		///< list linkage
	NodeSpec *p1, *p2;	///< edge vertices
	Edge *d1, *d2;		///< corresponding divided (split) edges
	float len;		///< squared edge length
	Triangle *t1, *t2;	///< faces using this edge (for adjacency)

	/// constructor
	Edge(NodeSpec *a, NodeSpec *b) : p1(a), p2(b) {
		len = ndist2(a, b);
		d1 = NULL;	// not split yet
		t1 = t2 = NULL;
		// add to front of active edge list
		next = edge_list;
		edge_list = this;
	}

	Edge(Edge *e) : p1(e->p1), p2(e->p2) {
		len = e->len;
		d1 = NULL;
		t1 = t2 = NULL;
		next = edge_list;
		edge_list = this;
	}

	void set_invalid()	{ p1 = NULL; }
	int  is_valid()		{ return p1 != NULL; }

	Vertex *v1()		///< vertices
		{ return nsv(p1); }
	Vertex *v2()
		{ return nsv(p2); }

	/// split edge at midpoint into 2 child edges
	void split(NodeSpec *mp) {
		d1 = new Edge(p1, mp);
		d2 = new Edge(mp, p2);
		// link new point back to edge
		nsv(mp)->r1 = p1;
		nsv(mp)->r2 = p2;
	}

	int is_split()		{ return d1 != NULL; }

	/// Convert edge pointer to (split point) reference vertex ID
	// return -1 if edge isn't split
	int ref_id() {
		return (d1) ? d1->v2()->fid : -1;
	}

	/// Store adjacency info for triangle using this edge
	// (each edge belongs to one or two triangles)
	void set_adj(Triangle *t)
	{
		if (t1)
			t2 = t;
		else
			t1 = t;
	}
};

/// mesh triangle
class Triangle {
public:
	Triangle *next;		///< mesh triangle list linkage
	Edge *e1, *e2, *e3;	///< triangle edges, clockwise order
	char rvs1, rvs2, rvs3;	///< edges reversed (p2->p1)?
	int fid;		///< output file ID

	/// children (subdivided triangles at next higher LOD)
	Triangle *k1, *k2, *k3, *k4;

	// parent triangle at previous LOD
	// ** Triangle *parent;

	/// constructor
	Triangle(Edge *a, int ra, Edge *b, int rb, Edge *c, int rc) :
			e1(a), e2(b), e3(c), rvs1(ra), rvs2(rb), rvs3(rc),
			k1(NULL), k2(NULL), k3(NULL), k4(NULL) {
		//e1->set_adj(this);		// link edges to me
		//e2->set_adj(this);
		//e3->set_adj(this);
		next = tri_list;		// store on list
		tri_list = this;
	}

	NodeSpec *p1() 		///< first point
		{ return rvs1 ? e1->p2 : e1->p1; }

	NodeSpec *p2() 		///< second point
		{ return rvs2 ? e2->p2 : e2->p1; }

	NodeSpec *p3() 		///< third point
		{ return rvs3 ? e3->p2 : e3->p1; }

	Vertex *v1()		///< vertex info
		{ return nsv(p1()); }
	Vertex *v2()
		{ return nsv(p2()); }
	Vertex *v3()
		{ return nsv(p3()); }

	void rotrvs() {		///< rotate edges 1->3, 2->1, 3->2
		Edge *te = e1;
		int tr = rvs1;
		e1 = e2; rvs1 = rvs2;
		e2 = e3; rvs2 = rvs3;
		e3 = te; rvs3 = tr;
	}

	void rotfwd() {		///< rotate edges 1->2, 2->3, 3->1
		Edge *te = e3;
		int tr = rvs3;
		e3 = e2; rvs3 = rvs2;
		e2 = e1; rvs2 = rvs1;
		e1 = te; rvs1 = tr;
	}

	int is_valid() {	///< points to valid edges?
		return e1->is_valid() && e2->is_valid() && e3->is_valid();
	}
};

/// component model data (one per forest tree) for texture
struct Model {	
	Image tex;	///< texture image
	int xres, yres;	///< dimensions of texture map
	double eye[3];	///< eye/camera position in base surface model coords
	/// map vertices from sfc 0 model coords to this model's object coords
	ZMatrix xform;	
	Summitt_range r; ///< limits in surface's model coords for texgen
};

#endif
