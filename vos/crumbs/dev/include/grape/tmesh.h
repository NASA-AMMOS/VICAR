#ifndef _TMESH_H_
#define _TMESH_H_
// tmesh.h 1.18 03/05/01 07:48:15
/** \file
 ** The Triangle_Model class, and Mesh_Triangle component class,
 ** define a triangle mesh for grape models.
 ** The derived MT_Mesh class, and Mesh_Edge and Mesh_Edge_List 
 ** helper classes, define additional objects used in the 
 ** Marching Triangles mesh generation algorithm.
 **/

#include "grape/octree.h"
#include "grape/vector_ops.h"

const int TM_HASH_SIZE = 1024;		// must be integer power of two

/// Basic triangle face for mesh.
/**
// Created by John Wright
// Created Sun Apr 16 08:27:30 2000
*/
class Mesh_Triangle {

    protected:
	NodeSpec	*vertex1, *vertex2, *vertex3;

    public:
	Mesh_Triangle	*next;		///< List linkage

	NodeSpec	*get_vertex_1(void) { return(vertex1); }
	NodeSpec	*get_vertex_2(void) { return(vertex2); }
	NodeSpec	*get_vertex_3(void) { return(vertex3); }

	/// Compute triangle's surface normal vector 
	void tri_surface_normal(double *out_norm) {
		double	v1[3], v2[3], v3[3];
		vertex1->get_global_center(v1);
		vertex2->get_global_center(v2);
		vertex3->get_global_center(v3);
		surface_normal(v1, v2, v3, out_norm);
	}
		
	/// Debug output
	void print_block_triangle(FILE *fp) {
		double gc[3];
		vertex1->get_global_center(gc);
		fprintf(fp,"v %14.7f %14.7f %14.7f\n", gc[0], gc[1], gc[2]);
		vertex2->get_global_center(gc);
		fprintf(fp,"v %14.7f %14.7f %14.7f\n", gc[0], gc[1], gc[2]);
		vertex3->get_global_center(gc);
		fprintf(fp,"v %14.7f %14.7f %14.7f\n", gc[0], gc[1], gc[2]);
		fprintf(fp,"f -1 -2 -3\n\n");
	}

	/// Output one triangle during full mesh output
	// assuming vertex node ID's have been setup
	void print_face(FILE *fp) {
		fprintf(fp,"f %d %d %d\n", 
			vertex1->id, vertex2->id, vertex3->id);
	}

	/// Reset vertex pointers. Needed to reorder for triangle strips
	void reset(NodeSpec *ns1, NodeSpec *ns2, NodeSpec *ns3) {
		vertex1 = ns1;
		vertex2 = ns2;
		vertex3 = ns3;
	}

    	/// Constructor
	Mesh_Triangle(NodeSpec *ns1, NodeSpec *ns2, NodeSpec *ns3) {
		reset(ns1, ns2, ns3);
		next = NULL;
	}
};

/// Describes a triangle mesh for a 3D surface model.
/**
// Created by John Wright
// Created Sun Apr 16 11:19:49 2000
*/
class Triangle_Model {

    private:
	// swap 4-byte values from big-endian to little or vice-versa
	static void swap4(void *p, int n)
	{
		char *cp = (char *)p;
		while (--n >= 0) {
			char t = cp[0];
			cp[0] = cp[3];
			cp[3] = t;
			t = cp[1];
			cp[1] = cp[2];
			cp[2] = t;
			cp += 4;
		}
	}

    public:
	Mesh_Triangle	*tri_list;		///< Mesh's triangles
	NodeSpec_List_Element *pt_list;		///< Vertex list
	int 		pt_count;		///< Number of points in list

	/// Debug info
	int tri_count() {
		int n=0;
		for (Mesh_Triangle *tmp=tri_list; tmp; tmp=tmp->next)
			n++;
		return n;
	}

	/// Add triangle to head of list
	void	add_triangle(Mesh_Triangle *tri) {
		tri->next = tri_list;
		tri_list = tri;
	}

	/// Delete all vertices
	void	clean_up_pt_list() {
		if (pt_list)
			pt_list->cleanup();
		pt_list = NULL;
		pt_count = 0;
	}

	/// Add points from octree to mesh's vertex list
	void	add_pt_list(Octree *oct) {
		double tctr[] = { 0, 0, 0 };
		clean_up_pt_list();	// don't leak if something there
		pt_count = oct->get_data()->find_points_within_radius(
					oct->get_max_levels(), 
					tctr, tctr, 1.0E6, &pt_list);
	}

	/// Delete all points and triangles
	void	clean_up() {
		clean_up_pt_list();
		while (tri_list) {
			Mesh_Triangle *tmp = tri_list;
			tri_list = tri_list->next;
			delete tmp;
		}
	}

	/// Debug output to open file
	void	print_blocked_triangles(FILE *fp, char *source_name=NULL) {
		if(source_name) 
			fprintf(fp,"# mesh created from %s\n", source_name);
		for (Mesh_Triangle *tmp = tri_list; tmp; tmp = tmp->next)
			tmp->print_block_triangle(fp);
	}

	/// Debug output to named file
	void	print_blocked_file(char *fname, char *source_name=NULL) {
		FILE *fp = fopen(fname,"w");
		if(fp) {
			print_blocked_triangles(fp, source_name);
			fclose(fp);
		} else {
			fprintf(stderr,"print_blocked_file unable to open %s\n",fname);
		}
	}
		
	/// Write mesh to open file as Wavefront .obj file
	void	print_obj_file(FILE *fp, char *source_name=NULL) {
		if(source_name) 
			fprintf(fp,"# Mesh created from %s\n", source_name);

		// print vertices while setting IDs (.obj ID starts at 1)
		int node_id = 0;
		for (NodeSpec_List_Element *nle=pt_list; nle; nle=nle->next) {
			double gc[3];
			nle->nodespec->id = ++node_id;
			nle->nodespec->get_global_center(gc);
			fprintf(fp,"v %14.7f %14.7f %14.7f\n", 
				gc[0], gc[1], gc[2]);
		}

		// print triangle faces indexing vertex nodes
		for (Mesh_Triangle *tmp = tri_list; tmp; tmp=tmp->next) {
			tmp->print_face(fp);
		}
	}

	/// Write mesh to named file
	void	print_obj_file(char *fname, char *source_name=NULL) {
		FILE *fp = fopen(fname,"w");
		if(fp) {
			print_obj_file(fp, source_name);
			fclose(fp);
		} else {
			fprintf(stderr,"print_blocked_file unable to open %s\n",fname);
		}
	}

	/// Load mesh from input file; assumes octree was just read in.
	/**
	// Returns TRUE if input contained mesh info
	*/
	int parse_in(Dataport *fp, Octree *oct) {
		char token[4096];

		// get list of octree nodes
		add_pt_list(oct);

		// build temporary mapping of node ID to NodeSpec pointer
		NodeSpec **ptref = new NodeSpec *[pt_count];
		NodeSpec_List_Element *tmp = pt_list;
		while (tmp) {
			int i = tmp->nodespec->id;
			if (i < pt_count)
				ptref[i] = tmp->nodespec;
			tmp = tmp->next;
		}

		// load faces
		int status = FALSE;
		for (;;) {
			if (!get_next_token(fp, token)) {
				if (tri_list)	// added at least one triangle
					fprintf(stderr, " Whoops - Unexpected EOF in octree mesh\n");
				break;
			}
			if (!strcmp(token, "FACE")) {
				get_next_token(fp, token);
				int fid1 = atoi(token);
				get_next_token(fp, token);
				int fid2 = atoi(token);
				get_next_token(fp, token);
				int fid3 = atoi(token);
				if (fid1 < pt_count && fid2 < pt_count &&
						fid3 < pt_count)
					add_triangle(new Mesh_Triangle(
						ptref[fid1], ptref[fid2], 
						ptref[fid3]));
			} else if (!strcmp(token, "MESH_BIN_V1")) {
				// read binary face data (big-endian)
				long fid[3];
				while (fp->read(fid, 3*sizeof(long)) > 0) {
#if !INT_BIGENDIAN
					swap4(fid, 3);
#endif
					if (fid[0] < 0)	// end record
						break;
					add_triangle(new Mesh_Triangle(
						ptref[fid[0]], ptref[fid[1]],
						ptref[fid[2]]));
				}
				status = TRUE;	// okay, done
				break;
			} else if (!strcmp(token, "MESH_LBIN_V1")) {
				// read binary face data (little-endian)
				long fid[3];
				while (fp->read(fid, 3*sizeof(long)) > 0) {
#if INT_BIGENDIAN
					swap4(fid, 3);
#endif
					if (fid[0] < 0)	// end record
						break;
					add_triangle(new Mesh_Triangle(
						ptref[fid[0]], ptref[fid[1]],
						ptref[fid[2]]));
				}
				status = TRUE;	// okay, done
				break;
			} else if (!strcmp(token, "MESH_V1")) {
				;		// ignore start token
			} else if (!strcmp(token, "END_MESH")) {
				status = TRUE;	// okay
				break;
			} else if (pt_count == 0) {
				// unexpected token, but if no points, figure
				// it's octree data that was skipped due to
				// bounding volume check
				status = TRUE;
				break;
			} else {
				fprintf(stderr, " Whoops - Unexpected token >%s< encountered in parsing octree mesh\n",
					token);
				break;
			}
		}
		delete[] ptref;
		return status;
	}

	/// Write mesh data (triangles)
	/**
	// Assumes octree just written out,
	// with node IDs set to match output order
	*/
	int parse_out(Dataport *fp) {
		char token[128];

		Mesh_Triangle *tmp = tri_list;
		if (getenv("GRAPE_WRITE_TEXT")) {	// write text format
			put_token(fp, "\nMESH_V1");
			while (tmp) {
				sprintf(token, "\nFACE %d %d %d",
					tmp->get_vertex_1()->id, 
					tmp->get_vertex_2()->id, 
					tmp->get_vertex_3()->id);
				put_token(fp, token);
				tmp = tmp->next;
			}
			put_token(fp, "END_MESH\n");
		} else {				// write binary format
			long vertex[3];
#if INT_BIGENDIAN
			put_token(fp, "\nMESH_BIN_V1");
#else
			put_token(fp, "\nMESH_LBIN_V1");
#endif
			while (tmp) {
				vertex[0] = tmp->get_vertex_1()->id;
				vertex[1] = tmp->get_vertex_2()->id;
				vertex[2] = tmp->get_vertex_3()->id;
				fp->write(vertex, 3*sizeof(long));
				tmp = tmp->next;
			}
			// end record
			vertex[0] = -1;
			fp->write(vertex, 3*sizeof(long));
		}
		return TRUE;
	}

    	/// Constructor
	Triangle_Model() : tri_list(NULL), pt_list(NULL), pt_count(0) { }

    	/// Destructor
	~Triangle_Model() { 
		// cleanup is disabled, as it just slows down termination
		// (can always call clean_up() manually...)
		//clean_up(); 
	}
};

/// Mesh polygon edge
/**
// Created by John Wright
// Created Sun Apr 16 08:27:04 2000
//
// Mesh_Edge represents a single edge of a polygon.  It includes
// a third point to establish the plane and inside (assumes convex).
// Used in Marching Triangles meshing algorithm.
**/
class Mesh_Edge {

    protected:
	NodeSpec *end1;		///< Start vertex
	NodeSpec *end2;		///< End vertex
	NodeSpec *other;	///< Opposite vertex

    public:
	Mesh_Edge	*next;	///< List linkage

	NodeSpec *get_end_1(void) { return(end1); }
	NodeSpec *get_end_2(void) { return(end2); }
	NodeSpec *get_other(void) { return(other); }
	void	get_end_1(double *P) { get_end_1()->get_global_center(P); }
	void	get_end_2(double *P) { get_end_2()->get_global_center(P); }
	void	get_other(double *P) { get_other()->get_global_center(P); }

	///< Compute hash table index
	long 	hash_value() {
			return ((long(end1) ^ long(end2)) >> 3) & 
				(TM_HASH_SIZE-1);
		}

    	/// Constructor
	Mesh_Edge(NodeSpec *ns1, NodeSpec *ns2, NodeSpec *ns3) {
		end1 = ns1;
		end2 = ns2;
		other = ns3;
		next = NULL;
	}
};

/// Singly-linked list of edges.
/**
// Created by John Wright
// Created Sun Apr 16 08:28:16 2000
//
// Used in Marching Triangles meshing algorithm.
**/
class Mesh_Edge_List {

    protected:
	Mesh_Edge	*head;		///< First edge of list

    public:
	/// Delete list
	void	clean_up_list(void) {	
		Mesh_Edge	*tmp;
		while(head) {
			tmp = head;
			head = head->next;
			delete(tmp);
		}
	}

	/// Count number of edges contained in list
	int	number_of_edges(void) {
		int	count=0;
		Mesh_Edge	*tmp = head;
		while(tmp) {
			count++;
			tmp = tmp->next;
		}
		return(count);
	}

	/// Add edge to (head of) list
	void	add_edge(Mesh_Edge *ns) {
		ns->next = head;
		head = ns;
	}

	/// Return first edge of list, after removing it
	Mesh_Edge *remove_first_edge() {
		Mesh_Edge *me = head;		// get first edge
		if (head)			// take off list
			head = head->next;
		return me;
	}

	/// Search list for equivalent edge, return pointer or NULL
	Mesh_Edge *find(NodeSpec *ns1, NodeSpec *ns2) {
		register Mesh_Edge *tmp = head;
		while (tmp) {
			if ((ns1 == tmp->get_end_1() && ns2 == tmp->get_end_2()) ||
			     (ns1 == tmp->get_end_2() && ns2 == tmp->get_end_1()))
				return tmp;
			tmp = tmp->next;
		}
		return NULL;
	}

	Mesh_Edge *find(Mesh_Edge *me) {
		return find(me->get_end_1(), me->get_end_2());
	}

	/// Return TRUE if an equivalent edge is in the list
	int	is_in_list(Mesh_Edge *me) {
		return find(me) != NULL;
	}

	int	is_in_list(NodeSpec *ns1, NodeSpec *ns2) {
		return find(ns1, ns2) != NULL;
	}

	/// Search list for edge; if found, remove it from list.
	// Return pointer to removed edge, or NULL if not there.
	Mesh_Edge *find_and_remove(Mesh_Edge *me) {
		NodeSpec *ns1 = me->get_end_1();
		NodeSpec *ns2 = me->get_end_2();
		Mesh_Edge *prev = NULL;
		for (Mesh_Edge *tmp = head; tmp; tmp = tmp->next) {
			if ((ns1 == tmp->get_end_1() && ns2 == tmp->get_end_2()) ||
			     (ns1 == tmp->get_end_2() && ns2 == tmp->get_end_1())) {
				// found it, remove from this list
				if (prev)	// not first one
					prev->next = tmp->next;
				else		// removing first one
					head = tmp->next;
				return tmp;
			}
			prev = tmp;
		}
		return NULL;
	}

    	/// Constructor
	Mesh_Edge_List() {
		head = NULL;
	}

    	/// Destructor
	~Mesh_Edge_List() {
		clean_up_list();
	}

};

/// Extend Triangle_Model for Marching Triangles mesh algorithm
/**
// Extends the basic Triangle_Model with data structures and methods 
// for building the triangle mesh using the Marching Triangles algorithm.
//
// To speed up edge searching, hash values for each edge are
// computed to select one of N edge lists.
// A possible performance improvement would be to instead give 
// each vertex a list of edges starting from that vertex; 
// looking for an edge between A and B would just requiring 
// checking A's and B's list.
**/
class MT_Mesh : public Triangle_Model {

    protected:
	/// Edges used only once (candidates for growing)
	Mesh_Edge_List new_edges[TM_HASH_SIZE];

	/// Edges used once (failed extension, can be used once other way)
	Mesh_Edge_List once_edges[TM_HASH_SIZE];

	/// Edges used twice already (can't be used again)
	Mesh_Edge_List old_edges[TM_HASH_SIZE];

    public:
	/// Debug state dump
	void	stats() {
		fprintf(stderr, "%d points, %d triangles\n",
			pt_count, tri_count());
		int n1 = 0, n2 = 0, n3 = 0;
		for (int i=0; i<TM_HASH_SIZE; i++) {
			n1 += new_edges[i].number_of_edges();
			n2 += old_edges[i].number_of_edges();
			n3 += once_edges[i].number_of_edges();
		}
		fprintf(stderr, "edges: %d new, %d used 1x, %d used 2x\n", 
			n1, n3, n2);
	}

	/// Is equivalent edge already used twice?
	int is_old_edge(Mesh_Edge *me) {
		return old_edges[me->hash_value()].is_in_list(me);
	}

	/// Is edge of these vertices already used twice?
	int is_old_edge(NodeSpec *ns1, NodeSpec *ns2) {
		long hash_value = ((long(ns1) ^ long(ns2)) >> 3) & 
							(TM_HASH_SIZE-1);
		return old_edges[hash_value].is_in_list(ns1, ns2);
	}

	// is this edge already used the same direction in the mesh?
	int is_dup_edge(NodeSpec *ns1, NodeSpec *ns2) {
		long hash_value = ((long(ns1) ^ long(ns2)) >> 3) & 
							(TM_HASH_SIZE-1);
		Mesh_Edge *me = once_edges[hash_value].find(ns1, ns2);
		if (me && me->get_end_1() == ns1)
			return TRUE;
		me = old_edges[hash_value].find(ns1, ns2);
		if (me && me->get_end_1() == ns1)
			return TRUE;
		return FALSE;
	}

	/// Store edge to appropriate old/hash list
	void add_old_edge(Mesh_Edge *me) {
		me->next = NULL;
		old_edges[me->hash_value()].add_edge(me);
	}

	/// Keep track of "new" edge that failed extension.
	/**
	// Can't put it back into "new" edge list, or we'll 
	// get stuck forever trying to extend it. Putting it
	// onto the "old" edge list leaves lots of holes.
	*/
	void save_unextended_edge(Mesh_Edge *me) {
		me->next = NULL;
		once_edges[me->hash_value()].add_edge(me);
	}

	/// Store edge in new list if doesn't match one already there.
	/**
	// If it does, move the edge from new list to old list.
	// (Shouldn't be the same order!)
	*/
	int add_edge_if_unique(Mesh_Edge *me) {
		int hash = me->hash_value();
		Mesh_Edge *old = new_edges[hash].find_and_remove(me);
		if (old) {	// was there, move it to "old edges" list
#if 1			// temp sanity check
			if (me->get_end_1() == old->get_end_1()) {
				fprintf(stderr, "Repeat edge use!\n");
				abort();
			}
#endif
			old->next = NULL;
			old_edges[hash].add_edge(old);
			delete me;
			return FALSE;	// already there
		}
		new_edges[hash].add_edge(me);	// new, add to new list
		return TRUE;
	}

	/// Adding valid triangle, update edge lists.
	// For the two new edges, either add to new list,
	// or move existing edge (used once the other way) to old list.
	void add_edge_pair(Mesh_Edge *me1, Mesh_Edge *me2) {
		Mesh_Edge *old;
		int hash = me1->hash_value();
		if ((old = new_edges[hash].find_and_remove(me1))) {
			// first edge already in new list
			old->next = NULL;
			old_edges[hash].add_edge(old); // move to old list
			delete me1;
		} else if ((old = once_edges[hash].find_and_remove(me1))) {
			// first edge already in "once" list
			old->next = NULL;
			old_edges[hash].add_edge(old); // move to old list
			delete me1;
		} else {
			new_edges[hash].add_edge(me1);
		}
			
		hash = me2->hash_value();
		if ((old = new_edges[hash].find_and_remove(me2))) {
			// second edge already in new list
			old->next = NULL;
			old_edges[hash].add_edge(old); // move to old list
			delete me2;
		} else if ((old = once_edges[hash].find_and_remove(me2))) {
			// second edge already in "once" list
			old->next = NULL;
			old_edges[hash].add_edge(old); // move to old list
			delete me2;
		} else {
			new_edges[hash].add_edge(me2);
		}
	}

	/// Get a new edge not yet extended
	Mesh_Edge *next_new_edge() {
		static int hash;
		for (int i=0; i<TM_HASH_SIZE; i++) {
			Mesh_Edge *me = new_edges[hash].remove_first_edge();
			if (me)
				return me;
			hash = (hash+1) & (TM_HASH_SIZE-1);
		}
		return NULL;
	}

	/// Delete contents of edge lists
	void	clean_up_edges() {
		for (int i=0; i<TM_HASH_SIZE; i++) {
			old_edges[i].clean_up_list();
			new_edges[i].clean_up_list();
			once_edges[i].clean_up_list();
		}
	}

	/// Clean up complete mesh data structure
	void	clean_up() {
		clean_up_edges();
		Triangle_Model::clean_up();
	}

    	/// Destructor
	~MT_Mesh() { 
		// cleanup is disabled, as it just slows down termination
		// (can always call clean_up() manually...)
		//clean_up(); 
	}
};

#endif
