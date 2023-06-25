#ifndef _Octree_H_
#define _Octree_H_
// octree.h 1.38 03/10/30 20:54:05
/** \file
 ** Definitions for Octree recursive voxel handling
 ** Created by John Wright
 **/

#ifdef FLEXCOLOR
// #define FLEXCOLOR for variable-size voxels supporting hyperspectral data.
// Define NodeSpec as an alias so source code is compatible, but 
// linker can catch library incompatibility.
#define NodeSpec HSNodeSpec
#endif

#include "grape/color.h"

#define OCTREE_DIMS		3

/// Order of octree node children 
enum Octree_Child_Ref {
	UPPER_LEFT_BACK,		//+x +y +z
	UPPER_LEFT_FRONT,		//+x +y -z
	UPPER_RIGHT_BACK,		//+x -y +z
	UPPER_RIGHT_FRONT,		//+x -y -z
	LOWER_LEFT_BACK,		//-x +y +z
	LOWER_LEFT_FRONT,		//-x +y -z
	LOWER_RIGHT_BACK,		//-x -y +z
	LOWER_RIGHT_FRONT,		//-x -y -z
	NUMBER_OF_SPACE_PARTITIONS	// = 2^^OCTREE_DIMS
};

// Orig. un-ordered list
//const double X_Offset[NUMBER_OF_SPACE_PARTITIONS] = {1.0,-1.0, 1.0,-1.0, 1.0,-1.0, 1.0,-1.0};
//const double Y_Offset[NUMBER_OF_SPACE_PARTITIONS] = {1.0, 1.0,-1.0,-1.0, 1.0, 1.0,-1.0,-1.0};
//const double Z_Offset[NUMBER_OF_SPACE_PARTITIONS] = {1.0, 1.0, 1.0, 1.0,-1.0,-1.0,-1.0,-1.0};

const double X_Offset[NUMBER_OF_SPACE_PARTITIONS] = {1.0, 1.0, 1.0, 1.0,-1.0,-1.0,-1.0,-1.0};
const double Y_Offset[NUMBER_OF_SPACE_PARTITIONS] = {1.0, 1.0,-1.0,-1.0, 1.0, 1.0,-1.0,-1.0};
const double Z_Offset[NUMBER_OF_SPACE_PARTITIONS] = {1.0,-1.0, 1.0,-1.0, 1.0,-1.0, 1.0,-1.0};

/**
// Binary voxels appear in place of OCTREE_STUFF...END_OCTREE_STUFF
// in octree (or surface model) file, using the following format:
// 	OCTREE_BIN_STUFF
// 	binary_voxel_records
//	binary_end_record
// where the first binary record immediately follows a newline after
// the "OCTREE_BIN_STUFF" token.
// Each record contains:
//	record_length	- 1 byte, 0 indicates end record
//	flags		- 1 byte
//	data		- record_length bytes
//
// "flags" indicate the presence of optional fields in the record,
// 	depending on type
// "data" is binary data depending on the record type
**/
enum BinaryVoxelRecordFlags {
	BRVOXEL_FLAGS_RGB 	= 0x01,	// color present (3 bytes)
	BRVOXEL_FLAGS_WEIGHT	= 0x02,	// node weight present (1 float)
	BRVOXEL_FLAGS_NORMAL	= 0x04,	// surface normal present (3 floats)
	BRVOXEL_FLAGS_GRAY	= 0x08,	// grayscale present (1 byte)

	BRVOXEL_FLAGS_LENDIAN	= 0x80,	// explicitly little-endian
	BRVOXEL_FLAGS_BENDIAN	= 0xC0	// explicitly big-endian
};

struct BRHeader {			// record header
	uchar nbytes;			// data length
	uchar flags;
};

/// Type for one voxel in octree.
/**
// Leaf cells in the octree structure can contain multiple
// voxel nodes, kept in a singly-linked list.
**/
class NodeSpec {

    private:

    protected:

#ifdef FLEXCOLOR
	ColorSpec	*nodecolor;	///< flexible color
#else
	float	color[4];		///< RGBA color
#endif

	/// Voxel center in model space
	double		local_center[OCTREE_DIMS];
	double		global_center[OCTREE_DIMS];

	/// Surface normal vector
	double		normal[OCTREE_DIMS];

	/// Accumulation weighting
	double		weight;

    public:

	double		edge_length;	///< Size of voxel
	NodeSpec	*next;		///< List linkage
	int 		id;		///< Index for file I/O

#ifdef FLEXCOLOR
	// These methods may be used with any type of ColorSpec
	// from single banded to hyperspectral
	void	set_color(ColorSpec *nc) {
			delete(nodecolor);
			nodecolor = nc;
		}
	ColorSpec	*get_color(void) { return(nodecolor); }
	void use_alpha( int bFlag) { nodecolor->use_alpha(bFlag); }
#else
	void use_alpha(int flag) { }
#endif

	// These methods may be used together and will default the
	// ColorSpec to RGB.  These shouldn't be used with the above
	// methods unless the programmer is guaranteeing that the
	// ColorSpec being set is RGB
	void	set_color(double r, double g, double b, double a=1.0);
	int	get_color(double *r, double *g, double *b, double *a=NULL);

	void	set_color(int r, int g, int b, int a=255);
	int	get_color(int *r, int *g, int *b, int *a=NULL);


	double	get_weight(void) { return(weight); }
	void	set_weight(double w) { weight = w; }

	void	set_local_center(double *cnt) 
		{ memcpy(local_center, cnt, sizeof(local_center)); }
	void	get_local_center(double *cnt)
		{ memcpy(cnt, local_center, sizeof(local_center)); }

	/// Set voxel coordinates from array
	void	set_global_center(double *cnt)
		{ memcpy(global_center, cnt, sizeof(global_center)); }
	/// Get voxel coordinates to array
	void 	get_global_center(double *vec)
		{ memcpy(vec, global_center, sizeof(global_center)); }
	
	/// Set surface normal from array
	void	set_normal(double *cnt)
		{ memcpy(normal, cnt, sizeof(normal)); }
	/// Normal valid? (not all zeros)
	Boolean has_normal() 
		{ return normal[2]!=0.0 || normal[1]!=0.0 || normal[0]!=0.0; }
	Boolean get_normal(double *vec) {
			if (has_normal()) {
				memcpy(vec, normal, sizeof(normal));
				return TRUE;
			}
			return FALSE;
		}
			
	/// Make copy of node (ought to be a const function)
	NodeSpec * duplicate(NodeSpec *dup = NULL);

	virtual	int	parse_in(Dataport *fp);
	virtual	int	parse_out(Dataport *fp, int expand=FALSE);

	/// write/read node in binary format
	void write_binary(Dataport *fp);
	void read_binary(BRHeader *hdr, Dataport *fp);

    	/// Constructor
	NodeSpec() {
#ifdef FLEXCOLOR
		nodecolor = NULL;
#endif
		edge_length = 0.0;
		next = NULL;
		weight = 1.0;
		memset(normal, 0, sizeof(normal));
	}

	NodeSpec(NodeSpec &ns) {
		ns.duplicate(this);
	}

	NodeSpec& operator=(NodeSpec &ns) {
		if (this == &ns)
			return *this;
		ns.duplicate(this);
		return *this;
	}
	
    	/// Destructor
	~NodeSpec() {
#ifdef FLEXCOLOR
		delete(nodecolor);
#endif
		delete(next);
	}

};

/// Element for keeping lists of voxels
/**
// These are for extracted lists, independent of the list of voxels in
// a single octree cell.
**/
class NodeSpec_List_Element {

    private:

    protected:

    public:

	NodeSpec	*nodespec;
	NodeSpec_List_Element	*next;

	/// Delete all entries in a nodelist (including self)
	// Since list could be long, don't do it by recursion
	void cleanup() {
		NodeSpec_List_Element *nodelist, *next_node;
		for (nodelist=this; nodelist; nodelist = next_node) {
			next_node = nodelist->next;
			delete nodelist;
		}
	}

	/// Constructor
	NodeSpec_List_Element() : nodespec(NULL), next(NULL) { }
	NodeSpec_List_Element(NodeSpec *ns) :
		nodespec(ns), next(NULL) { }

	/// Destructor
	// Can't call cleanup() as this would call destructor
	~NodeSpec_List_Element() { }
};

/// The recursively linked octree cell
/**
// Each cell represents a region of space. A cell's children
// (if any) subdivide that space.
**/
class Octree_Data {

    private:

    protected:

	Octree_Data	*child[NUMBER_OF_SPACE_PARTITIONS];
	NodeSpec	*nodespec;	///< Head of voxel list
	long		level;

	int	in_volume(double outcoords[OCTREE_DIMS]) {
			int i;
			for(i=0; i<OCTREE_DIMS; i++) {
				if(outcoords[i] < -1.0 || outcoords[i] > 1.0) return(FALSE);
			}
			return(TRUE);
		}

	/** Averages/accumulates the various composite values that make up
	 ** a nodespec.  Used by accumulate_voxel
	 **/
	void accumulate_values(NodeSpec *, double = 1.0);

	/// Determines which method to call between add_data or accumulate_values.
	void add_or_acc(NodeSpec *, double * = NULL, double = 1.0);

	/// Add voxel (or list) to this node's list 
	void	add_data(NodeSpec *ns, double locn[OCTREE_DIMS] = NULL) {
		
			// Set location
			if(locn) 
				ns->set_local_center(locn);
	
			// Find end of input list, usually same as start
			NodeSpec *tail;
			for (tail=ns; tail->next; tail=tail->next)
				;

			// Add new list to front of node's list
			tail->next = nodespec;
			nodespec = ns;
		}

	void	init(void) {
			int i;
			for(i=UPPER_LEFT_BACK; i<NUMBER_OF_SPACE_PARTITIONS; i++) {
				child[i] = NULL;
			}
			nodespec = NULL;
			level = 0;
		}
    public:

	int		any_children(void);
	Octree_Data	*get_child(Octree_Child_Ref n) { return(child[n]); }
	NodeSpec	*get_node_data(void) { return(nodespec); }

	float		match_rgb(double *, double *, double *, double *, double * = NULL);

	int		get_rgb(double *, double *, double *, double *, double * = NULL);
	int		get_rgb(double *, int *, int *, int *, int * = NULL);
#ifdef FLEXCOLOR
	ColorSpec	*get_color(double *);
#endif

/******************************************************************************************
	The following functions are used to add data to the octree.  add_voxel adds a
data item which has a volume.  It is assumed to be a cube with a given edge length.
add_sample adds a sample data item which is assumed to be a point of zero volume.
Samples are forced down the tree far enough such that there is one and only one sample
at a given leaf node and none at any higher nodes.  Only nodes at level 0 may contain
more than one sample as they cannot be forced any further down.  Voxels, however, are
forced down until their volume is approximately equal to the node at that depth.  It si
quite possible to have multiple voxels at any node and to have voxels at multiple
levels within the tree (inherently multiresolution).  Thus, it is unwise to attempt to
mix sample and voxels in a single octree as incoming samples will force resident voxels
to be treated as samples and forced down the tree below the appropriate volume level.
Consider this your only warning.
******************************************************************************************/
	virtual long	add_voxel(NodeSpec *, double * = NULL, double = -1.0);	// returns level
	virtual long	add_sample(NodeSpec *, double * = NULL);		// returns level

	/// Accumulates/merges all voxels at their appropriate level
	virtual long	accumulate_voxel(NodeSpec *, double * = NULL, double = -1.0, double = 1.0);  // returns level
	/// Subdivdes all large voxels with accumulation being done at leaf nodes
	virtual long	accumulate_at_leaf(NodeSpec *, double * = NULL, double = -1.0);  // returns level

	void	accumulate_node();

	long    subdivide();

	/** force this node to this level setting all below,
	 ** optionally accumulate voxels from lower levels first
	 **/
	long	set_level(long lvl, int accumulate=FALSE);

	long	get_level(void) { return(level); }
	long	get_levels(void);	// return max depth of tree below here


        /// Condensing functions
	int	condense(int (* compare_func)() = NULL);   // Prune the octree based on children's equality 
	int	compare_color( NodeSpec *, NodeSpec *);  // Compare two NodeSpecs' colors 

	/// Search methods
	double	search_model_space(double in[OCTREE_DIMS], NodeSpec **matched_node);
	double  find_closest_nodespec(NodeSpec * node, double input_point[OCTREE_DIMS], double match_point[OCTREE_DIMS]);
	double  find_closest_nodespec(NodeSpec * node, double input_point[OCTREE_DIMS], NodeSpec **matched_node);
	double	fast_find_closest(long toplevel, double indsq, 
		double glob_in[OCTREE_DIMS], double loc_in[OCTREE_DIMS], 
		NodeSpec **closest_node);
	double	fast_find_closest(long toplevel, double indsq, 
		double glob_in[OCTREE_DIMS], double loc_in[OCTREE_DIMS], 
		double out[OCTREE_DIMS]);
	int	find_points_within_radius(long top_level, double global_pt[OCTREE_DIMS], 
		double local_pt[OCTREE_DIMS], double radius, NodeSpec_List_Element **pt_list);

	/// "release" the nodespec, e.g. set the nodespec value to null.
	void	release_nodespec() {
		nodespec = NULL;
	}

	/// Process the octree for weighted match
	double setup_weighted_match(float weight);

	/// Compare the children of this octree based on the given function.
	int	compare_child_list( int (* compare_func)() = NULL);

	/// Recursively discard any voxels outside specified volume
	virtual void clip(Range *volume, ZMatrix m2w);

	/// I/O functions
	void	ascii_dump(FILE *fs, long lvl);

	virtual	int	parse_in(Dataport *fp);
	virtual	int	parse_out(Dataport *fp, int expand=FALSE);

	/// Save voxels in binary format
	void write_binary(Dataport *fp);

    	/// Constructor
	Octree_Data(int lvl) { init(); level = lvl; }

    	/// Destructor
	~Octree_Data() {
		delete(nodespec);

		for(int i=UPPER_LEFT_BACK; i<NUMBER_OF_SPACE_PARTITIONS; i++)
			delete(child[i]);
	}

};

/// Top-level Octree class
/**
// This is a type of object, and points to the top-level octree_data cell.
// Services transform coordinates from object to model space and
// invoke recursive octree_data functions at the top-level cell.
//
// Created by John Wright
// Created Thu Mar 26 14:00:46 1998
**/

class Octree:public Obj {

    private:

    protected:

	Octree_Data	*octree;
	/// Maximum number of levels in the octree
	/**
	// Levels are numbered from 0 at the bottom to levels-1 at the top
	**/
	long		levels;

	void		init(void) {
				octree = NULL;
				levels = 0;
			}

    public:

	Octree_Data 	*init_data();
	Octree_Data	*get_data() { return( octree ); }
	virtual int	get_type(void) { return( OCTREE_V1 ); }
	// add virtual methods for parsing in selected object types
	// virtual void	scan_convert(...);

	int	XformToOctree(double *incoords, double *outcoords) {
			ZMatrix xform;
			GetObjectToModelTransform(xform);
			MultPoints(incoords, xform, outcoords);
			return(TRUE);
		}
	void	OctreeLimits(double *mins, double *maxs);

	void	ascii_dump(FILE *fs) {
			if(octree) octree->ascii_dump(fs, 0);
			else fprintf(fs, "No octree to dump\n");
		}

	virtual void	add_voxel(NodeSpec *);
	virtual void	add_sample(NodeSpec *);

        virtual void    accumulate_voxel(NodeSpec *, double = 1.0);

#ifdef FLEXCOLOR
	virtual ColorSpec	*get_color(double *xyx);
	virtual ColorSpec	*get_color(double _x, double _y=0.0, double _z=0.0) {
					double xyz[OCTREE_DIMS] = { 0.0 };
					xyz[0] = _x; xyz[1] = _y; xyz[2] = _z;
					return(get_color(xyz));
				}
#endif
	virtual int	get_rgb(double *xyz, int *r, int *g, int *b, int *a=NULL);
	virtual int	get_rgb(double *xyz, double *r, double *g, double *b, double *a=NULL);
	virtual int	get_rgb(double _x, double _y, double _z, int *r, int *g, int *b, int *a=NULL) {
					double xyz[OCTREE_DIMS] = { 0.0 };
					xyz[0] = _x; xyz[1] = _y; xyz[2] = _z;
					return(get_rgb(xyz, r, g, b, a));
				}

	/// Set the max number of levels in the tree.
	/**
	// The level of the top node is set to levels-1
	*/
	long	set_max_levels(long lvl, int accumulate=FALSE) {
			levels = lvl;
			if(octree) octree->set_level(lvl-1, accumulate);
			return(get_levels());
		}
	long	get_max_levels(void) { return(levels); }

	/// Return current depth of the tree
	/** 
	// The number of levels in the tree which contain nodes
	*/
	long	get_levels(void) { 
			if(octree)return(octree->get_levels());
			return(0);
		}

	/// Return number of voxels (only valid right after parse_in)
	int	get_node_count();

	/// Discard voxels outside specified volume in world frame
	void clip(Range *volume, ZMatrix m2w) {
			if (octree) octree->clip(volume, m2w); 
		}

	/// Load from file
	virtual	int	parse_in(Dataport *fp);

	/// Save to file
	virtual	int	parse_out(Dataport *fp, int expand=FALSE);

	int	condense( int (*comp_func)() = NULL);

	double	setup_weighted_match( float weight );

	/// Search methods
	double	search_object_space(double in[], double out[]);
	double	fast_find_closest(double in[], double out[]);
	int	find_points_within_radius(double input_pt[], double radius, NodeSpec **pt_list);

	/// Constructors
        Octree() { init(); }
        Octree(long lvl) { init(); set_max_levels(lvl); }

    	/// Destructor
        ~Octree() { delete(octree); }
};

#endif
