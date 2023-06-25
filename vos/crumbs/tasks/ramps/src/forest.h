#ifndef _FOREST_H_
#define _FOREST_H_
// forest.h 1.11 03/03/26 15:39:29
/** \file
 ** Forest class definition
 **/
#include "grape/object.h"
#include "patch.h"

// application object types - should be in a common file...
#define FOREST_V1 100

/// List of octrees (merged surface model)
/**
// A Forest is a GroupObj (list of ObjNodes), where the nodes are
// Patch objects (octree+mesh+ancillary data), representing a combined model. 
//
// By convention, the first model in the forest is considered the
// "base" model. Typically, this is a low-resolution model covering a
// wide area. (A forest may have additional base models, as indicated
// by the Patch base_model flags.)
//
// If the base model is *not* georeferenced, all models in the
// forest are matched via a common world coordinate frame (XYZ) -
// each model's ObjNode object-to-world transform registers that model
// to the common frame.
//
// If the base model *is* georeferenced, any other georeferenced
// models in the forest are matched via the geo frame (lat/long/el)
// and not the "world" transform frame. Non-georeferenced models in
// this forest are still matched to the base model via their world
// frames, and are therefore georeferenced using the base model's
// geo transform.
//
// (So far) no data items are added.
// A future goal is to provide forest-level search functions equivalent 
// to octree searches. Possibly the search should return a (list of)
// voxel pointers, along with the corresponding voxel coordinates in
// world (or georeferenced?) space.
// Doing this efficiently will require intelligent transform caching.
//
// Forest doesn't override GroupObj's parse_out() function, and
// uses the same file start token. So an application needs to know
// ahead of time that an input file is a forest rather than a
// generic GroupObj.
*/

class Forest : public GroupObj {

public:
	/// identify forest objects
	virtual int get_type(void)  { return(FOREST_V1); }

	/// load forest from file 
	// (same as base method but loads Patch nodes)
	int parse_in(Dataport *fp, int expand);
	int parse_in(Dataport *fp) { return parse_in(fp, TRUE); }

	/// get georeferencing for Nth model in the forest
	// (assumes forest *is* georeferenced)
	GeoData *get_geo_data(int n)
	{
		ObjNode *model = get_child(n);
		if (model && model->get_geo_data())
			return model->get_geo_data();
		return get_child(0)->get_geo_data();
	}

	/// convenience for accessing child as a patch
	Patch *get_patch(int i) {
		return (Patch *)get_child(i);
	}

	/// lookup child number by identifier, -1 if not found
	int find_patch(const char *ident);
};

#endif
