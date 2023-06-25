#ifndef _SFCMODEL_H_
#define _SFCMODEL_H_
// sfcmodel.h 1.8 02/07/26 15:26:23
/** \file
 ** Define surface model class
 **/

#include "grape/octree.h"
#include "grape/tmesh.h"

/// A SfcModel is an octree with a (possibly empty) triangle mesh
class SfcModel : public Octree {

public:
	Triangle_Model mesh;

	/// Identify object type
	int get_type(void)  { return(SFC_MODEL_V1); }
	
	/// Build/update mesh from octree nodes (NOT IMPLEMENTED)
	int update_mesh();
	
	/// Recursively discard any voxels outside specified volume
	// ** triangles are not clipped!
	virtual void clip(Range *volume, ZMatrix m2w) {
		Octree::clip(volume, m2w);
		mesh.clean_up_pt_list();	// point list not valid
	}

	/// Load from file
	int parse_in(Dataport *fp) {
		char token[4096];
		do {	// skip optional SFC_MODEL_V1 and get OCTREE_V1
			get_next_token(fp, token);
		} while (!strcmp(token, "SFC_MODEL_V1"));
		if (!Octree::parse_in(fp))
			return FALSE;
		return mesh.parse_in(fp, (Octree *)this);
	}

	/// Save to file
	int parse_out(Dataport *fp, int expand=FALSE) {
		put_token(fp, "SFC_MODEL_V1");
		if (!Octree::parse_out(fp, expand))
			return FALSE;
		return mesh.parse_out(fp);
	}
	
	/// Setup mesh point list from octree
	void add_pt_list() { mesh.add_pt_list((Octree *)this); }

	/// Default constructor
	SfcModel() {}
	
	/// Constructor setting octree depth
	SfcModel(long nlevels) { set_max_levels(nlevels); }
};

#endif
