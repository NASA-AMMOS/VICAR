// mmerge.C 1.1 02/06/14 13:05:02
/** \file
** Function to merge octree forest destination for SUMMITT matcher
*/

#include "summitt_func.h"

#ifdef MATRIXDEBUG
#define DBGMATDUMP(fp,msg,mat)	MatDump(fp,msg,mat)
#else
#define DBGMATDUMP(fp,msg,mat)
#endif

// Is this forest entry "child" an active match destination?
static Boolean use_dest(int child, int src, int dst)
{
	if (dst == MATCH_PREV) 		// only use preceding models
		return (child < src);
	if (dst == MATCH_ALL)		// use all other models
		return (child != src);
	// else use only specific model
	return (child == dst);
}

// Recursive scan of destination octree. 
// For each input point, transform to world space using m2w, and 
// test against clip limits. If passes, transform to output
// model space using w2m, and add to output octree.
// ** Note: voxels aren't duplicated, so this leaves the input tree 
// with links but no voxels.
static void clip_and_merge(Octree_Data *od, Summitt_range *clip, ZMatrix m2w,
	ZMatrix w2m, Octree_Data *out)
{
	// do voxels at this level
	NodeSpec *nextptr;
	for (NodeSpec *ns = od->get_node_data(); ns; ns = nextptr) {
		nextptr = ns->next;
		ns->next = NULL;

		// ** need to mess with alpha stuff?

		// get model coords, transform to world space
		double center[3], center_trans[3];
		ns->get_global_center(center);
		MultPoints(center, m2w, center_trans);

		// check against clip limits
		if (!clip->in_range(center_trans)) {
			delete ns;	// don't leak it
			continue;
		}

		// transform to output model space
		MultPoints(center_trans, w2m, center);

		// add to output octree
		ns->set_global_center(center);
		out->add_voxel(ns);
	}

    // remove linkage from source octree
    od->release_nodespec();
	
    // recursive descent part
    for (int ref=UPPER_LEFT_BACK; ref<NUMBER_OF_SPACE_PARTITIONS; ref++) {
	Octree_Data *kid = od->get_child((Octree_Child_Ref)ref);
	if (kid)
        	clip_and_merge(kid, clip, m2w, w2m, out);
    }
}

/// Merge destination models to single octree for ICP matcher.
/**
**   To match a new source model to a destination octree forest,
**   we can either search for point matches against each destination
**   model, or pre-merge the destination models into a single octree.
**   Since we're interested in matching terrain models, nominally
**   XY planes, stretching the matching space (dest model coords) 
**   in Z can improve performance. So we merge the destination 
**   models ahead of time. While we're at it, we can choose to limit 
**   the destination to an error-model-expanded bounding volume of 
**   the source octree.
**
**   src is the index of the source model in the forest.
**   dst is the index of a specific destination model in the forest,
**   or MATCH_PREV indicating a match against all models 0 to (src-1),
**   or MATCH_ALL  indicating a match against all models except src.
**
**   bvfactor scales the source octree (estimated world) bounding volume.
**   A bvfactor less than one shrinks the volume; 
**   a bvfactor greater than one expands the volume.
**
**   zstretch scales the merged octree's Z model space.
**
**   The return value is a dynamically allocated merged octree, which
**   the caller is responsible for deleting.
*/
Octree *matcher_merge(Forest *forest, int src, int dst,
		double bvfactor, double zstretch)
{
	int i;
	Summitt_range bvol;

	// get source octree node from forest
	ObjNode *srcn = forest->get_child(src);

	// find source bounding volume in (estimated) world space
	summitt_bounding_volume(srcn, &bvol);

	// scale volume to leave a margin inside or outside boundary
	double d = (bvol.xmax - bvol.xmin) * (bvfactor - 1.0)/2.0;
	bvol.xmin -= d;
	bvol.xmax += d;
	d = (bvol.ymax - bvol.ymin) * (bvfactor - 1.0)/2.0;
	bvol.ymin -= d;
	bvol.ymax += d;
	d = (bvol.zmax - bvol.zmin) * (bvfactor - 1.0)/2.0;
	bvol.zmin -= d;
	bvol.zmax += d;
	if (verbose > 1)
		bvol.dump(stderr, "source world");
    
    // find max destination octree depth, and forest base dest model
    int levels = 2;
    ObjNode *base = NULL;
    for (i=0; i<forest->get_num_children(); i++) {
	if (use_dest(i, src, dst)) {
		if (base == NULL)	// base is first dest model
			base = forest->get_child(i);
		Octree *oct = (Octree *)(forest->get_child(i)->get_object());
		if (oct->get_max_levels() > levels)
			levels = oct->get_max_levels();
	}
    }

    Octree *base_oct = (Octree *)(base->get_object());
    
    // create merged destination octree (MDO)
    Octree *mdo = new Octree(levels);
    Octree_Data *od = mdo->init_data();
    
    // model->object transform is same as base model except Z-stretched
    mdo->x.set_value(base_oct->x.get_value()); 
    mdo->y.set_value(base_oct->y.get_value());
    mdo->z.set_value(base_oct->z.get_value());
    mdo->xrot.set_value(base_oct->xrot.get_value());
    mdo->yrot.set_value(base_oct->yrot.get_value());
    mdo->zrot.set_value(base_oct->zrot.get_value());
    mdo->xscale.set_value(base_oct->xscale.get_value());
    mdo->yscale.set_value(base_oct->yscale.get_value());
    mdo->zscale.set_value(base_oct->zscale.get_value() / zstretch);

    // setup transform from world to MDO model space for merging dest pts
    ZMatrix mdo_m2w, mdo_w2m;
    base->set_object((Obj *)mdo);	// borrow objnode for obj-world xform
    base->GetTransformationMatrix(mdo_m2w);
    base->set_object(base_oct);		// restore
    MatInvert(mdo_m2w, mdo_w2m);	// get inverse transform
    
    // ready to merge destination points into MDO
    if (verbose)
    	fprintf(stderr, "merging dest to %d-level stretched octree\n", levels);
    	
    for (i=0; i<forest->get_num_children(); i++) {
	if (!use_dest(i, src, dst))
		continue;

	ObjNode *on = forest->get_child(i);
	Octree *oct = (Octree *)on->get_object();
	// setup xform from this dest model to world space
	ZMatrix m2w;
	on->GetTransformationMatrix(m2w);
	clip_and_merge(oct->get_data(), &bvol, m2w, mdo_w2m, od);
    }

    return mdo;
}
