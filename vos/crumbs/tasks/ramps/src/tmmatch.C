// tmmatch.C 1.3 02/08/13 15:53:12
/** \file
** Apply the SUMMITT iterative closest point algorithm for the
** PVM terrain merge slave task.
**
** To match a new source model to a destination octree forest,
** we can either search for point matches against each destination
** model, or pre-merge the destination models into a single octree.
** Since we're interested in matching terrain models, nominally
** planes, stretching the matching space (dest model coords) 
** in height can improve performance. So we merge the destination 
** models ahead of time. While we're at it, we can choose to limit 
** the destination to an error-model-expanded bounding volume of 
** the source octree.
**
** At startup, the forest data is loaded, including voxels for
** base models only, and merged to a "base destination octree".
** For each new patch request, a "working destination octree" is
** built from the base destination octree and any non-base forest 
** models that overlap a clip volume (scaled bounding
** volume of the source patch). Only voxels inside the clip volume
** are used. If stretching is enabled, only 2 ICP iterations are 
** done with this working destination, then its voxels are rebuilt 
** into a stretched working destination octree for the remaining iterations.
**
** The base and working destination octrees use an identity object-world
** transform (object space = world coords) and a model space covering
** the nominal expected site volume.
**
*/
#include <ctype.h>
#include "summitt_func.h"

// define if we care about accurate surface normals when stretching model
#define STRETCH_NORMALS

// Working forest
static Forest forest;

// Base destination octree (BDO), with voxels from all base models.
// Created at startup and kept intact.
static ObjNode bdo;

// Working destination octree (WDO), created on the fly for each match.
static ObjNode wdo;

// Unstretched world<->WDO-model transforms for clipping, merging
static ZMatrix wdo_m2w, wdo_w2m;

// program options come from config file, are read by tmerge master,
// and passed to tmslave via command line
extern double bvscale, tepsilon, repsilon, tolerance, err_radius;
extern double stretch[3];
extern int max_iterations;

// octree merge options - should these be command args?
static Boolean accum = FALSE;
static Boolean level_grow = TRUE;

// diagnostic count of voxels going into working destination octree
static int wdo_count;

extern int icp_flags;

// octree merge support in summitt_func.C
void summitt_verify_level( Octree *oct1, Octree *oct2,
        ZMatrix m2w1, ZMatrix w2m2, GeoData *geo1, GeoData *geo2);
void track_memory(const char *checkpoint);

// ** temp
#if 0
#define dump_octree(o,n)
#else
static void dump_octree(Octree *oct, const char *name)
{
if (icp_flags & 256) {
	FILE_Dataport fp;
	fp.wopen(name);
	oct->parse_out(&fp);
	fp.close();
}
}
#endif

/// load current forest and all base models to BDO at startup
// width = meters across site volume for BDO/WDO model transform
void load_forest(char *name, float width)
{
	// load patch header data but not component models
	FILE_Dataport fp;
	if (!fp.ropen(name)) {
		fprintf(stderr, "Can't open forest %s\n", name);
		exit(1);
	}
	if (verbose)
		fprintf(stderr, "Loading forest %s\n", name);
	char buf[1024];
	get_next_token(&fp, buf);	// skip GRP_V1 token
	if (!forest.parse_in(&fp, FALSE)) {
		fprintf(stderr, "Error reading forest %s\n", name);
		exit(1);
	}
	fp.close();

	// if forest name is an absolute path, change to its
	// directory so that relative component object paths are
	// resolved properly when loading models
	if (name[0] == '/') {
		strncpy(buf, name, sizeof(buf));
		char *endslash = strrchr(buf, '/');
		*endslash = 0;
		if (verbose)
			fprintf(stderr, "Changing to forest directory %s\n",
				buf);
		chdir(buf);
	}

	// Create BDO. For a well-formed octree, we want the model space
	// to cover the nominal site volume. The object and world space
	// *are* site frame.
	Octree *bdoct = new Octree(10);	// initial depth, will grow as needed
	Octree_Data *bdod = bdoct->init_data();
	bdo.set_object(bdoct);
	// model transform maps +1 to width/2, -1 to -width/2
	bdoct->xscale = bdoct->yscale = bdoct->zscale = width/2.0;

	// Load voxel data for base models only, merging them into the BDO.
	for (int i=0; i<forest.get_num_children(); i++) {
		Patch *p = forest.get_patch(i);		
		if (p->base_model) {
			if (!p->parse_reference(&fp))
				continue;	// failed!

			if (verbose)
				fprintf(stderr, "Merging base model %d\n", i);
			// merge this base model to BDO, moves voxels over
			summitt_merge_octrees((ObjNode *)p, &bdo,
							accum, level_grow);
			// free up octree's node links
			delete p->get_object();
			p->set_object(NULL);
		}
	}

	dump_octree((Octree *)bdo.get_object(), "basedest.oct");

	// get unstretched transforms world<->WDO model space,
	// since this never changes
	bdo.GetTransformationMatrix(wdo_m2w);
	MatInvert(wdo_m2w, wdo_w2m);	// get inverse transform
}

/// Copy model-to-object transform, with possible scaling
static void copy_m2o(Octree *src, Octree *dest, const double scale[3])
{
	dest->x.set_value(src->x.get_value() * scale[0]); 
	dest->y.set_value(src->y.get_value() * scale[1]);
	dest->z.set_value(src->z.get_value() * scale[2]);
	dest->xrot.set_value(src->xrot.get_value());
	dest->yrot.set_value(src->yrot.get_value());
	dest->zrot.set_value(src->zrot.get_value());
	dest->xscale.set_value(src->xscale.get_value() / scale[0]);
	dest->yscale.set_value(src->yscale.get_value() / scale[1]);
	dest->zscale.set_value(src->zscale.get_value() / scale[2]);
}

/// Recursive voxel copy from base BDO (od) to working WDO (out), 
// clipping to world space volume limits.
static void clip_copy(Octree_Data *od, Summitt_range *clip, Octree_Data *out)
{
	// do voxels at this level
	NodeSpec *nextptr;
	for (NodeSpec *ns = od->get_node_data(); ns; ns = ns->next) {
		// get model coords, transform to world space
		double center[3], center_trans[3];
		ns->get_global_center(center);
		MultPoints(center, wdo_m2w, center_trans);

		// check against clip limits
		if (!clip->in_range(center_trans))
			continue;

		// copy to output WDO octree
		out->add_voxel(ns->duplicate());
		wdo_count++;
	}

	// recursive descent part
	for (int ref=UPPER_LEFT_BACK; ref<NUMBER_OF_SPACE_PARTITIONS; ref++) {
		Octree_Data *kid = od->get_child((Octree_Child_Ref)ref);
		if (kid)
        		clip_copy(kid, clip, out);
	}
}

/// Recursive voxel move from non-base patch (od) to working WDO (out), 
// clipping to world space volume limits and transforming to WDO model space.
// Note: voxels aren't duplicated, so this leaves the input tree 
// with links but no voxels.
static void clip_move(Octree_Data *od, Summitt_range *clip, ZMatrix m2w,
				Octree_Data *out)
{
	// do voxels at this level
	NodeSpec *nextptr;
	for (NodeSpec *ns = od->get_node_data(); ns; ns = nextptr) {
		nextptr = ns->next;
		ns->next = NULL;

		// get model coords, transform to world space
		double center[3], center_trans[3];
		ns->get_global_center(center);
		MultPoints(center, m2w, center_trans);

		// check against clip limits
		if (!clip->in_range(center_trans)) {
			delete ns;	// don't leak it
			continue;
		}

		// transform to WDO's model space
		MultPoints(center_trans, wdo_w2m, center);

		// add to output octree
		ns->set_global_center(center);
		out->add_voxel(ns);
		wdo_count++;
	}

	// remove linkage from source octree
	od->release_nodespec();
	
	// recursive descent part
	for (int ref=UPPER_LEFT_BACK; ref<NUMBER_OF_SPACE_PARTITIONS; ref++) {
		Octree_Data *kid = od->get_child((Octree_Child_Ref)ref);
		if (kid)
        		clip_move(kid, clip, m2w, out);
	}
}

/// Simple recursive voxel move from unstretched to stretched model space.
static void move_voxels(Octree_Data *od, ZMatrix xform, Octree_Data *out)
{
	// do voxels at this level
	NodeSpec *nextptr;
	for (NodeSpec *ns = od->get_node_data(); ns; ns = nextptr) {
		nextptr = ns->next;
		ns->next = NULL;

		// get model coords, transform to new model space
		double center[3], center_trans[3];
		ns->get_global_center(center);
		MultPoints(center, xform, center_trans);
		ns->set_global_center(center_trans);

#ifdef STRETCH_NORMALS
		// normal vectors should get transformed also
		double normal[3];
		ns->get_normal(normal);
		normal[0] /= stretch[0];
		normal[1] /= stretch[1];
		normal[2] /= stretch[2];
		// back to unit length
		normalize_vector(normal);
		ns->set_normal(normal);
#endif

		// add to output octree
		out->add_voxel(ns);
	}

	// remove linkage from source octree
	od->release_nodespec();
	
	// recursive descent part
	for (int ref=UPPER_LEFT_BACK; ref<NUMBER_OF_SPACE_PARTITIONS; ref++) {
		Octree_Data *kid = od->get_child((Octree_Child_Ref)ref);
		if (kid)
        		move_voxels(kid, xform, out);
	}
}

// Setup (unstretched) working destination octree WDO, including BDO and 
// any non-base patches that overlap the bounding volume. 
// WDO only contains voxels inside the bounding volume. 
// BDO voxels are left intact.
static void build_working_dest(Summitt_range *bvol)
{
	// free up previous WDO octree
	delete wdo.get_object();

	// create new WDO octree, same depth as BDO
	Octree *bdoct = (Octree *)(bdo.get_object());
	Octree *wdoct = new Octree(bdoct->get_max_levels());
	Octree_Data *wdod = wdoct->init_data();
	wdo.set_object(wdoct);

	wdo_count = 0;			// reset for diagnostics
	static const double nostretch[] = { 1.0, 1.0, 1.0 };
	copy_m2o(bdoct, wdoct, nostretch);		// copy transform
	if (bdoct->get_data()) 				// copy voxels
		clip_copy(bdoct->get_data(), bvol, wdod);	
	dump_octree(wdoct, "workdest1.oct");

	// load non-base models that overlap bounding volume
	for (int i=0; i<forest.get_num_children(); i++) {
		Patch *p = forest.get_patch(i);
		if (p->base_model)
			continue;	// already loaded
		if (p->bvol.overlaps(bvol)) {
			if (verbose)
				fprintf(stderr, "loading forest entry %d, "
					"voxel count=%d\n", i, wdo_count);
        		FILE_Dataport fp;
			if (!p->parse_reference(&fp))
				continue;
			Octree *poct = (Octree *)p->get_object();

			// ** test, accumulate to reduce memory use
			if (icp_flags & 16)
				poct->set_max_levels(poct->get_max_levels()-2,
							TRUE);

			// Move voxels to working destination
			// First, grow destination octree depth if needed
			// (same way that summitt_merge_octrees() does)
			ZMatrix m2w;
			p->GetTransformationMatrix(m2w);
			summitt_verify_level(poct, wdoct, m2w, wdo_w2m,
								NULL, NULL);
			clip_move(poct->get_data(), bvol, m2w, wdod);

			// free up octree's node links
			delete p->get_object();
			p->set_object(NULL);
		}
	}
	dump_octree(wdoct, "workdest2.oct");

	if (verbose)
		fprintf(stderr, "%d voxels copied to WDO\n", wdo_count);
}

// Rebuild working destination octree (WDO) with stretching, 
// moving voxels to new octree. ObjNode (world) transform is
// the same, only the model-object transform changes.
static void stretch_octree(const double scale[3])
{
	Octree *oct_in = (Octree *)wdo.get_object();
	Octree *oct_out = new Octree(oct_in->get_max_levels());
	Octree_Data *od = oct_out->init_data();

	// adjust transform
	ZMatrix old_m2o, new_o2m;
	oct_in->GetModelToObjectTransform(old_m2o);
	copy_m2o(oct_in, oct_out, scale);
	oct_out->GetObjectToModelTransform(new_o2m);

	// transform to map from old to new model space
	MatPreMult(new_o2m, old_m2o);

	// move voxels to stretched octree
	move_voxels(oct_in->get_data(), new_o2m, od);

	// free up old octree nodes
	delete oct_in;

	// point WDO at new stretched octree
	wdo.set_object((Obj *)oct_out);

	dump_octree(oct_out, "stretch.oct");
}

/// Perform ICP matching of new model (source node) against forest.
// Returns zero if successful, with source node's transform updated.
int match_model(ObjNode *src, Summitt_range *mbvol)
{
	// convert bounding volume to (estimated) world space
	Summitt_range bvol = *mbvol;
	world_volume(src, &bvol);
	if (verbose)
		bvol.dump(stderr, "source estimated world volume for match");

	// expand (or shrink) bounding volume by scale factor
	double margin;
	margin = (bvol.xmax - bvol.xmin) * (bvscale - 1.0);
	bvol.xmin -= margin;
	bvol.xmax += margin;
	margin = (bvol.ymax - bvol.ymin) * (bvscale - 1.0);
	bvol.ymin -= margin;
	bvol.ymax += margin;
	margin = (bvol.zmax - bvol.zmin) * (bvscale - 1.0);
	bvol.zmin -= margin;
	bvol.zmax += margin;

	// setup working destination octree (WDO)
	build_working_dest(&bvol);
	track_memory("Built WDO");

extern double vsum_factor;
	vsum_factor = -1.0;
	if (stretch[0] != 0.0) {
		// First pass on unstretched model to get basic orientation.
		if (summitt_icp_match_octrees(src, &wdo, 2, tepsilon, 
				repsilon, tolerance, bvscale,
				err_radius, TRUE) < 0.0)
			return -1;

	    	// rebuild destination octree with stretching
		stretch_octree(stretch);
		track_memory("Stretched WDO");
		vsum_factor = 0.75;
	}

	// Continue matching against (stretched) model.
	float result = summitt_icp_match_octrees(src, &wdo, max_iterations, 
		tepsilon, repsilon, tolerance, bvscale, err_radius, TRUE);

	return result < 0.0;
}
