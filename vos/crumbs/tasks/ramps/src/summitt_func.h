#ifndef _SUMMITT_FUNC_H_
#define _SUMMITT_FUNC_H_
// summitt_func.h 1.18 02/07/10 15:45:22
/** \file
 ** Support functions for summitt programs
 **/
#include "grape/sfcmodel.h"
#include "forest.h"
#include "range.h"
//#include "cahvor.h"

// summit_match dst options
enum {	MATCH_ALL = -1,
	MATCH_PREV = -2
};

extern int verbose;		///< enable trace message output?

int summitt_clip_octree(Octree_Data *, Summitt_range *, 
	ZMatrix,  Octree_Data *);
float summitt_merge_octrees(ObjNode *, ObjNode *, Boolean, Boolean);
float summitt_icp_match_octrees(ObjNode *src, ObjNode *dst,
	int max_iterations, double tepsilon, double repsilon,
	double tolerance, double bvfactor=1.0, double err_radius=0.0, 
	Boolean cleanup=TRUE);
void summitt_octree_extents(Octree *octree, Summitt_range *r);
void summitt_bounding_volume(ObjNode *node, Summitt_range *r);
void world_volume(ObjNode *node, Summitt_range *r);
int  count_known_voxels();
int  count_empty_voxels();
Octree *matcher_merge(Forest *forest, int src, int dest,
	double bvfactor, double zstretch);

/*
int summitt_sfc2vst(const char *impl,
	int num_models, SfcModel *sfc[], char *outfile,
	char *texfile, char *texext, int xres, int yres, double fov, 
	ZMatrix xform, CAHVOR *ccmod, CAHVOR *wcmod, int compact_verts=FALSE);
*/
int summitt_config(const char *program, char *args[], int maxargs);

MT_Mesh *make_triangle_model_from_forest(Forest *fst,
		double cos_thresh, int max_proj, int acc_levels,
		int check_all, int zdown=FALSE, int skip_new_mesh=FALSE);

#ifdef BASEIMAGE

// range2octree option flags
enum {	R2O_XCAM = 1,		// transform to camera frame?
	R2O_MESH = 2,		// generate triangle mesh?
	R2O_EYE = 4,		// show eye voxel?
	R2O_GRID = 8		// show grid voxels?
};

SfcModel *range2octree(ImageData *xyz, ImageData *rgb, double cam[],
	float fov, float range_low, float range_high, 
	ZMatrix cxform, int flags, double mcnorm, double fthresh, 
	double zstretch, SfcModel *insfc=NULL);

void summitt_range_resample(ImageData *imap, ImageData *omap);
void summitt_range_copy(ImageData *imap, ImageData *omap);
void summitt_range_filter(ImageData *imap, ImageData *omap, float delta);
void summitt_range_interpolate(ImageData *imap, int maxgap);

#endif

#endif
