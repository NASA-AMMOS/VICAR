#ifndef _PATCH_H_
#define _PATCH_H_
// patch.h 1.6 03/08/28 12:54:17
/** \file
// Define terrain patch class.
//
// A Patch is a subclass of ObjNode representing a single patch or
// wedge of terrain generated from a single input dataset (e.g. 
// one stereo range map). The Obj contained in a Patch is always
// a SfcModel (octree with optional triangle mesh). The Patch object
// includes ancillary data for selecting and operating on the patch
// for constructing output products (3D models).
//
// The ObjNode name should be mappable back to the 
// range map and texture image.
//
// SUMMITT forests are lists of Patches. Patches are not stored directly
// in files - the ancillary data (including ObjNode stuff) is one
// part of the forest file, while the SfcModel (voxel/mesh) data is in
// a separate octree/surface file (named in the ObjNode 'reference' item).
//
// The voxels in a Patch's SfcModel Obj are in octree model coordinates
// (-1 to +1). The Obj transform maps these into camera frame (meters,
// +X forward, +Y left, +Z up). The Patch ObjNode transform in turn
// contains (possibly corrected) camera position and pointing to map
// Obj coords into world frame (typically meters, +X north, +Y east,
// +Z down). The Patch camera model is defined in camera frame and
// specifies the non-linear transform between 2D image pixel coordinates
// and 3D Obj (camera frame XYZ) coordinates. The Patch original pointing
// records the transform used to build the Patch's XYZ range map - 
// if the range map is in camera frame, the original pointing
// translation and rotation are all zeros.
//
// For the MER mission, site information is added. "World" frame means
// site zero, but the rover site number and currently-used site-to-world
// translation vector are saved to simplify updating the translation.
// The ObjNode name corresponds to the "PRODUCT_ID" header from the
// patch's source (XYZ) data.
**/

#include "grape/object.h"
#include "range.h"
#include "cahvor.h"

// (really should subclass Patch to MER_Patch, but I don't want
// to change lots of other code, so compile-time switch should be okay)
#define MER_MISSION

// application object types - should be in a common file...
#define PATCH_V1 101

class Patch : public ObjNode {

protected:
	/// Initialize
	void init() {
		base_model = FALSE;
		obj_res = 0.0;
		memset(otrans, 0, sizeof(otrans));
		memset(orot, 0, sizeof(orot));
#ifdef MER_MISSION
		site = 0;
		site_vector[0] = site_vector[1] = site_vector[2] = 0.0;
#endif
	}

public:

	virtual int get_type(void) { return(PATCH_V1); }

	/// ancillary patch data
	int base_model;		///< is this model part of the forest baseline?
	int xres, yres, bands;	///< texture image dimensions
	float fov;		///< image horizontal field of view, degrees
				// (-1 = LH height map, 0 = RH height map)
	CAHVOR cmod;		///< image camera model (in camera coord frame)
	Summitt_range bvol;	///< bounding volume, XYZ space, world coords

	double obj_res;		///< model resolution (nominal voxel edge)
				// (in object coords, 0=unknown)

	double otrans[3];	///< orig cam-to-site translation in XYZ file
	double orot[3];		///< orig cam-to-site rotation in XYZ file

	double get_res() {	///< model resolution in world coordinates
		return obj_res * (xscale + yscale + zscale) / 3.0; }

	void set_res(double wres) {	///< set world resolution
		obj_res = wres / ((xscale + yscale + zscale) / 3.0); }

	/// Read ancillary data from forest file, and optionally surface data
	virtual int parse_in(Dataport *fp, int expand=FALSE);
	
	/// Write ancillary data to forest file
	virtual int parse_out(Dataport *fp, int expand=FALSE);

	/// Update bounding volume from object voxel range
	void update_bvol();

	/// Constructor
	Patch() { init(); }


#ifdef MER_MISSION
	int site;		///< site index

	/** Current translation from world to site frame.
	 ** This is the sum of the site vectors for sites 1 through (site).
	 ** This translation (inverted) is already incorporated in 
	 ** the ObjNode transform. 
	 **/
	double site_vector[3];
#endif
};

#endif
