// tilemesh.h 1.3 03/05/01 07:56:13
// Definitions for tile mesh generator

#define MAX_PATCH	100	// (in one tile)

GLOBAL struct PInfo {	// info for forest patches overlapping current tile
	Patch *p;		// the patch
	ImageData *xyz;		// XYZ range map (current LOD, clipped)
	Mesh_Triangle *tri;	// triangle mesh list
	ZMatrix m2cam;		// xform merged model to patch camera space
} plist[MAX_PATCH];
GLOBAL int npatch;

void write_mesh(FILE *fp, Triangle_Model *mesh, SfcModel *sfc, 
			Range *bvol, int flip);
void write_mesh_obj(FILE *fp, Triangle_Model *mesh, SfcModel *sfc, 
			Range *bvol, int flip);
void track_memory(const char *checkpoint);
