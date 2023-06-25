// vista.C 1.5 02/10/11 08:33:47
/** \file
// ViSTa file support functions
// define SWAP_ENDIAN to write in non-native endian format
*/

#include <stdio.h>
#include <string.h>
typedef int Boolean;
#include "vista.h"

typedef long I32;		// 32-bit integer value
typedef float F32;		// 32-bit float value
typedef double F64;		// 64-bit double value

#define VST_MAGIC	"VST"	// file header magic number

struct BBox {			///< Bounding box
	F32 xmin, ymin, zmin;
	F32 xmax, ymax, zmax;
};

struct VSTHeader {		///< file header record
	char magic[4];		// "VST"
	I32 endian;		// BE=03,02,01,00 LE=00,01,02,03
	I32 major;		// major version
	I32 minor;		// minor version
	char impl_id[4];
	char reserved[8];
	I32 ntexref;		// number of TextureRefs
	I32 nvertex;		// number of vertices
	I32 nLOD;		// number of Levels of Detail
	BBox box;		// overall bounding box
};

struct VSTVertex {		///< vertex record
	F32 tex_s, tex_t;	// texture coordinate
	F32 x, y, z;		// spatial coordinate
};

struct LODHeader {		///< Level of Detail header record
	I32 nbytes;		// total LOD size
	char reserved[8];
	I32 nvertices;
	F32 LOD_threshold;	// switching distance threshold
	I32 npatches;
	I32 max_vertex;		// highest vertex index referenced
};

struct PatchHeader {		///< surface patch record
	char reserved[8];
	I32 type;		// 0=triangle strip, 1=point cloud
	I32 texture;		// TextureRef index
	I32 narray;		// number of index arrays
	I32 nindex;		// total point indices
};

/// swap byte order for 32-bit value
static void *swap32(const char *b)
{
	static char swap[4];
	swap[0] = b[3];
	swap[1] = b[2];
	swap[2] = b[1];
	swap[3] = b[0];
	return swap;
}

/// swap byte order for 64-bit (double) value
static void *swap64(const char *b)
{
	static char swap[8];
	memcpy(swap, swap32(b+4), 4);
	memcpy(swap+4, swap32(b), 4);
	return swap;
}

/// convert 32-bit integer or float to native or swapped-endian value
#ifdef SWAP_ENDIAN
inline I32 i32(int i) 	{ return *(int *)swap32((char *)&i); }
inline F32 f32(float f)	{ return *(float *)swap32((char *)&f); }
inline F64 f64(double d) { return *(double *)swap64((char *)&d); }
#else
inline I32 i32(int i) 	{ return i; }
inline F32 f32(float f) { return f; }
inline F64 f64(double d) { return d; }
#endif

/// write file header
int vst_write_header(FILE *fp, const char *impl, int ntex, int nvert, 
			int nlod, const Summitt_range *bbox)
{
	VSTHeader hdr;
	
	memset(&hdr, 0, sizeof(hdr));
	if (nvert) {		// not a dummy, fill in non-zero data
		strcpy(hdr.magic, VST_MAGIC);
#ifdef SWAP_ENDIAN
		hdr.endian = 0x00010203;
#else
		hdr.endian = 0x03020100;
#endif
		strcpy(hdr.impl_id, impl ? impl : VST_SIMPLE);
		hdr.major   = i32(VST_MAJOR);
		hdr.minor   = i32(VST_MINOR);
		hdr.ntexref = i32(ntex);
		hdr.nvertex = i32(nvert);
		hdr.nLOD    = i32(nlod);
		if (bbox) {
			hdr.box.xmin = f32(bbox->xmin);
			hdr.box.ymin = f32(bbox->ymin);
			hdr.box.zmin = f32(bbox->zmin);
			hdr.box.xmax = f32(bbox->xmax);
			hdr.box.ymax = f32(bbox->ymax);
			hdr.box.zmax = f32(bbox->zmax);
		}
	}
	return fwrite(&hdr, 1, sizeof(hdr), fp);
}

/// write texture reference (path components separated by nulls)
int vst_write_texture(FILE *fp, const char *texname)
{
	static char buf[TEXREF_SIZE];
	memset(buf, 0, sizeof(buf));
	char *p = buf;
	for (int i=0; *texname && i<TEXREF_SIZE; i++, p++, texname++) {
		if (*texname != '/')
			*p = *texname;
	}
	return fwrite(buf, 1, TEXREF_SIZE, fp);
}

/// write coordinate system as empty section
int vst_write_coordsys(FILE *fp)
{
	// assuming COORDSYS_SIZE is multiple of 1024
	char buf[1024];
	memset(buf, 0, sizeof(buf));
	for (int i=0; i<COORDSYS_SIZE; i+=sizeof(buf)) {
		if (fwrite(buf, 1, sizeof(buf), fp) != sizeof(buf))
			return 1;
	}
	return 0;
}

/// write coordinate system as CAHV model
int vst_write_coordsys(FILE *fp, double c[], double a[], double h[], 
			double v[])
{
	// pack into buffer with correct endianness
	F64 cahv[4*3];
	int i;
	for (i=0; i<3; i++) {
		cahv[  i] = f64(c[i]);
		cahv[3+i] = f64(a[i]);
		cahv[6+i] = f64(h[i]);
		cahv[9+i] = f64(v[i]);
	}
	fwrite(cahv, 1, sizeof(cahv), fp);		
	
	// pad rest of section with zeros
	char buf[1024];
	memset(buf, 0, sizeof(buf));
	fwrite(buf, 1, sizeof(buf) - sizeof(cahv), fp);
	for (i=sizeof(buf); i<COORDSYS_SIZE; i+=sizeof(buf)) {
		if (fwrite(buf, 1, sizeof(buf), fp) != sizeof(buf))
			return 1;
	}
	return 0;
}

/// write vertex
int vst_write_vertex(FILE *fp, const float xyz[3], const float st[2])
{
	VSTVertex v;
	
	v.x = f32(xyz[0]);	// spatial coordinates
	v.y = f32(xyz[1]);
	v.z = f32(xyz[2]);
	v.tex_s = f32(st[0]);	// texture coordinates
	v.tex_t = f32(st[1]);
	return fwrite(&v, 1, sizeof(v), fp);
}

/// write LOD header
int vst_write_lod_header(FILE *fp, int nbytes, int nverts,
		int npatches, int maxvert, float threshold)
{
	struct LODHeader hdr;
	if (nbytes) {		// not a dummy, fill in non-zero data
		memset(&hdr, 0, sizeof(hdr));
		hdr.nbytes 	= i32(nbytes);
		hdr.nvertices 	= i32(nverts);
		hdr.npatches 	= i32(npatches);
		hdr.max_vertex 	= i32(maxvert);
		hdr.LOD_threshold = f32(threshold);
	}
	return fwrite(&hdr, 1, sizeof(hdr), fp);
}

/// write bounding box
int vst_write_bbox(FILE *fp, const Summitt_range *bbox)
{
	BBox b;
	b.xmin = f32(bbox->xmin);
	b.ymin = f32(bbox->ymin);
	b.zmin = f32(bbox->zmin);
	b.xmax = f32(bbox->xmax);
	b.ymax = f32(bbox->ymax);
	b.zmax = f32(bbox->zmax);
	return fwrite(&b, 1, sizeof(b), fp);
}

/// write triangle strip patch header
int vst_write_tripatch_header(FILE *fp, int texture, int narrays, int npts)
{
	struct PatchHeader hdr;
	memset(&hdr, 0, sizeof(hdr));
	hdr.texture = i32(texture);
	hdr.narray  = i32(narrays);
	hdr.nindex  = i32(npts);
	return fwrite(&hdr, 1, sizeof(hdr), fp);
}

/// write point cloud patch header
int vst_write_ptpatch_header(FILE *fp, int texture, int narrays, int npts)
{
	struct PatchHeader hdr;
	memset(&hdr, 0, sizeof(hdr));
	hdr.type    = i32(1);
	hdr.texture = i32(texture);
	hdr.narray  = i32(narrays);
	hdr.nindex  = i32(npts);
	return fwrite(&hdr, 1, sizeof(hdr), fp);
}

/// write one integer (array length, vertex index)
int vst_write_int(FILE *fp, int val)
{
	val = i32(val);
	return fwrite(&val, 1, 4, fp);
}

/// write array of N integers
int vst_write_ints(FILE *fp, int n, const int *list)
{
	int nb = 0;
	while (--n >= 0)
		nb += vst_write_int(fp, *list++);
	return nb;
}
