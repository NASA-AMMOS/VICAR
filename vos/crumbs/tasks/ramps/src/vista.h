#ifndef VISTA_H
#define VISTA_H
// vista.h 1.6 02/10/11 08:33:25
/** \file
 ** ViSTa file format services
 **/
#include <stdio.h>
#include "range.h"

enum { 	TEXREF_SIZE = 2048,	///< bytes in a TextureRef
	COORDSYS_SIZE = 4096,	///< bytes in CoordinateSystem spec
	VST_MAJOR = 0,		///< current major version
	VST_MINOR = 9		///< current minor version
};

#define VST_SIMPLE      "SMP"   ///< simple implementation ID (rover frame)
#define VST_FIDO        "FDO"   ///< FIDO implementation ID (rover frame)
#define VST_MER         "MER"   ///< MER implementation ID (camera frame)

/// All vst_write_...() functions return number of bytes written.

/// write (real or temporary) header
int vst_write_header(FILE *fp, const char *impl=NULL, int ntex=0, 
	int nvert=0, int nlod=0, const Summitt_range *bbox=NULL);

/// write texture reference
int vst_write_texture(FILE *fp, const char *texname);

/// write coordinate system as empty section
int vst_write_coordsys(FILE *fp);

/// write coordinate system as CAHV model
int vst_write_coordsys(FILE *fp, 
	double c[], double a[], double h[], double v[]);

/// write coordinate system as 4x4 transform
int vst_write_coordsys(FILE *fp, double xform[]);

/// write vertex
int vst_write_vertex(FILE *fp, const float xyz[3], const float st[2]);

/// write LOD header
int vst_write_lod_header(FILE *fp, int nbytes=0, int nverts=0,
		int npatches=0, int maxvert=0, float threshold=0.0f);

/// write bounding box
int vst_write_bbox(FILE *fp, const Summitt_range *bbox);

/// write triangle strip patch header
int vst_write_tripatch_header(FILE *fp, int texture=0, int narrays=0, 
		int npts=0);

/// write point cloud patch header
int vst_write_ptpatch_header(FILE *fp, int texture=0, int narrays=0, 
		int npts=0);

/// write one integer (array length, vertex index)
int vst_write_int(FILE *fp, int val);

/// write array of N integers
int vst_write_ints(FILE *fp, int n, const int *list);

#endif
