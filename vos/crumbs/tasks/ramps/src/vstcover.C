// Look at VISTA file texture coordinates to evaluate mesh coverage
// of source image
#include <stdio.h>
#include <ctype.h>
#include <string.h>

#define TCRES	10		// resolution of texture coverage, each dim

typedef long I32;		// 32-bit integer value
typedef float F32;		// 32-bit float value
typedef double F64;		// 64-bit double value

enum { 	TEXREF_SIZE = 2048,	// bytes in a TextureRef
	COORDSYS_SIZE = 4096,	// bytes in CoordinateSystem spec
	VST_MAJOR = 0,		// current major version
	VST_MINOR = 9,		// current minor version
	NATIVE = 0x03020100	// endian field value if native format
};

struct BBox {			// Bounding box
	F32 xmin, ymin, zmin;
	F32 xmax, ymax, zmax;
};

struct VSTHeader {		// file header record
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
} hdr;

struct VSTVertex {		// vertex record
	F32 tex_s, tex_t;	// texture coordinate
	F32 x, y, z;		// spatial coordinate
};

struct LODHeader {		// Level of Detail header record
	I32 nbytes;		// total LOD size
	char reserved[8];
	I32 nvertices;
	F32 LOD_threshold;	// switching distance threshold
	I32 npatches;
	I32 max_vertex;		// highest vertex index referenced
};

struct PatchHeader {		// surface patch record
	char reserved[8];
	I32 type;		// 0=triangle strip, 1=point cloud
	I32 texture;		// TextureRef index
	I32 narray;		// number of index arrays
	I32 nindex;		// total point indices
};

// swap endian-ness on nwords 32-bit values at buf
static void swap(char *buf, int nwords)
{
	while (--nwords >= 0) {
		char a = buf[0]; buf[0] = buf[3]; buf[3] = a;
		char b = buf[1]; buf[1] = buf[2]; buf[2] = b;
		buf += 4;
	}
}

inline void swap(void *p, int n) { swap((char *)p, n); }

int main(int argc, char **argv)
{
	static char buf[4096];
	int i, j, k;
	int verbose = 0;
	
	char *vst = argv[1];
	if (argc == 3 && !strcmp(argv[1], "-v")) {
		verbose = 1;
		vst = argv[2];
	} else if (argc != 2) {
		fprintf(stderr, "usage: %s [-v] vst-input\n", argv[0]);
		return 1;
	}

	FILE *fp = fopen(vst, "rb");
	if (fp == NULL) {
		perror(vst);
		return 1;
	}

	// read file header
	if (fread(&hdr, 1, sizeof(hdr), fp) != sizeof(hdr)) {
		fprintf(stderr, "Can't read header\n");
		return 1;
	}

	// validate
	if (strncmp(hdr.magic, "VST", 3)) {
		fprintf(stderr, "Input is not a ViSTa file! (bad magic)\n");
		return 1;
	}

	// need to swap bytes?
	if (hdr.endian != NATIVE) {
		swap(&hdr.major, 2);
		swap(&hdr.ntexref, 3+6);
	}
	
	if (hdr.major != VST_MAJOR || hdr.minor != VST_MINOR)
		fprintf(stderr, "Warning, not expected version %d.%d\n",
			VST_MAJOR, VST_MINOR);

	// texture refs
	for (i=0; i<hdr.ntexref; i++) {
		if (fread(buf, 1, TEXREF_SIZE, fp) != TEXREF_SIZE) {
			fprintf(stderr, "Can't read texref %d\n", i);
			return 1;
		}
	}

	// coordinate system
	if (fread(buf, 1, COORDSYS_SIZE, fp) != COORDSYS_SIZE) {
		fprintf(stderr, "Can't read coordinate system\n");
		return 1;
	}

	// vertices - check texture coords
	int toutside = 0;
	int tcov[TCRES][TCRES];
	memset(tcov, 0, sizeof(tcov));

	for (i=0; i<hdr.nvertex; i++) {
		VSTVertex v;
		if (fread(&v, 1, sizeof(v), fp) != sizeof(v)) {
			fprintf(stderr, "Can't read vertex %d\n", i);
			return 1;
		}
		if (hdr.endian != NATIVE)
			swap(&v, sizeof(v)/4);
		// scale from (0.0-1.0) to (0 - TCRES-1)
		int ts = int(v.tex_s * TCRES);
		int tt = int(v.tex_t * TCRES);
		if (ts < 0 || ts >= TCRES || tt < 0 || tt >= TCRES)
			toutside++;
		else
			tcov[ts][tt]++;
	}

	// count number of regions covered
	int numcov = 0;
	for (i=0; i<TCRES; i++) {
		for (j=0; j<TCRES; j++) {
			if (tcov[i][j])
				numcov++;
		}
	}

	// dump coverage results
	printf("%d vertices, %d texture coordinates outside [0-1)\n",
		hdr.nvertex, toutside);
	printf("%d of %d regions (%dx%d) have coverage.\n", 
		numcov, TCRES*TCRES, TCRES, TCRES);
	if (verbose) {
		printf("Percent of mesh vertices per region:\n");
		for (i=0; i<TCRES; i++) {
			for (j=0; j<TCRES; j++) {
				printf("%5.1f", 
					100.0 * tcov[i][j] / hdr.nvertex);
			}
			putchar('\n');
		}
	}
	return 0;
}
