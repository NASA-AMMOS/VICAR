// Dump VST file
#include <stdio.h>
#include <ctype.h>
#include <string.h>

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

static int dump_array(int n, FILE *fp)
{
	for (int k=0; k<n; k++) {
		I32 v;
		if (fread(&v, 1, 4, fp) != 4)
			return k;
		if (hdr.endian != NATIVE)
			swap(&v, 1);
		printf("%c%6ld", k&7 ? ' ' : '\n', v);
	}
	printf("\n");
	return -1;
}

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
		fprintf(stderr, "usage: %s [-v] vst-input > dumpfile\n",
			argv[0]);
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

	// dump file header
	printf("== File Header ==\n");
	printf("version=%ld.%ld, impl=", hdr.major, hdr.minor);
	for (i=0; i<4; i++)
		printf(isprint(hdr.impl_id[i]) ? "%c" : "x%02X",
			hdr.impl_id[i]);
	printf("\n#tex=%ld #vert=%ld #LOD=%ld\n",
		hdr.ntexref, hdr.nvertex, hdr.nLOD);
	printf("bounding box: X=%f:%f Y=%f:%f Z=%f:%f\n",
		hdr.box.xmin, hdr.box.xmax, hdr.box.ymin, 
		hdr.box.ymax, hdr.box.zmin, hdr.box.zmax);

	// texture refs
	for (i=0; i<hdr.ntexref; i++) {
		if (fread(buf, 1, TEXREF_SIZE, fp) != TEXREF_SIZE) {
			fprintf(stderr, "Can't read texref %d\n", i);
			return 1;
		}
		char *s = buf;
		printf("Texture %d: %s", i, s);
		for (;;) {
			s += strlen(s) + 1;
			if (*s == 0)
				break;
			printf("/%s", s);
		}
		printf("\n");
	}

	// coordinate system
	if (fread(buf, 1, COORDSYS_SIZE, fp) != COORDSYS_SIZE) {
		fprintf(stderr, "Can't read coordinate system\n");
		return 1;
	}
	if (!strncmp(hdr.impl_id, "MER", 3)) {
		if (hdr.endian != NATIVE) {
			swap(buf, 3*2*4);
			I32 *p = (I32 *)buf;
			for (i=0; i<3*4; i++, p+=2) {	// swap 32-bit words
				I32 t = p[0];
				p[0] = p[1];
				p[1] = t;
			}
		}
		F64 *fp = (F64 *)buf;
		printf("Rover frame camera model:\n");
		printf("C = %f %f %f\n", fp[0], fp[1], fp[2]);
		printf("A = %f %f %f\n", fp[3], fp[4], fp[5]);
		printf("H = %f %f %f\n", fp[6], fp[7], fp[8]);
		printf("V = %f %f %f\n", fp[9], fp[10], fp[11]);
	}

	// vertices
	if (verbose) {
		printf("== Vertices ==\n");
		for (i=0; i<hdr.nvertex; i++) {
			VSTVertex v;
			if (fread(&v, 1, sizeof(v), fp) != sizeof(v)) {
				fprintf(stderr, "Can't read vertex %d\n", i);
				return 1;
			}
			if (hdr.endian != NATIVE)
				swap(&v, sizeof(v)/4);
			printf("%6d: X=%f Y=%f Z=%f   S=%f T=%f\n",
				i, v.x, v.y, v.z, v.tex_s, v.tex_t);
		}
	} else {
		fseek(fp, hdr.nvertex * sizeof(VSTVertex), SEEK_CUR);
	}

	// LODs
	for (i=0; i<hdr.nLOD; i++) {
		LODHeader lod;
		if (fread(&lod, 1, sizeof(lod), fp) != sizeof(lod)) {
			fprintf(stderr, "Can't read LOD %d header\n", i);
			return 1;
		}
		printf("== LOD %d ==\n", i);
		if (hdr.endian != NATIVE)
			swap(&lod, sizeof(lod)/4);
		printf("%6ld bytes, %6ld vertices, %6ld patches\n",
			lod.nbytes, lod.nvertices, lod.npatches);
		printf("threshold=%f max_vertex=%ld\n", 
			lod.LOD_threshold, lod.max_vertex);
		

		// bounding boxes at this LOD
		for (j=0; j<hdr.ntexref; j++) {
			BBox box;
			if (fread(&box, 1, sizeof(box), fp) != sizeof(box)) {
				fprintf(stderr, 
					"Can't read LOD %d tex %d bbox\n", 
					i, j);
				return 1;
			}
			if (hdr.endian != NATIVE)
				swap(&box, sizeof(box)/4);
			printf("LOD %d Texture %d bounding box: "
				"X=%f:%f Y=%f:%f Z=%f:%f\n",
				i, j, box.xmin, box.xmax, box.ymin, 
				box.ymax, box.zmin, box.zmax);
		}


		// patches at this LOD
		for (j=0; j<lod.npatches; j++) {
			PatchHeader patch;
			if (fread(&patch, 1, sizeof(patch), fp) != 
							sizeof(patch)) {
				fprintf(stderr, 
					"Can't read LOD %d patch %d header\n", 
					i, j);
				return 1;
			}
			if (hdr.endian != NATIVE)
				swap(&patch, sizeof(patch)/4);
			printf("-- LOD %d patch %d --\n", i, j);
			printf("type=%s texture=%ld #arrays=%ld #points=%ld\n",
				patch.type ? "point cloud" : "tri strip",
				patch.texture, patch.narray, patch.nindex);

			// index array lengths
			if (verbose) {
				printf("index array lengths:");
				k = dump_array(patch.narray, fp);
				if (k >= 0) {
					fprintf(stderr, "\nCan't read LOD %d "
						"patch %d indexlen %d\n",
						i, j, k);
					return 1;
				}
			} else if (patch.narray) {
				int ntri = 0;
				for (k=0; k<patch.narray; k++) {
					I32 v;
					if (fread(&v, 1, 4, fp) != 4) {
						fprintf(stderr, 
							"\nCan't read LOD %d "
							"patch %d indexlen %d\n",
							i, j, k);
						return 1;
					}
					if (hdr.endian != NATIVE)
						swap(&v, 1);
					ntri += v-2;
				}
				printf("%d triangles, average strip = %d\n",
					ntri, ntri/patch.narray);
			}

			// indices
			if (verbose) {
				printf("vertex indices:");
				k = dump_array(patch.nindex, fp);
				if (k >= 0) {
					fprintf(stderr, "\nCan't read LOD %d "
						"patch %d index %d\n",
						i, j, k);
					return 1;
				}
			} else {
				fseek(fp, patch.nindex * 4, SEEK_CUR);
			}
		}
	}

	printf("== Done ==\n");
}
