// asd2vst.C 1.5 02/10/11 08:30:47
/** \file
// Convert ASD Mesh file to ViSTa binary file
// (code is adapted from asdload.C)
//
// This program converts a SUMMITT ASD mesh file (as created by asdmesh)
// into ViSTa format for SAP.
// The ASD file must have per-vertex explicit texture coordinates.
//
// Relevant data from the ASD mesh is loaded into memory, then the
// output file is created. The tricky part is merging texture coordinates
// from attribute records with vertices, which may require creating
// additional vertices (same spatial coords, new texture coords).
//
// Another small complication is filling in of header record fields.
// Some headers are re-written after writing the record's data,
// once the necessary information (e.g. byte count) has been computed.
*/

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <math.h>
#include <grape/matrix.h>
#include "darray.h"
typedef int Boolean;
#include "vista.h"
#include "asdvst.h"

#ifndef FLT_MAX
#define FLT_MAX 	1.0E30
#endif

static const char usage[] = 
	"usage: %s [-b beginLOD] [-e endLOD] [-g] [-s x y z] [-t x y z]\n"
	"   [-r x y z] [-ext texture-suffix] [-v]\n" 
	"   -i asdmesh-input -o vista-output\n"
	"-b = beginning level of detail to include (default=4)\n"
	"-e = ending level of detail to include (default=last)\n"
	"-g = glom triangles into strips\n"
	"-s = coordinate scaling (input*scale = meters)\n"
	"-t = coordinate translation offset in meters\n"
	"-r = coordinate rotation to world frame\n"
	"-ext = replace texture filename suffix\n"
	"-v = verbose output to stderr\n"
	"-i = ASD mesh input filename\n"
	"-o = ViSTa output filename\n";

// default array allocation sizes
enum {	DEF_VERTICES 	= 1000,
	DEF_ATTRS 	= DEF_VERTICES,
	DEF_FACES	= 2*DEF_VERTICES
};

// bounding boxes - overall, and one per texture
static Summitt_range bbox;
static Summitt_range *tbox;
static float bb_side;		// average box side length

// LOD range to convert
static int min_lod = 4;
static int max_lod = 30;

// Coordinate transform 
static ZMatrix xform;

// ViSTa output file pointer
static FILE *ofp;

// per-vertex attributes
static int pv_color, pv_normal, pv_texture;
static int attr_size = 0;	// floats per attribute value

static int cur_lod;		// current level of detail
static int num_textures;	// total number of output textures
static int num_vertices;	// total number of output vertices
static int face0;		// ASD file face index for first loaded face

// dynamic component arrays
static DArray *attr;		// attribute array
static DArray *vert;		// input vertex array
static DArray *face;		// master face list
static LOD *lod;		// LOD array

static int adjacency;		// input file has adjacency info
int stripit;			// build triangle strips

int verbose;

// Handle parsing error.
static void parse_error(char *msg, char *arg)
{
	fprintf(stderr, "asd2vst: %s %s\n", msg, arg);
	exit(1);
}

// parse "p [color] [normal] [texture]"
static void parse_per_vertex(char *s)
{
	s++;
	for (;;) {
		while (isspace(*s))	// skip whitespace
			s++;
		if (*s < ' ')		// end of string
			break;

		switch (*s) {
		case 'c':
			pv_color = 1;	break;
		case 'n':
			pv_normal = 1;	break;
		case 't':
			pv_texture = 1;	break;
		default:
			parse_error("Invalid per-vertex attribute", s);
			return;
		}

		while (*s && !isspace(*s))	// skip token
			s++;
	}

	// ViSTa constraints
	if (!pv_texture)
		parse_error("Per-vertex texture is required for ViSTa", "");

        // attribute size depends on per-vertex options
        attr_size = 2;			// required texture
        if (pv_color)
                attr_size += 3;
        if (pv_normal)
                attr_size += 3;
}

// allocate arrays
static void allocate(int nv, int na, int nf)
{
	vert = new DArray(sizeof(Vertex), nv);
	attr = new DArray(sizeof(Attr), na);
	face = new DArray(sizeof(Face), nf);
}

// allocate arrays with default sizes if no allocation requested yet
static void check_alloc()
{
	if (vert == NULL)
		allocate(DEF_VERTICES, DEF_ATTRS, DEF_FACES);
}

// parse "n <nvertex> <nface> <nattr> <ntexture>"
static void parse_alloc(char *s)
{
	int nv, nf, na, nt;
	if (vert)
		parse_error("Alloc line too late", s);
	else if (sscanf(s+1, "%d %d %d %d", &nv, &nf, &na, &nt) != 4)
		parse_error("Invalid alloc line", s);
	else
		allocate(nv, na, nf);
}

// parse "g <red> <green> <blue> <alpha>"
static void parse_global(char *s)
{
	// not applicable for ViSTa
}

// parse "t <texture_map_name> [sa sb sc sd ta tb tc td]"
// optionally replace filename extension
static void parse_texture(char *s, char *ext)
{
	char name[1024];
	float sa, sb, sc, sd, ta, tb, tc, td;
	int n = sscanf(s+1, "%s %f %f %f %f %f %f %f %f", 
			name, &sa, &sb, &sc, &sd, &ta, &tb, &tc, &td);
	if (name[0] == '"') {	// handle quote-delimited name
		while (*++s != '"')
			;	// need to explicitly skip leading whitespace
		n = sscanf(s, "\"%[^\"]\" %f %f %f %f %f %f %f %f", 
			name, &sa, &sb, &sc, &sd, &ta, &tb, &tc, &td);
	}

	if (n == 9)
		parse_error("Texgen coords not supported in ViSTa\n", "");
	if (n != 1)
		parse_error("Invalid texture line", s);

	// change texture filename extension if desired
	if (ext) {
		char *dot = strrchr(name, '.');
		if (!dot) {
			dot = name + strlen(name);
			*dot = '.';
		}
		strcpy(dot+1, ext);
	}
	
	vst_write_texture(ofp, name);
	num_textures++;		// keep count
}

// parse "l <level-of-detail> [<switchin> [<morph>]]"
static void parse_LOD(char *s)
{
	int new_lod;
	float switchin, morph;

	int n = sscanf(s+1, "%d %f %f", &new_lod, &switchin, &morph);
	// expecting LOD to be one above previous LOD
	if (n < 1)
		parse_error("missing LOD value", s);
	if (new_lod != cur_lod+1)
		parse_error("unexpected LOD value", s);
	cur_lod = new_lod;

	if (cur_lod < min_lod)	// don't care about this one?
		return;

	lod[cur_lod].switchin = (n<2) ? 0.0 : switchin;

	// allocate list of face chains for each texture
	lod[cur_lod].face = new Face *[num_textures];
	memset(lod[cur_lod].face, 0, num_textures * sizeof(Face *));
}

// parse "x xf[0][0] xf[0][1] ... xf[3][3]"
static void parse_xform(char *s)
{
	for (int i=0; i<4; i++) {
		for (int j=0; j<4; j++) {
			char *e;
			xform[i][j] = strtod(s+1, &e);
			if (e == s+1)
				parse_error("missing Xform value", s);
			s = e;
		}
	}
}

// parse "s <flag>"
// indicating if triangle strip adjacency info is included
static void parse_strip(char *s)
{
	if (sscanf(s+1, "%d", &adjacency) != 1)
		parse_error("invalid strip adjacency line", s);
}

// parse "a [<nx ny nz [dnx dny dnz]>] [<r g b [dr dg db]>] [<u v [du dv]>]"
// (only care about texture coords u v for VST)
static void parse_attribute(char *s)
{
	check_alloc();

	float f[6+6+4];		// fields from file line

	// extract the bunch o' floats to f[]
	int i;			// input, output index
	for (i=0; i<6+6+4; i++) {
		char *e;
		f[i] = strtod(s+1, &e);
		if (e == s+1)
			break;
		s = e;
	}

	// check if got enough values
        if (i < attr_size)
                parse_error("missing vertex attribute data", s);

        // enough for difference values too?
        int diffs = (i > attr_size) ? 2 : 1;

	// find texture coords in array
	int tc = 0;
	if (pv_normal)
		tc += 3 * diffs;
	if (pv_color)
		tc += 3 * diffs;

	// store in array.
	// Flip t so 0,0 is at top instead of bottom.
	// ** (really should be (yres-1)/yres - f[tc+1])
	Attr a;
	a.s = f[tc];
	a.t = 1.0 - f[tc+1];
	if (attr->store(&a))
		parse_error("Attribute array overflow", s);
}

// parse "v x y z [xd yd zd]"
static void parse_vertex(char *s)
{
	check_alloc();

	Vertex v;
	double p[3], px[3];
	int n = sscanf(s+1, "%lf %lf %lf", &p[0], &p[1], &p[2]);
	if (n < 3)
		parse_error("Vertex data missing at", s);

	// transform to output frame
	MultPoints(p, xform, px);
	v.xyz[0] = (float)px[0];
	v.xyz[1] = (float)px[1];
	v.xyz[2] = (float)px[2];

	// allocate per-texture output vertex info
	v.sv = new SubVert[num_textures];

	// mark them all as unused
	memset(v.sv, 0xFF, num_textures * sizeof(SubVert));

	// store vertex in array
	if (vert->store(&v))
		parse_error("Vertex array overflow", s);
}

// parse "f v1 v2 v3  r1 r2 r3  c1 c2 c3 c4 [t] [va1 va2 va3  ra1 ra2 ra3]"
// (only care about final vertices, texture ID, and final attributes,
// and whether it has any children).
// If "adjacency" flag is set (from header), "adj" contains IDs of
// adjacent faces, for triangle stripping
static void parse_face(char *s, char *adj)
{
	int i;

	// get final vertex indices
	Face f;
	for (i=0; i<3; i++) {
		int vnum = strtol(s+1, &s, 10);
		if (vnum < 0 || vnum >= vert->curlen()) {
			parse_error("invalid face vertex ID", s);
			vnum = 0;
		}
		f.v[i] = (Vertex *)vert->item(vnum);
	}

	// skip ref and child stuff, but count number of kids
	for (i=0; i<3; i++)
		strtol(s+1, &s, 10);
	int nkids = 0;
	for (i=0; i<4; i++) {
		int child = strtol(s+1, &s, 10);
		if (child >= 0)
			nkids++;
	}
	f.final = (nkids == 0);
		
	// texture ID
	int t = strtol(s+1, &s, 10);
	if (t < 0 || t >= num_textures) {
		parse_error("Invalid face texture ID", s);
		t = 0;
	}

	// associate attributes (for texture coords) with vertices
	for (i=0; i<3; i++)
		f.v[i]->sv[t].attr = strtol(s+1, &s, 10);

	// Store adjacency info for triangle stripping.
	// face0 is offset from file face ID to face array index
	// (in case skipped initial LODs)
	if (adjacency) {
		int af[3];	// adjacent face file indices
		if (sscanf(adj, "%d %d %d", &af[0], &af[1], &af[2]) != 3) {
			parse_error("Bad face adjacency line", s);
			af[0] = af[1] = af[2] = -1;
		}

		int naf = 0;
		for (i=0; i<3; i++) {
			if (af[i]>=0)	// valid adjacent face, save ptr
				f.adj[naf++] = (Face *)face->item(af[i]-face0);
		}
		f.nadj = naf;		// save count of adjacent faces
	}

	// setup link to chain for this LOD/texture
	f.next = lod[cur_lod].face[t];

	// Discard this face if it points away from camera
	// (texture coords CW). (Still need placeholder in face array)
	Attr *a0 = (Attr *)attr->item(f.v[0]->sv[t].attr);
	Attr *a1 = (Attr *)attr->item(f.v[1]->sv[t].attr);
	Attr *a2 = (Attr *)attr->item(f.v[2]->sv[t].attr);
	if ((a0->t - a1->t) * (a2->s - a1->s) > 
			(a0->s - a1->s) * (a2->t - a1->t))
		f.v[0] = NULL;	// mark as a reject

	// store in master face array
	if (face->store(&f))
		parse_error("Face array overflow", s);

	// link into face list for this LOD/texture if not rejected
	if (f.v[0])
		lod[cur_lod].face[t] = (Face *)face->item(face->curlen()-1);
}

// write vertices to ViSTa file, assign indices, and update bounding boxes
static void write_vertices()
{
	int i, t;

	// setup bounding boxes - one overall, and one per texture
	bbox.init();
	tbox = new Summitt_range[num_textures];
	for (t=0; t<num_textures; t++)
		tbox[t].init();

	num_vertices = 0;	// current output ID, also count for header
	
	Vertex *v = (Vertex *)vert->array();
	for (i=0; i<vert->curlen(); i++, v++) {
		bbox.include(v->xyz);
		// output vertex once for each texture that uses it
		for (t=0; t<num_textures; t++) {
			if (v->sv[t].attr >= 0) {
				tbox[t].include(v->xyz);
				// assign file ID
				v->sv[t].id = num_vertices++;
				Attr *a = (Attr *)attr->item(v->sv[t].attr);
				vst_write_vertex(ofp, v->xyz, &(a->s));
			}
		}
	}

	// average bounding box side length
	bb_side = (bbox.xmax - bbox.xmin + bbox.ymax - bbox.ymin +
			bbox.zmax - bbox.zmin) / 3.0;

	if (verbose) {
		fprintf(stderr, "Wrote %d vertices\n", num_vertices);
		bbox.dump(stderr, "Overall bounding box");
	}
}

// Build triangle strips, filling in length array.
// Returns number of strips, and sets total vertex count.
int stripify(Face *flist, int striplen[], int *nverts)
{
	int ns = 0;	// strip number
	int nv = 0;	// total vertices
	Face *f;

#if 0
	// trivial version - all one-triangle strips
	for (f=flist; f; f=f->adj[0]) {
		striplen[ns++] = 3;
		nv += 3;
	}
#endif

#if 1
	// accumulate adjacent triangles into strips when valid
	Face *fn;			// next face
	for (f=flist; f; f=fn) {
		int n = 3;		// number of vertices in this strip
		fn = f->adj[0];
		while (fn && fn->v[0] == f->v[1] && fn->v[1] == f->v[2]) {
			n++;
			f = fn;
			fn = f->adj[0];
		}

		striplen[ns++] = n;
		nv += n;
	}
#endif

	*nverts = nv;
	return ns;
}

// Write patch for this LOD and this texture,
// return number of bytes written. Update max vertex file ID used.
static int write_patch(int lev, int tex, int *maxvert)
{
	int i, j;
	Face *flist = lod[lev].face[tex];

	if (flist == NULL)	// no faces at this LOD/texture?
		return 0;

	// Count faces at the LOD/texture, and optionally
	// (if requested, and input had triangle adjacency info)
	// reorder and rotate for better triangle stripping.
	// Resulting list is linked by f->adj[0], not f->next
	int nfaces = sort_faces(&flist, stripit && adjacency);

	Face *f;

	// accumulate triangles into strips
	int *striplen = new int[nfaces];	// # verts in each strip
	int nverts;				// total stripped verts
	int nstrips = stripify(flist, striplen, &nverts);

	// write patch header
	int nbytes = vst_write_tripatch_header(ofp, tex, nstrips, nverts);

	// Write array lengths
	nbytes += vst_write_ints(ofp, nstrips, striplen);

	// Write array indices
	for (f=flist, i=0; i<nstrips; i++) {	// for each strip
		// write first triangle - all vertices
		for (j=0; j<3; j++) {
			int vid = f->v[j]->sv[tex].id;
			nbytes += vst_write_int(ofp, vid);
			if (vid > *maxvert)
				*maxvert = vid;
		}
		f = f->adj[0];

		// remaining triangles - last vertex only
		for (j=3; j<striplen[i]; j++) {
			int vid = f->v[2]->sv[tex].id;
			nbytes += vst_write_int(ofp, vid);
			if (vid > *maxvert)
				*maxvert = vid;
				
			// odd strip face and needed at next LOD?
			if ((j & 1) && f->final) {
				// swap vertex order back to normal
				Vertex *t = f->v[0];
				f->v[0] = f->v[1];
				f->v[1] = t;
			}
			
			f = f->adj[0];
		}
	}

	delete [] striplen;

	// move any childless faces to next LOD for re-use
	// (back to using 'next' link)
	if (lod[lev+1].face) {
		Face *copy = NULL;	// temp list of copied faces
		Face *ctail;
		for (f=flist; f; f=f->adj[0]) {
			if (f->final) {			// move it
				if (copy == NULL)	// first one
					copy = f;
				else
					ctail->next = f;
				ctail = f;
				f->nadj = 0;		// adjacency info bogus!
			}
		}
		// add copied faces to front of next LOD's face list
		if (copy) {
			ctail->next = lod[lev+1].face[tex];
			lod[lev+1].face[tex] = copy;
		}
	}

	return nbytes;
}

// write LODs (including faces) to ViSTa file
static void write_lods()
{
	int i, t;

	// loop through requested LODs in order
	for (i=min_lod; i<=max_lod; i++) {
	
		// write dummy LOD header to allocate file space
		long start = ftell(ofp);
		int lod_bytes = vst_write_lod_header(ofp);
		int lod_patches = 0;

		// write bounding boxes for each texture
		// (same for all LODs...)
		for (t=0; t<num_textures; t++)
			lod_bytes += vst_write_bbox(ofp, &tbox[t]);
			
		// create patch for each texture that has faces at this LOD
		int maxvert = 0;
		for (t=0; t<num_textures; t++) {
			int patch_bytes = write_patch(i, t, &maxvert);
			if (patch_bytes) {
				lod_bytes += patch_bytes;
				lod_patches++;
			}
		}

		// crude logic for switch threshold, going for cutoff
		// at nominal 10-pixel triangle widths
		if (lod[i].switchin <= 0.0)
			lod[i].switchin = 57.3 * bb_side / sqrt(maxvert);

		// fixup LOD header
		fseek(ofp, start, SEEK_SET);
		vst_write_lod_header(ofp, lod_bytes, maxvert+1, 
			lod_patches, maxvert, lod[i].switchin);
		fseek(ofp, 0L, SEEK_END);
		
		if (verbose)
			fprintf(stderr, "LOD %2d: %6d bytes, %6d vertices, "
				"%2d patch(es)\n",
				i, lod_bytes, maxvert+1, lod_patches);
	}
}

// write coordinate system info
static void write_coordsys()
{
	// assuming COORDSYS_SIZE is multiple 1024
	char buf[1024];
	memset(buf, 0, sizeof(buf));
	for (int i=0; i<COORDSYS_SIZE; i+=sizeof(buf))
		fwrite(buf, 1, sizeof(buf), ofp);
}

int main(int argc, char **argv)
{
	char *infile = NULL;
	char *outfile = NULL;
	char *texext = NULL;
	int i;

	MatIdent(xform);	// default transform

	for (i=1; i<argc; i++) {
		if (!strcmp(argv[i], "-b")) {
			min_lod = atoi(argv[++i]);
		} else if (!strcmp(argv[i], "-e")) {
			max_lod = atoi(argv[++i]);
		} else if (!strcmp(argv[i], "-i")) {
			infile = argv[++i];
		} else if (!strcmp(argv[i], "-o")) {
			outfile = argv[++i];
		} else if (!strcmp(argv[i], "-g")) {
			stripit++;
		} else if (!strcmp(argv[i], "-ext")) {
			texext = argv[++i];
			// behave properly with either ".foo" or "foo"
			if (texext[0] == '.')
				texext++;
		} else if (!strcmp(argv[i], "-v")) {
			verbose++;
		} else {
			fprintf(stderr, "Unknown option %s\n", argv[i]);
			fprintf(stderr, usage, argv[0]);
			return 1;
		}	
	}
	if (!infile) {
		fprintf(stderr, "Input ASD file not specified\n");
		fprintf(stderr, usage, argv[0]);
		return 1;
	}
	if (!outfile) {
		fprintf(stderr, "Output ViSTA file not specified\n");
		fprintf(stderr, usage, argv[0]);
		return 1;
	}
	
	FILE *fp = fopen(infile, "r");
	if (fp == NULL)
		parse_error("Can't open", infile);

	if (verbose)
		fprintf(stderr, "Loading ASD file %s\n", infile);

	// read and check header line
	char buf[1024], buf2[128];
	if (fgets(buf, sizeof(buf), fp) == NULL)
		parse_error("Can't read", infile);
	if (strncmp(buf, "#ASDMesh V1.0 ascii", 19)) {
		parse_error("Missing ASD mesh header (or unsupported version)", 
			infile);
	}

	// init data structures
	cur_lod = -1;
	lod = new LOD[max_lod+1];

    //make sure output file will have proper permissions
    umask(002);
	// start output file
	ofp = fopen(outfile, "wb");
	if (ofp == NULL)
		parse_error("Can't create", outfile);
	// reserve space for header, to be filled in later
	vst_write_header(ofp);

	// parse input lines
	while (fgets(buf, sizeof(buf), fp)) {
		char *s = buf;
		while (isspace(*s))	// skip leading whitespace
			s++;
		switch (*s) {
		case 'a':	parse_attribute(s);
				break;
		case 'f':	if (adjacency)
					fgets(buf2, sizeof(buf2), fp);
				if (cur_lod >= min_lod)
					parse_face(s, buf2);
				else
					face0++;
				break;
		case 'g':	parse_global(s);
				break;
		case 'l':	parse_LOD(s);
				if (cur_lod > max_lod)
					goto INPUT_DONE;
				break;
		case 'n':	parse_alloc(s);
				break;
		case 'p':	parse_per_vertex(s);
				break;
		case 's':	parse_strip(s);
				break;
		case 't':	parse_texture(s, texext);
				break;
		case 'v':	parse_vertex(s);
				break;
		case 'x':	parse_xform(s);
				break;
		case '#':	break;
		default:
				parse_error("Unknown record type", s);
		}
	}
	// reached EOF
	max_lod = cur_lod++;

    INPUT_DONE:
	fclose(fp);

	// if didn't allocate anything, indicate failure
	if (!vert)
		parse_error("No vertices found!\n", infile);

	// write output file (already wrote dummy header, real texrefs)
	if (verbose)
		fprintf(stderr, "Finished loading, creating ViSTa file\n");

	write_coordsys();

	write_vertices();

	write_lods();
	
	// fixup file header
	rewind(ofp);
	vst_write_header(ofp, VST_SIMPLE, num_textures, num_vertices, 
				cur_lod - min_lod, &bbox);
	
	return 0;
}

// temp debug
int fnum(Face *f) { return f - (Face *)face->array(); }

