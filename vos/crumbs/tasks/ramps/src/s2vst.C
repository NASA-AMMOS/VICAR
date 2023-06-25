// s2vst.C 1.2 02/07/09 11:55:30
/** \file
* Convert Surface model meshes into ViSTa binary model.
* This function takes a series of triangular meshes in GRAPE
* surface model format, and creates a VST output file where
* each input mesh produces a corresponding level of detail.
*/

#include <math.h>
#include "summitt_func.h"
#include "vista.h"
#include "cahvor.h"

extern int verbose;

static FILE *ofp;		// VST output file
static const char *impl;	// VST implementation type
static int cam_frame;		// camera frame implementation?
static int num_vertices;	// total number of output vertices
static SfcModel **sfc;		// input surface models
static Summitt_range bbox;	// overall bounding box
static float bb_side;		// average bounding box side length
static int xres, yres;		// texture map resolution
static enum { XYZ_MAP, RH_HEIGHT_MAP, LH_HEIGHT_MAP } model_type;
static CAHVOR *ccmod;		// camera model, camera frame
static CAHVOR *wcmod;		// camera model, world/rover frame
static double (*obj2wld)[DIM_ZMAT];	// object-to-world transform
static int compact;		// produce compact output (reuse vertices)?
static int bad_texture;		// count of bogus texture coordinates

// Mark vertex as used in mesh triangles, either -1 = new vertex
// or (fileID) = vertex used at previous LOD
static void mark_vertex(int mdl, NodeSpec *ns, ZMatrix prevlod)
{
	if (ns->id != 0)	// vertex is already marked
		return;
	if (mdl > 1 && compact) {	// not at lowest detail levels
		// (note LOD 0 = terrain quad, doesn't have an octree)

		// get vertex coordinates in previous LOD's model space
		double p[3], pp[3];
		ns->get_global_center(p);
		MultPoints(p, prevlod, pp);

		// look for vertex in previous LOD's octree
		NodeSpec *nsp;
		double d2 = sfc[mdl-1]->get_data()->fast_find_closest(
				sfc[mdl-1]->get_max_levels(), FLT_MAX,
				pp, pp, &nsp);
		// check distance squared in model space
		if (d2 < 1.0E-8) {	// found same point
			ns->id = nsp->id;
			return;
		}
	}
	ns->id = -1;		// new vertex used in mesh
}

// write vertices to ViSTa file, assign indices, and update bounding boxes
static void write_vertices(int mdl)
{
	// make sure surface has extracted point list
	if (sfc[mdl]->mesh.pt_count == 0)
		sfc[mdl]->add_pt_list();

	// initialize all the vertex ID's to zero = unused
	NodeSpec_List_Element *plist;
	for (plist = sfc[mdl]->mesh.pt_list; plist; plist = plist->next)
		plist->nodespec->id = 0;

	// get transform from model space to previous LOD's model space
	ZMatrix xform, prevlod;
	sfc[mdl]->GetModelToObjectTransform(xform);
	if (mdl > 1) {
		sfc[mdl-1]->GetObjectToModelTransform(prevlod);
		MatPreMult(prevlod, xform);
	}

	// Mark vertices used by triangles (id = -1).
	// Vertices may have been used in a previous LOD;
	// if so, mark them with the already-output file ID instead.
	for (Mesh_Triangle *f=sfc[mdl]->mesh.tri_list; f; f=f->next) {
		mark_vertex(mdl, f->get_vertex_1(), prevlod);
		mark_vertex(mdl, f->get_vertex_2(), prevlod);
		mark_vertex(mdl, f->get_vertex_3(), prevlod);
	}

	// Now output the new vertices used at this LOD

	if (!cam_frame) 	// xform = model to world
		MatPostMult(xform, obj2wld);
	CAHVOR *pcmod = cam_frame ? ccmod : wcmod;

	for (plist = sfc[mdl]->mesh.pt_list; plist; plist = plist->next) {
		NodeSpec *ns = plist->nodespec;
		if (ns->id >= 0)	// not used or already output
			continue;
			
		double p[3], po[3];
		ns->get_global_center(p);
		// convert to output space
		MultPoints(p, xform, po);
		float pf[3], s[2];
		// convert to floats, reorient axes
		if (cam_frame) {
			pf[0] = -po[1];
			pf[1] = po[0];
			pf[2] = po[2];
		} else {		// rover frame
			pf[0] = po[0];
			pf[1] = po[1];
			pf[2] = po[2];
		}
		bbox.include(pf);

		// compute texture coords
		if (model_type == XYZ_MAP) {
			// use camera model to project 3D point to image
			double st[2];
			pcmod->To_2D(po, st);
			// convert double -> float, pixel->fraction
			s[0] = st[0] / xres;	
			s[1] = st[1] / yres;
			// check for bogus XYZ/camera model
			// (but allow bad texture in projected 
			// LOD 0 terrain // quad vertices)
			if (verbose && mdl > 0) {	
				if (s[0] < 0.0 || s[0] > 1.0 || 
				    s[1] < 0.0 || s[1] > 1.0)
					bad_texture++;
			}
		} else if (model_type == RH_HEIGHT_MAP) {
			// object coords = pixel index+0.5
			// map (0.5,0.5) to (0,0)
			// and (xres+0.5, yres+0.5) to (1,1)
			s[0] = (po[0] - 0.5) / xres;
			s[1] = (po[1] - 0.5) / yres;
		} else {			// LH height map
			// object coords = pixel index+0.5
			// map (0.5,yres-0.5) to (0,0)
			// and (xres+0.5, -0.5) to (1,1)
			s[0] = (po[0] - 0.5) / xres;
			s[1] = (yres - 0.5 - po[1]) / yres;
		}

		ns->id = num_vertices++;	// assign file ID
		vst_write_vertex(ofp, pf, s);
	}
}

// Is f1-f2 a valid initial triangle strip?
// May reorder f1 and f2 vertices.
static int strip0(Mesh_Triangle *f1, Mesh_Triangle *f2)
{
	NodeSpec *a = f1->get_vertex_1();
	NodeSpec *b = f1->get_vertex_2();
	NodeSpec *c = f1->get_vertex_3();

	NodeSpec *d = f2->get_vertex_1();
	NodeSpec *e = f2->get_vertex_2();
	NodeSpec *f = f2->get_vertex_3();

	if (a == d) {
		if (b == f) {		// shared edge ab and fd
			f1->reset(c, a, b);
			f2->reset(a, b, e);
		} else if (c == e) {	// shared edge is ca and de
			f1->reset(b, c, a);
			f2->reset(c, a, f);
		} else {
			return 0;
		}
	} else if (a == e) {
		if (b == d) {		// shared edge is ab and de
			f1->reset(c, a, b);
			f2->reset(a, b, f);
		} else if (c == f) {	// shared edge is ca and ef
			f1->reset(b, c, a);
			f2->reset(c, a, d);
		} else {
			return 0;
		}
	} else if (a == f) {
		if (b == e) {		// shared edge is ab and ef	
			f1->reset(c, a, b);
			f2->reset(a, b, d);
		} else if (c == d) {	// shared edge is ca and fd
			f1->reset(b, c, a);
			f2->reset(c, a, e);
		} else {
			return 0;
		}
	} else if (b == d && c == f) {	// shared edge is bc and fd
		f2->reset(b, c, e);
	} else if (b == e && c == d) {	// shared edge is bc and de
		f2->reset(b, c, f);
	} else if (b == f && c == e) {	// shared edge is bc and ef
		f2->reset(b, c, d);
	} else {
		return 0;		// no edge shared
	}
	return 1;
}

// Is f2 a valid strip triangle after f1?
// May reorder f2 vertices.
static int strip(Mesh_Triangle *f1, Mesh_Triangle *f2)
{
	NodeSpec *b = f1->get_vertex_2();
	NodeSpec *c = f1->get_vertex_3();

	NodeSpec *d = f2->get_vertex_1();
	NodeSpec *e = f2->get_vertex_2();
	NodeSpec *f = f2->get_vertex_3();

	if (b == d) {
		if (c == e)		// bcf=def
			return 1;	// f2 in correct order
		else if (c == f)	// bce=dfe
			f2->reset(b, c, e);
		else
			return 0;
	} else if (b == e) {
		if (c == d)		// bcf=edf
			f2->reset(b, c, f);
		else if (c == f)	// bcd=efd
			f2->reset(b, c, d);
		else
			return 0;
	} else if (b == f) {
		if (c == d)		// bce=fde
			f2->reset(b, c, e);
		else if (c == e)	// bcd=fed
			f2->reset(b, c, d);
		else
			return 0;
	} else {
		return 0;
	}

	return 1;
}		

// Build triangle strips, filling in length array.
// Returns number of strips, and sets total vertex count.
// ** Currently reorders vertices but not faces - assuming the faces
// ** are already in a good order for stripping.
static int stripify(Mesh_Triangle *flist, int striplen[], int *nverts)
{
	int ns = 0;	// strip number
	int nv = 0;	// total vertices
	Mesh_Triangle *f;

	// accumulate adjacent triangles into strips when valid
	Mesh_Triangle *fn;		// next face
	for (f=flist; f; f=fn) {
		fn = f->next;
		int n = 3;	// number of vertices in this strip
		if (fn && strip0(f, fn)) {
			do {
				n++;
				f = fn;
				fn = f->next;
			} while (fn && strip(f, fn));
		}

		striplen[ns++] = n;
		nv += n;
	}

	*nverts = nv;
	return ns;
}

// Write patch for this LOD,
// return number of bytes written. Update max vertex file ID used.
static int write_patch(int lev, int *maxvert, int *num_faces)
{
	int i, j;
	Mesh_Triangle *flist = sfc[lev]->mesh.tri_list;
	Mesh_Triangle *f;

	// count the faces for this LOD
	int nfaces = 0;
	for (f=flist; f; f=f->next)
		nfaces++;

	// accumulate triangles into strips
	int *striplen = new int[nfaces];	// # verts in each strip
	int nverts;				// total stripped verts
	int nstrips = stripify(flist, striplen, &nverts);

	// write patch header
	int nbytes = vst_write_tripatch_header(ofp, 0, nstrips, nverts);

	// Write array lengths
	nbytes += vst_write_ints(ofp, nstrips, striplen);

	// Write array indices
	for (f=flist, i=0; i<nstrips; i++) {	// for each strip
		// write first triangle - all vertices
		int v1 = f->get_vertex_1()->id;
		int v2 = f->get_vertex_2()->id;
		int v3 = f->get_vertex_3()->id;
		nbytes += 3*vst_write_int(ofp, v1);
		vst_write_int(ofp, v2);
		vst_write_int(ofp, v3);
		if (v1 > *maxvert) *maxvert = v1;
		if (v2 > *maxvert) *maxvert = v2;
		if (v3 > *maxvert) *maxvert = v3;

		f = f->next;

		// remaining triangles - last vertex only
		for (j=3; j<striplen[i]; j++) {
			int vid = f->get_vertex_3()->id;
			nbytes += vst_write_int(ofp, vid);
			if (vid > *maxvert)
				*maxvert = vid;
			f = f->next;
		}
	}

	delete [] striplen;
	*num_faces = nfaces;
	return nbytes;
}

// write one LOD (including faces) to ViSTa file -
// one texture, one patch
static void write_lod(int i)
{
	// write dummy LOD header to allocate file space
	long start = ftell(ofp);
	int lod_bytes = vst_write_lod_header(ofp);

	// write bounding box
	lod_bytes += vst_write_bbox(ofp, &bbox);
		
	// create patch
	int maxvert = 0;
	int nfaces;
	int patch_bytes = write_patch(i, &maxvert, &nfaces);
	lod_bytes += patch_bytes;

	// fixup LOD header
	float switchin = 57.3 * bb_side / sqrt(nfaces);
	fseek(ofp, start, SEEK_SET);
	vst_write_lod_header(ofp, lod_bytes, maxvert+1, 
		1, maxvert, switchin);
	fseek(ofp, 0L, SEEK_END);
		
	if (verbose)
		fprintf(stderr, "LOD %2d: %10d bytes, %10d vertices\n",
			i, lod_bytes, maxvert+1);
}

// Convert list of surface models (lowest-detail first) into a 
// ViSTa model, written to outfile.
//
// impl = VST implementation ID (e.g. "MER")
// texfile = name of texture image, texext = optional replacement extension.
// txres/tyres = texture map resolution (don't need to load actual image data).
// FOV = XYZ map horizontal field of view in radians, or zero for RH
// height map or -1 for LH height map.
// xform = object (camera) to world transform
// ccmod = camera model in camera coordinate frame
// wcmod = camera model in world (rover) coordinate frame
// compact_verts = remove redundant vertices (takes longer but produces
//	smaller output file)
// Returns zero if no problems.
int summitt_sfc2vst(const char *vst_impl,
	int num_models, SfcModel *sfc_list[], char *outfile,
	char *texfile, char *texext, int txres, int tyres,
	double fov, ZMatrix xform, CAHVOR *pccmod, CAHVOR *pwcmod, 
		int compact_verts)
{
	// copy args to local variables
	impl = vst_impl;
	sfc  = sfc_list;
	xres = txres;
	yres = tyres;
	ccmod = pccmod;
	wcmod = pwcmod;
	obj2wld = xform;
	compact = compact_verts;

	// is this camera frame (not world/rover frame) VST implementation?
	if (!strcmp(impl, VST_MER))
		cam_frame = 1;

	// texture mapping setup
	if (fov > 0.0) {
		model_type = XYZ_MAP;
	} else if (fov < 0.0) {
		model_type = LH_HEIGHT_MAP;
	} else {
		model_type = RH_HEIGHT_MAP;
	}

	// start output file
    //make sure output file will have proper permissions
    umask(002);
	ofp = fopen(outfile, "wb");
	if (ofp == NULL) {
		fprintf(stderr, "sfc2vst: Can't create %s\n", outfile);
		return 1;
	}

	// reserve space for header, to be filled in later
	vst_write_header(ofp);

	// store texture name, possibly tweaked
	if (texext) {
		static char name[1024];
		strcpy(name, texfile);
		char *dot = strrchr(name, '.');
		if (!dot) {
			dot = name + strlen(name);
			*dot = '.';
		}
		if (texext[0] == '.')
			texext++;
		strcpy(dot+1, texext);
		texfile = name;
	}
	vst_write_texture(ofp, texfile);

	// write coordinate system - MER = world cam model, else filler
	if (!strcmp(impl, VST_MER))
		vst_write_coordsys(ofp, wcmod->c, wcmod->a, wcmod->h, wcmod->v);
	else
		vst_write_coordsys(ofp);

	// write vertices, update bounding box
	bbox.init();
	num_vertices = 0;
	bad_texture = 0;
	int i;
	for (i=0; i<num_models; i++)
		write_vertices(i);
	if (verbose) {
		fprintf(stderr, "Wrote %d vertices\n", num_vertices);
		if (bad_texture)
			fprintf(stderr, "%d texture coordinates invalid\n",
				bad_texture);
		bbox.dump(stderr, "Overall bounding box");
	}
        // average bounding box side length
	bb_side = (bbox.xmax - bbox.xmin + bbox.ymax - bbox.ymin +
			bbox.zmax - bbox.zmin) / 3.0;

	// write LODs, one per input model
	for (i=0; i<num_models; i++)
		write_lod(i);
	
	// fixup file header
	rewind(ofp);
	vst_write_header(ofp, impl, 1, num_vertices, num_models, &bbox);
	return fclose(ofp);
}
