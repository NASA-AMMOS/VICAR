// tileout.C 1.7 03/10/14 13:07:12
// Output triangle meshes for multi-LOD tile format, using Inventor
//
// Clips triangles rougly to tile border - currently very crude version.
//
// Triangle strip logic is disabled. Although it makes the Inventor
// files smaller, it interferes with Performer converter's smarter
// stripping logic, so the PFB files come out much smaller and better-
// stripped if I don't do it here. Also, somewhere I was producing
// occasional backwards triangles...
//define TRI_STRIPS
#define TEX_PER_VERTEX

#include <math.h>
#include <sys/types.h>
#include <time.h>
#include "summitt_func.h"
#define GLOBAL extern
#include "tilemesh.h"

// transform from model to object/site coordinates
static ZMatrix xform;

extern int zdown;	// world frame has +Z pointing down?
static int flip_tris;	// flip triangle directions?

// Clip triangles to tile boundary.
// ** initial stupid version.
// Uses nodespec ID field: bit 0: 0=outside tile, 1=inside tile
//                         bit 1: 0=not used in mesh, 1=used in mesh
// Clipped triangles have nodespecs set to NULL (simpler than unlinking,
// maybe?)
static void clip_triangles(Triangle_Model *mesh, Range *bvol)
{
	NodeSpec_List_Element *nle;
	Mesh_Triangle *tri;

	// first tag vertices as inside or outside, and as not used yet
	for (nle = mesh->pt_list; nle; nle = nle->next) {
		// transform point to site coordinates for clipping
		double gc[3], sc[3];
		nle->nodespec->get_global_center(gc);
		MultPoints(gc, xform, sc);
		nle->nodespec->id = bvol->in_range(sc) ? 1 : 0;
	}
	
	// now test each triangle
	int nclip = 0;		// diagnostic
	for (tri = mesh->tri_list; tri; tri = tri->next) {
		NodeSpec *ns1 = tri->get_vertex_1();
		NodeSpec *ns2 = tri->get_vertex_2();
		NodeSpec *ns3 = tri->get_vertex_3();
		// ** keep triangle if at least one vertex is inside tile
		if ((ns1->id & 1) || (ns2->id & 1) || (ns3->id & 1)) {
			ns1->id |= 2;	// triangle okay, mark vertices as used
			ns2->id |= 2;
			ns3->id |= 2;
		} else {		// toss it (mark as clipped)
			tri->reset(NULL, NULL, NULL);
			nclip++;
		}		
	}

	if (verbose)
		fprintf(stderr, "%d triangles clipped\n", nclip);
}

// Write vertex list for entire LOD (in site frame)
static void write_vertices(FILE *fp, Triangle_Model *mesh)
{
	//fprintf(fp, "Coordinate3 {point [\n");
	int vindex = -1;
	NodeSpec_List_Element *nle;
	for (nle = mesh->pt_list; nle; nle = nle->next) {
		if (nle->nodespec->id & 2) {	// only if used by mesh
			nle->nodespec->id = ++vindex;	// save for face index
			double gc[3], sc[3];
			nle->nodespec->get_global_center(gc);
			MultPoints(gc, xform, sc);
			fprintf(fp,"v %f %f %f\n", sc[0], sc[1], sc[2]);
		}
	}
	//fprintf(fp, "]}\n");
}

// Compute texture coordinates for a vertex ns using given patch.
// output is st[] (in range 0-1)
static void vertex_texture(NodeSpec *ns, PInfo *pi, double st[2])
{
	// get coordinates in patch's camera frame
	double gc[3], cc[3];
	ns->get_global_center(gc);
	MultPoints(gc, pi->m2cam, cc);

	// use camera model to project 3D point to image
	pi->p->cmod.To_2D(cc, st);

	// convert pixel->fraction, deal with left-handedness
	st[0] /= pi->p->xres;
	st[1] = 1.0 - st[1] / pi->p->yres;
}

//no stripping
#define stripify(list)
#define is_strip(f1, f2) FALSE

// Output triangle strips for one patch.
// Also update average edge info.
// If this mesh was created by marching triangles, they always 
// face +Z, so reverse first tri of each strip if necessary.
static void write_patch(FILE *fp, PInfo *pi, Mesh_Triangle *tri_list)
{
	stripify(tri_list);

        //!!!! ozp
	//fprintf(fp, "Texture2 {filename \"%s.rgb\" "
	//		"wrapS CLAMP wrapT CLAMP}\n", pi->p->get_name());
        //fprintf(fp, "USE Texture_%s\n", pi->p->get_name());

	// write texture coords, one per strip vertex
	//fprintf(fp, "TextureCoordinate2 { point [\n");
	Mesh_Triangle *t, *tn;
	for (t = tri_list; t; t = tn) {
		tn = t->next;
		NodeSpec *ns1 = t->get_vertex_1();
		if (ns1 == NULL)	// skip if clipped
			continue;
			
		// first triangle in strip - 3 vertices
		double st[6];
		vertex_texture(ns1, pi, st+0);
		if (flip_tris) {
			vertex_texture(t->get_vertex_3(), pi, st+2);
			vertex_texture(t->get_vertex_2(), pi, st+4);
		} else {
			vertex_texture(t->get_vertex_2(), pi, st+2);
			vertex_texture(t->get_vertex_3(), pi, st+4);
		}
		fprintf(fp, "vt %.3f %.3f\nvt %.3f %.3f\nvt %.3f %.3f\n",
			st[0], st[1], st[2], st[3], st[4], st[5]);
		//fprintf(fp, "vt %.3f %.3f\n", st[0], st[1]);

		for (; tn && is_strip(t, tn); tn = t->next) {
			// next triangle in strip - 1 vertex
			vertex_texture(tn->get_vertex_3(), pi, st);
			//fprintf(fp, "vt %.3f %.3f,\n", st[0], st[1]);
			t = tn;
		}
	}
	//fprintf(fp, "]}\n");

	// write triangle strips
	// ** if not doing strips, maybe shouldn't be "strip set"?
	//fprintf(fp, "IndexedTriangleStripSet {coordIndex [\n");
        long tc = 1;
	for (t = tri_list; t; t = tn) {
		tn = t->next;
		NodeSpec *ns1 = t->get_vertex_1();
		if (ns1 == NULL)	// skip if clipped
			continue;

		// first triangle in strip - 3 vertices
		NodeSpec *ns2, *ns3;
		if (flip_tris) {
			ns2 = t->get_vertex_3();
			ns3 = t->get_vertex_2();
		} else {
			ns2 = t->get_vertex_2();
			ns3 = t->get_vertex_3();
		}
		// vertex coordinate indices saved in nodespec id field
		//fprintf(fp,"f %d,%d,%d,", (ns1->id)+1, (ns2->id)+1, (ns3->id)+1);
		fprintf(fp,"f %d/%d/ %d/%d/ %d/%d/\n", (ns1->id)+1, tc, (ns2->id)+1, tc+1, (ns3->id)+1, tc+2);
                tc = tc + 3;

		for (; tn && is_strip(t, tn); tn = t->next) {
			// next triangle in strip - 1 vertex
			ns3 = tn->get_vertex_3();
			//fprintf(fp, "%d,", ns3->id);
			t = tn;
		}

		//fprintf(fp, "-1,\n");	// end of strip
	}

#ifndef TEX_PER_VERTEX
	// performer's inventor loader doesn't handle 
	// per-vertex binding of texture coordinates correctly, 
	// so we need to provide an explicit texcoord index list :(
	//fprintf(fp, "]\n textureCoordIndex [\n");
	int tci = 0;
	for (t = tri_list; t; t = tn) {
		tn = t->next;
		if (t->get_vertex_1() == NULL)	// skip if clipped
			continue;

		// first triangle in strip - 3 vertices
		fprintf(fp, "f %d,%d,%d\n", tci, tci+1, tci+2);
		tci += 3;

		for (; tn && is_strip(t, tn); tn = t->next) {
			// next triangle in strip - 1 vertex
			//fprintf(fp, "%d,", tci++);
			t = tn;
		}

		//fprintf(fp, "-1,\n");	// end of strip
	}
#endif

	//fprintf(fp, "]}\n");
}

// Get patch number that a NodeSpec (triangle vertex) came from.
// The voxel color red component is used to track this.
static int vertex_source(NodeSpec *ns)
{
	int r, g, b;
	ns->get_color(&r, &g, &b);
	return r;
}

// Get cosine of view angle to face for given patch
static double view_cos(int pnum, double vertex[3], double face_normal[3])
{
	double cpos[3];		// patch camera position
	cpos[0] = plist[pnum].p->x;
	cpos[1] = plist[pnum].p->y;
	cpos[2] = plist[pnum].p->z;

	double viewdir[3];	// unit vector from camera to vertex
	get_vector(cpos, vertex, viewdir);

	return dot_product(viewdir, face_normal);
}

// 2D distance
static double dist2d(double a[2], double b[2])
{
	double dx = a[0] - b[0];
	double dy = a[1] - b[1];
	return sqrt(dx*dx + dy*dy);
}

// Compute squared area of face in 2D texture space (using Heron's forumula).
// Return -1 if face is at least partly outside the patch's view frustrum.
static double texture_area(Mesh_Triangle *tri, int pnum)
{
	double st1[2], st2[2], st3[2];
	PInfo *pi = &plist[pnum];
	vertex_texture(tri->get_vertex_1(), pi, st1);
	if (st1[0] < 0.0 || st1[0] > 1.0 || st1[1] < 0.0 || st1[1] > 1.0)
		return -1.0;
	vertex_texture(tri->get_vertex_2(), pi, st2);
	if (st2[0] < 0.0 || st2[0] > 1.0 || st2[1] < 0.0 || st2[1] > 1.0)
		return -1.0;
	vertex_texture(tri->get_vertex_3(), pi, st3);
	if (st3[0] < 0.0 || st3[0] > 1.0 || st3[1] < 0.0 || st3[1] > 1.0)
		return -1.0;

	double a = dist2d(st2, st3);
	double b = dist2d(st1, st3);
	double c = dist2d(st1, st2);
	double s = 0.5 * (a + b + c);
	return (s * (s-a) * (s-b) * (s-c)) * pi->p->xres * pi->p->yres;
}

// Choose best patch for texturing a triangle
static int choose_texture(Mesh_Triangle *tri)
{
	// get source patch for each vertex
	NodeSpec *ns1 = tri->get_vertex_1();
	if (ns1 == NULL)			// clipped
		return 0;
	int src1 = vertex_source(ns1);
	int src2 = vertex_source(tri->get_vertex_2());
	int src3 = vertex_source(tri->get_vertex_3());

	// how many unique source patches?
	int nsource = 3;
	if (src3 == src1 || src3 == src2)	// 3 is duplicate
		nsource--;
	if (src2 == src1) {			// 2 is duplicate
		nsource--;
		src2 = src3;			// shift over
	}
	// if all came from same patch, that's our guy
	if (nsource == 1)
		return src1;

	// compute score for each source, larger = better choice
	double score1, score2, score3;

	// get triangle surface normal
	double face_normal[3];
	double v1[3], v2[3], v3[3];
	tri->get_vertex_1()->get_global_center(v1);
	tri->get_vertex_2()->get_global_center(v2);
	tri->get_vertex_3()->get_global_center(v3);

	surface_normal(v1, v2, v3, face_normal);

	// larger cosine of view angle = better score
	score1 = view_cos(src1, v1, face_normal);
	score2 = view_cos(src2, v1, face_normal);
	if (nsource == 3)
		score3 = view_cos(src3, v1, face_normal);
	else
		score3 = -2.0;

	// larger surface area in texture space = better score
	// (outside of view frustrum = bad)
	if (score1 > 0.0)
		score1 *= texture_area(tri, src1);
	if (score2 > 0.0)
		score2 *= texture_area(tri, src2);
	if (score3 > 0.0)
		score3 *= texture_area(tri, src3);

	// choose the best result
	if (score1 >= score2)
		return (score1 >= score3) ? src1 : src3;
	return (score2 >= score3) ? src2 : src3;
}

// Output one LOD mesh node, Inventor ASCII format.
// Triangles outside the tile volume are discarded.
void write_mesh_obj(FILE *fp, Triangle_Model *mesh, SfcModel *sfc, 
			Range *bvol, int flip)
{
	// need to flip triangle directions? (save local copy)
	flip_tris = flip;
	
	// get transform to world/site frame
	sfc->GetModelToObjectTransform(xform);

	// get transforms from merged model to each patch's camera frame
	// (for texture mapping)
	int i;
	for (i=0; i<npatch; i++) {
		ZMatrix o2w;
		plist[i].p->GetObjToWorldTransform(o2w);
		MatInvert(o2w, plist[i].m2cam);	// world/site to obj/camera
		MatPreMult(plist[i].m2cam, xform);	// model to camera

		plist[i].tri = NULL;		// also init mesh list
	}

	// Clip triangles and mark which vertices are needed
	clip_triangles(mesh, bvol);

	// output LOD vertices, may be used by multiple subnodes of the LOD
	write_vertices(fp, mesh);

	// if only one patch, all faces get that patch's texture
	if (npatch == 1) {
		//fprintf(fp, "DEF _%s Separator {\n", plist[0].p->get_name());
		write_patch(fp, &plist[0], mesh->tri_list);
		//fprintf(fp, "}\n");

	// else split mesh triangle list into one list per patch,
	// based on best patch for texturing the triangle
	} else {
		Mesh_Triangle *t, *tnext;
		for (t = mesh->tri_list; t; t = tnext) {
			tnext = t->next;	// keep place in list
			// choose best patch to texture this triangle
// ** for now, choose() returns 0 for clipped tris, which is okay
// ** except if all of patch 0's tris are clipped, we'll output an
// ** empty node...
			int patch_num = choose_texture(t);
			// move triangle to that patch's triangle list
			t->next = plist[patch_num].tri;
			plist[patch_num].tri = t;
		}

		// disconnect original list
		mesh->tri_list = NULL;

		// output one node for each input patch
		for (i=0; i<npatch; i++) {
			t = plist[i].tri;
			if (t) {
				//fprintf(fp, "DEF _%s Separator {\n", 
						//plist[i].p->get_name());
				write_patch(fp, &plist[i], t);
				//fprintf(fp, "}\n");

				// clean up split list
				while (t) {
					tnext = t->next;
					delete t;
					t = tnext;
				}					
			}

		}
	}
}
