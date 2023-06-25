// multitex.C 1.9 03/05/01 07:52:02
/** \file
* Build Inventor-format file of merged terrain model with
* multiple texture maps
*/
#include <float.h>
#include "grape/tmesh.h"
#include "grape/sfcmodel.h"
#include "forest.h"

static const char usage[] = 
"Usage: %s [-v] -i mesh_file -f forest_file [-o output_inventor_file]\n"
"  [-n norm_threshold] [-p opt] [-c color_dist]\n"
"  tex_file[:fov] [tex_file[:fov] ...]\n\n"
"Builds texture-mapped Inventor model from an obj_file corresponding\n"
"to an octree forest.\n\n"
"-v = verbose output\n"
"-i = merged surface model (octree mesh or wavefront .obj)\n"
"-f = octree forest file (for object->world transforms)\n"
"-o = output inventor file (default = standard output)\n"
"-n = max view normal angle in degrees to accept texture (default=90)\n"
"-p = handling for partially-covered and back-facing triangles:\n"
"     opt=0: ignore (default), opt=1: use model color,\n"
"     opt=2: extrapolate texture\n"
"-c = max delta for RGB component between texture and model color;\n"
"     0=ignore model color (default), <0=discard if delta > -color_dist\n"
"     >0=use model color if delta > color_dist\n"
"tex_files = texture images, in same order as forest models;\n"
":fov = corresponding model's horizontal field-of-view in degrees,\n"
"needed if model is range map (else model is assumed to be height map).\n";

#define RGB_MAGIC 0732

struct Vertex {	// merged model vertex data
	double xyz[3];	// coordinates in base object's model coords
	uchar r, g, b;	// model color
	float *u;	// texture coords for each forest model
	float *v;
#ifdef TEXSIZE
	float *ts;	// texture size at vertex for each model
#endif
	int id;		// file ID
};

struct Face {	// merged model face
	Vertex *v[3];	// vertex pointer
	int mdl;	// selected surface model index; -1=reject, -2=mdl clr
};

struct Model {	// component model data
	ObjNode node;	// for transforms
	char *name;	// model filename
	Image tex;	// texture image
	int xres, yres;	// dimensions of texture map
	double fov;	// range map camera horizontal field of view (degrees),
			// or zero if this is a height map
	double eye[3];	// eye/camera position in base surface model coords
};

static char *progname;
extern int verbose;

int num_model;		// number of models in forest
Model *model;		// component models
char **texname;		// texture filenames
int num_vertex;		// number of vertices
Vertex *vertex;		// merged model vertices
int num_face;		// number of triangle faces
Face *face;		// merged model triangle faces 
double sx, sy;		// fov texture scale factors for current range model
double cos_threshold = 0.0;	// cosine view normal angle threshold
int extrapolate;	// extrapolate option
int have_color;		// have model color data? (loaded .sfc, not .obj)
int color_dist;		// color distance option (model vs. texture color)
int partial_reject;	// count partially-covered faces discarded
int color_reject;	// count color-delta faces discarded

// load model i's texture map
static void get_tex(int i)
{
	Model *m = &model[i];
	m->tex.read(texname[i]);
	if (m->tex.get_res(&m->xres, &m->yres) > 0)
		return;
	
	fprintf(stderr, "%s: Can't load texture %s\n",
		progname, texname[i]);
	exit(1);
}

// Load info from forest file and models specified in forest file
// (Not using forest parse_in, as we don't want to load all the
// model data)
// ** CAN FIX THIS TO USE NEW parse_in(fp, FALSE)!
static void load_forest(char *name)
{
	int i;

	FILE_Dataport fp;
	if (!fp.ropen(name)) {
		fprintf(stderr, "%s: Can't open forest file %s\n",
			progname, name);
		exit(1);
	}

	char token[4096];
	// check file header
	if (!get_next_token(&fp, token) || strcmp(token, "GRP_V1")) {
		fprintf(stderr, "%s: Invalid forest file %s\n",
			progname, name);
		exit(1);
	}
	// skip CENTROID
	for (i=0; i<10; i++)
		get_next_token(&fp, token);
		
	// check number of children
	if (!get_next_token(&fp, token) || strcmp(token, "CHILDREN")) {
		fprintf(stderr, "%s: Invalid forest file %s\n",
			progname, name);
		exit(1);
	}
	get_next_token(&fp, token);
	i = atoi(token);
	if (i != num_model) {
		fprintf(stderr, "%s: Wrong number of texture files"
			" on command line\n"
			"(got %d, but forest %s has %d models)\n",
			progname, num_model, name, i);
		exit(1);
	}

	// okay, allocate model list
	model = new Model[num_model];
	for (i=0; i<num_model; i++) {
		if (!get_next_token(&fp, token) || (strcmp(token, "OBJECT") 
					&& strcmp(token, "GEOOBJECT"))) {
			fprintf(stderr, 
				"%s: Unexpected EOF in forest file %s\n",
				progname, name);
			exit(1);
		}
		get_next_token(&fp, token);	// skip name
		get_next_token(&fp, token);	// get model name
		model[i].name = strdup(token);
		model[i].node.x.parse_in(&fp);	// get world transform
		model[i].node.y.parse_in(&fp);
		model[i].node.z.parse_in(&fp);
		model[i].node.xrot.parse_in(&fp);
		model[i].node.yrot.parse_in(&fp);
		model[i].node.zrot.parse_in(&fp);
		model[i].node.xscale.parse_in(&fp);
		model[i].node.yscale.parse_in(&fp);
		model[i].node.zscale.parse_in(&fp);
		
		// while we're on this model, get fov and texture size
		char *c = strrchr(texname[i], ':');
		if (c) {
			model[i].fov = atof(c+1);
			*c = 0;			// remove :fov
		} else {
			model[i].fov = 0.0;	// indicate height model
		}
		get_tex(i);
		if (verbose)
			fprintf(stderr, "Model %d: xres=%d yres=%d fov=%f\n",
				i, model[i].xres, model[i].yres, model[i].fov);
	}
	fp.close();

	// get transform info from base model file
	if (!fp.ropen(model[0].name)) {
		fprintf(stderr, "%s: Can't open base model file %s\n",
			progname, model[0].name);
		exit(1);
	}

	// skip [SFC_MODEL and] OCTREE_V1 header tokens
	get_next_token(&fp, token);
	if (!strcmp(token, "SFC_MODEL_V1"))
		get_next_token(&fp, token);

	// get centroid = object transform
	Obj *obj = new Obj;
	if (!obj->parse_in(&fp))
		exit(1);
	model[0].node.set_object(obj);
	fp.close();
}

// Load mesh from octree surface model
static void load_sfc_model(FILE *fp)
{
	// load file data
	FILE_Dataport dp;
	dp.open(fp);
	SfcModel sfc;
	if (!sfc.parse_in(&dp))
		exit(1);

	// allocate vertex list
	num_vertex = sfc.mesh.pt_count;
	vertex = (Vertex *)malloc(num_vertex * sizeof(Vertex));
	if (vertex == NULL) {
		fprintf(stderr, "%s: Can't allocate vertex data\n",
			progname);
		exit(1);
	}
	
	// extract vertex data from mesh point list
	int i;
	NodeSpec_List_Element *nle;
	Vertex *v = vertex;
	for (i=0, nle=sfc.mesh.pt_list; nle; i++, nle=nle->next, v++) {
		nle->nodespec->id = i;	// to link faces
		nle->nodespec->get_global_center(v->xyz);

		int r, g, b;
		nle->nodespec->get_color(&r, &g, &b);
		v->r = r;
		v->g = g;
		v->b = b;
	}

	// allocate face list
	Mesh_Triangle *tmp = sfc.mesh.tri_list;
	for (num_face=0; tmp; tmp=tmp->next)
		num_face++;
	face = (Face *)malloc(num_face * sizeof(Face));
	if (face == NULL) {
		fprintf(stderr, "%s: Can't allocate %d face data\n",
			progname, num_face);
		exit(1);
	}
	
	// extract face data from mesh triangle list
	Face *f = face;
	for (i=0, tmp=sfc.mesh.tri_list; tmp; i++, tmp=tmp->next, f++) {
		f->v[0] = &vertex[tmp->get_vertex_1()->id];
		f->v[1] = &vertex[tmp->get_vertex_2()->id];
		f->v[2] = &vertex[tmp->get_vertex_3()->id];
	}	
}

// Load mesh from wavefront .obj file (first line already read in)
// (assuming all faces are triangles).
// No model color data!
static void load_obj_file(FILE *fp, char *buf)
{
	// load vertices
	int alloc = 32768;	// initial allocation
	vertex = (Vertex *)malloc(alloc * sizeof(Vertex));
	if (vertex == NULL) {
		fprintf(stderr, "%s: Can't allocate vertex data\n",
			progname);
		exit(1);
	}
	while (buf[0] != 'f') {
		double x, y, z;
		if (sscanf(buf, "v %lf %lf %lf", &x, &y, &z) == 3) {
			// need to increase allocation?
			if (num_vertex >= alloc) {
				alloc *= 2;
				if (verbose)
					fprintf(stderr, "Increasing vertex"
						" alloc to %d\n", alloc);
				vertex = (Vertex *)realloc(vertex, 
					alloc*sizeof(Vertex));
				if (vertex == NULL) {
					fprintf(stderr, "%s: Can't realloc"
						"%d vertices\n", 
						progname, alloc);
					exit(1);
				}
			}
			vertex[num_vertex].xyz[0] = x;
			vertex[num_vertex].xyz[1] = y;
			vertex[num_vertex].xyz[2] = z;
			num_vertex++;
		}
		if (!fgets(buf, 1024, fp))
			break;
	}

	// load faces
	alloc *= 2;	// initial allocation = 2 faces per vertex
	face = (Face *)malloc(alloc * sizeof(Face));
	if (face == NULL) {
		fprintf(stderr, "%s: Can't allocate %d faces\n",
			progname, alloc);
		exit(1);
	}

	do {	// (already read first 'face' line)
		int a, b, c;
		if (sscanf(buf, "f %d %d %d", &a, &b, &c) == 3) {
			// need to increase allocation?
			if (num_face >= alloc) {
				alloc = (alloc * 3) / 2;
				if (verbose)
					fprintf(stderr, "Increasing face"
						" alloc to %d\n", alloc);
				face = (Face *)realloc(face, 
						alloc*sizeof(Face));
				if (face == NULL) {
					fprintf(stderr, "%s: Can't realloc"
						"%d faces\n", 
						progname, alloc);
					exit(1);
				}
			}
			// convert 1->0 bias
			face[num_face].v[0] = &vertex[a - 1];
			face[num_face].v[1] = &vertex[b - 1];
			face[num_face].v[2] = &vertex[c - 1];
			num_face++;
		}
	} while (fgets(buf, 1024, fp));
}

// Load data from merged object file - octree surface model or .obj
static void load_object(char *name)
{
	char buf[1024];

	FILE *fp = fopen(name, "r");
	if (!fp) {
		fprintf(stderr, "%s: Can't open merged object file %s\n",
			progname, name);
		exit(1);
	}

	// check model file type (read first line)
	fgets(buf, sizeof(buf), fp);
	if (!strncmp(buf, "SFC_MODEL", 9)) {
		load_sfc_model(fp);
		have_color = 1;		// note that we have model color data
	} else {
		load_obj_file(fp, buf);
		if (extrapolate == 1)	// can't do model coloring
			extrapolate = 2;
		color_dist = 0;		// can't do color distance
	}
	fclose(fp);

	// allocate texture coordinates for each vertex
	for (int i=0; i<num_vertex; i++) {
		vertex[i].u = (float *)malloc(num_model * sizeof(float));
		vertex[i].v = (float *)malloc(num_model * sizeof(float));
#ifdef TEXSIZE
		vertex[i].ts = (float *)malloc(num_model * sizeof(float));
#endif
	}
}

// Compute texture coordinate for range-map model
// vo = vertex in this surface's object coordinates
// uv = output texture coordinates
static void range_texture(int i, double vo[], double sd[], Vertex *v)
{
	// object coords = camera frame, X=view dir, Y=left, Z=up
	// use perspective transform to image plane
	v->u[i] = sx * -vo[1] / vo[0] + 0.5;
	v->v[i] = sy *  vo[2] / vo[0] + 0.5;
#ifdef TEXSIZE
	// transform delta vertex the same way
	double dx = sx * -sd[1] / sd[0] + 0.5;
	double dy = sy *  sd[2] / sd[0] + 0.5;
	// get size of delta in texture pixels
	dx = fabs(dx - v->u[i]) * model[i].xres;
	dy = fabs(dy - v->v[i]) * model[i].yres;
	// choose smaller one (most stretched)
	v->ts[i] = (dx < dy) ? dx : dy;
#endif
}

// Compute texture coordinate for image-map (height map) model
// i = forest surface index
// vo = vertex in this surface's object coordinates
static void img_texture(int i, double vo[], double sd[], Vertex *v)
{
	// object coords = pixel indices, which correspond to UV,
	// except for scaling; map pixel (xres, yres) to (1, 1)
	v->u[i] = vo[0] / model[i].xres;
	v->v[i] = vo[1] / model[i].yres;
#ifdef TEXSIZE
	// (really, ts[] is same everywhere for img texture)
	double dx = fabs(sd[0] - vo[0]);
	double dy = fabs(sd[1] - vo[1]);
	v->ts[i] = (dx < dy) ? dx : dy;
#endif
}

// is this texture coordinate inside the texture?
static int inside(double uv[])
{
	return uv[0]>=0.0 && uv[0]<=1.0 && uv[1]>=0.0 && uv[1]<=1.0;
}

// 2D distance
static double dist2d(double a[2], double b[2])
{
	double dx = a[0] - b[0];
	double dy = a[1] - b[1];
	return sqrt(dx*dx + dy*dy);
}

// 3D integer squared distance (color distance)
static int idsq(int r1, int g1, int b1, int r2, int g2, int b2)
{
	r1 -= r2;
	g1 -= g2;
	b1 -= b2;
	return r1*r1 + g1*g1 + b1*b1;
}

// compute squared area of face in 2D texture space (using Heron's forumula)
static double texture_area(double uv1[2], double uv2[2], double uv3[2], int i)
{
	double a = dist2d(uv2, uv3);
	double b = dist2d(uv1, uv3);
	double c = dist2d(uv1, uv2);
	double s = 0.5 * (a + b + c);
	return (s * (s-a) * (s-b) * (s-c)) * model[i].xres * model[i].yres;
}

// Choose the best model to texture this face.
// Rigorous method would clip face to texture boundaries and find
// best coverage. Shortcut here just checks for full coverage; if
// no model textures fully cover the face, choose the texture whose
// center is nearest the average face vertex texture coordinates.
// If multiple models have full coverage, choose the one with the
// largest area in texture space (the one with the most detail).
static void choose_face(Face *f)
{
	int best_model = 0;
	
	// best_dist > 0 means no model has full coverage, value is 
	// 		 smallest "texture distance"
	// best_dist < 0 means at least one model has full coverage,
	//		 value is -(face area in texture coords [squared])
	double best_dist = FLT_MAX;	// -1=full cvg, else texture dist

	// face vertices
	Vertex *v1 = f->v[0];
	Vertex *v2 = f->v[1];
	Vertex *v3 = f->v[2];

	// face normal in base model coords
	double normal[3];
	surface_normal(v1->xyz, v3->xyz, v2->xyz, normal);

	double d;
	int i;
	for (i=0; i<num_model; i++) {
		// get face vertex texture coords for this model
		double uv1[2], uv2[2], uv3[2];
		uv1[0] = v1->u[i];
		uv1[1] = v1->v[i];
		uv2[0] = v2->u[i];
		uv2[1] = v2->v[i];
		uv3[0] = v3->u[i];
		uv3[1] = v3->v[i];

		// does this model's texture fully cover the face?
		if (inside(uv1) && inside(uv2) && inside(uv3)) {
			// yes, check angle between face and view direction
			double view[3];
			vector_diff(v1->xyz, model[i].eye, view);
			normalize_vector(view);
			// dot product = cosine
			if (dot_product(view, normal) < cos_threshold) {
				// only use this if no other full coverage
				d = 0.0;
			} else {
				// measure of texture area

#ifdef TEXSIZE
				// texture size at first vertex - less flipping?
				d = -v1->ts[i];
#else
				// technically better logic, but flips more?
				d = -texture_area(uv1, uv2, uv3, i);
#endif

			}

			// more negative = more pixels = better
			if (d < best_dist) {
				best_model = i;
				best_dist = d;
			}
			
		// no, compute distance (squared) from average texture
		// coordinate to center of texture = 0.5,0.5
		} else {
			double du = (uv1[0] + uv1[0] + uv2[0]) / 3 - 0.5;
			double dv = (uv1[1] + uv1[1] + uv2[1]) / 3 - 0.5;
			d = du*du + dv*dv;
			if (d < best_dist) {
				best_model = i;
				best_dist = d;
			}		
		}
	}				

	// partial coverage or back-facing triangle?
	if (best_dist >= 0.0) {
		if (extrapolate == 0) {		// discard
			best_model = -1;
			partial_reject++;	// diagnostic count
		} else if (extrapolate == 1) {	// use model color
			best_model = -2;
		}
	}
	
	// Optionally check distance between texture color and model color.
	// This semi-kludge handles problems such as triangles stitched
	// between meshes that aren't really visible from any model.
	if (color_dist && best_model >= 0) {
		int dsq = color_dist * color_dist;
		Model *m = &model[best_model];
		for (i=0; i<3; i++) {	// each face vertex
			// note texture v[0]=0.0 at bottom,
			// but map index 0 is at top
			double tx = f->v[i]->u[best_model]*m->xres;
			double ty = (m->yres-1) - 
					f->v[i]->v[best_model]*m->yres;
			uchar tr, tg, tb;	// texture map color
			m->tex.get_data()->iget_color(tx, ty, &tr, &tg, &tb);

			if (idsq(tr, tg, tb, f->v[i]->r, f->v[i]->g, 
							f->v[i]->b) > dsq) {
				// color too far off
				if (color_dist > 0)	// use model color
					best_model = -2;
				else {			// discard
					best_model = -1;
					color_reject++;	// diagnostic count
				}
				break;
			}
		}
	}

	f->mdl = best_model;	
}

// set vertex id's to 1 if needed by specified model number,
// and write Inventor coordinate list
static void write_vertices_for(int model, FILE *ofp)
{
	Vertex *v;
	Face *f;
	int j;
	
	// first mark all unused
	for (j=0, v=vertex; j<num_vertex; j++, v++)
		v->id = 0;
	// now mark the ones referenced by faces
	for (j=0, f=face; j<num_face; j++, f++) {
		if (f->mdl == model) 
			f->v[0]->id = f->v[1]->id = f->v[2]->id = 1;
	}

	// output coordinate list
	fputs("Coordinate3 {point [\n", ofp);
	for (j=0, v=vertex; j<num_vertex; j++, v++) {
		if (v->id)
			fprintf(ofp, "%f %f %f,\n",
				v->xyz[0], v->xyz[1], v->xyz[2]);
	}
	fputs("]}\n", ofp);
}

int main(int argc, char **argv)
{
	char *objfile=NULL, *fstfile=NULL, *ivfile=NULL;
	int	i, j, next_id;
	
	progname = argv[0];
	for(i=1; i<argc; i++) {
		if(!strcmp(argv[i],"-i")) {
			objfile = argv[++i];
		} else if(!strcmp(argv[i],"-f")) {
			fstfile = argv[++i];
		} else if(!strcmp(argv[i],"-o")) {
			ivfile = argv[++i];
		} else if(!strcmp(argv[i],"-v")) {
			verbose = TRUE;
		} else if(!strcmp(argv[i],"-n")) {
			cos_threshold = cos(TORADIANS * atof(argv[++i]));
		} else if(!strcmp(argv[i],"-p")) {
			extrapolate = atoi(argv[++i]);
		} else if(!strcmp(argv[i],"-c")) {
			color_dist = atoi(argv[++i]);
		} else if (argv[i][0] == '-') {
			fprintf(stderr, "Unknown argument %s\n", argv[i]);
			fprintf(stderr, usage, argv[0]);
			exit(1);
		} else {	// end of options
			break;
		}
	}
	// remaining args are texture files
	texname = argv + i;
	num_model = argc - i;

	if (objfile == NULL || fstfile == NULL) {
		fprintf(stderr, "Missing required arguments\n");
		fprintf(stderr, usage, argv[0]);
		exit(1);
	}

	if (verbose)
		fprintf(stderr, "Loading forest info\n");
	load_forest(fstfile);
	if (verbose)
		fprintf(stderr, "%d surfaces\nLoading merged model\n",
			num_model);
	load_object(objfile);
	if (verbose)
		fprintf(stderr, "%d vertices, %d faces\n",
			num_vertex, num_face);
				
	FILE *ofp;
	if (ivfile == NULL) {
		ofp = stdout;
	} else {
        //make sure output file will have proper permissions
        umask(002);
		ofp = fopen(ivfile, "w");
		if (ofp == NULL) {
			fprintf(stderr, 
				"%s: can't open output inventor file %s\n",
				progname, ivfile);
			exit(1);
		}
	}

	// compute texture coords for each model and vertex
	Face *f;
	Vertex *v;
	ZMatrix mdlwld0;
	model[0].node.GetTransformationMatrix(mdlwld0);
	Model *m = model;
	for (i=0; i<num_model; i++, m++) {
		if (verbose)
			fprintf(stderr, "Computing sfc %d texture coords\n", i);

		// setup transform to map vertex from sfc 0 model coords
		// to current surface object coords:
		// world-to-object[i] . model-to-world[0]
		ZMatrix objwldi, xform;
		m->node.GetObjToWorldTransform(objwldi);
		MatInvert(objwldi, xform);
		MatMult(xform, mdlwld0);

		if (m->fov) {	// range map?
			sx = 0.5 / tan(m->fov*TORADIANS/2.0);
			sy = sx * m->xres / m->yres;
		}
		
		for (j=0, v=vertex; j<num_vertex; j++, v++) {
			double vo[3];	// vertex in object coords
			MultPoints(v->xyz, xform, vo);

			// delta in object coords
			double sd[3];
			double delta[] = {v->xyz[0]+0.0001, 
					v->xyz[1]+0.0001, v->xyz[2]+0.0};
			MultPoints(delta, xform, sd);

			if (m->fov)	// range map?
				range_texture(i, vo, sd, v);
			else
				img_texture(i, vo, sd, v);
		}		

		// use inverse transform, objX to model0, to
		// compute eye point in base model coords
		ZMatrix invxform;
		MatInvert(xform, invxform);
		double eye[3];
		if (m->fov) {	// range map
			eye[0] = eye[1] = eye[2] = 0.0;
		} else {
			eye[0] = m->xres/2.0;
			eye[1] = m->yres/2.0;
			eye[2] = 1.0e10;
		}
		MultPoints(eye, invxform, m->eye);
	}

	// choose best model to texture each face
	if (verbose)
		fprintf(stderr, "Choosing best model for each face\n");
	for (i=0; i<num_face; i++)
		choose_face(&face[i]);

	// write inventor file
	if (verbose)
		fprintf(stderr, "Writing inventor model\n");
	fputs("#Inventor V2.0 ascii\n"
		"ShapeHints {\n"
		"vertexOrdering COUNTERCLOCKWISE\n"
		"shapeType SOLID\n"
		"faceType CONVEX}\n"
		"LightModel {model BASE_COLOR}\n", ofp);
	for (i=0; i<num_model; i++) {

		fprintf(ofp, "Separator { # model %s\n", model[i].name);
		fprintf(ofp, "Texture2 {filename \"%s\" "
				"wrapS CLAMP wrapT CLAMP}\n", texname[i]);

		// write and mark vertices that are needed by this model
		write_vertices_for(i, ofp);

		// write parallel list of texture coordinates
		fputs("TextureCoordinate2 {point [\n", ofp);
		next_id = 0;
		for (j=0, v=vertex; j<num_vertex; j++, v++) {
			if (v->id) {
				fprintf(ofp, "%.3f %.3f,\n", v->u[i], v->v[i]);
				v->id = next_id++;	// store file index
			}
		}
		fputs("]}\n", ofp);
		if (verbose)
			fprintf(stderr, "Model %d uses %d vertices\n",
				i, next_id);

		fputs("IndexedFaceSet {coordIndex [\n", ofp);
		for (j=0, f=face; j<num_face; j++, f++) {
			if (f->mdl == i)
				fprintf(ofp, "%d,%d,%d,-1,\n", f->v[0]->id,
					f->v[1]->id, f->v[2]->id);
		}

		fputs("] textureCoordIndex [\n", ofp);
		for (j=0, f=face; j<num_face; j++, f++) {
			if (f->mdl == i)
				fprintf(ofp, "%d,%d,%d,-1,\n", f->v[0]->id,
					f->v[1]->id, f->v[2]->id);
		}
		fputs("]}}\n", ofp);
	}

	// write model-colored faces
	fputs("Separator { # model-colored faces\n", ofp);
	fputs("LightModel {model PHONG}\n", ofp);
	fputs("MaterialBinding {value PER_VERTEX_INDEXED}\n", ofp);

	// write needed vertices
	write_vertices_for(-2, ofp);

	// write materials
	fputs("Material {diffuseColor [\n", ofp);
	next_id = 0;
	for (j=0, v=vertex; j<num_vertex; j++, v++) {
		if (v->id) {
			fprintf(ofp, "%.3f %.3f %.3f,\n",
				v->r/255.0, v->g/255.0, v->b/255.0);
			v->id = next_id++;	// store file index
		}
	}
	fputs("]}\n", ofp);
	if (verbose)
		fprintf(stderr, "%d model-colored vertices\n", next_id);
	
	// write faces
	fputs("IndexedFaceSet {coordIndex [\n", ofp);
	for (j=0, f=face; j<num_face; j++, f++) {
		if (f->mdl == -2)
			fprintf(ofp, "%d,%d,%d,-1,\n", f->v[0]->id,
					f->v[1]->id, f->v[2]->id);
	}
	fputs("] materialIndex [\n", ofp);
	for (j=0, f=face; j<num_face; j++, f++) {
		if (f->mdl == -2)
			fprintf(ofp, "%d,%d,%d,-1,\n", f->v[0]->id,
					f->v[1]->id, f->v[2]->id);
	}
	fputs("]}}\n", ofp);
	
	if (verbose) {
		fprintf(stderr, "%d partial/back-side face(s) discarded\n",
			partial_reject);
		fprintf(stderr, "%d color mismatched face(s) discarded\n",
			color_reject);
	}
	exit(0);
}
