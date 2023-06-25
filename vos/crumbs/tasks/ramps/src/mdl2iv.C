// %M% %I% %E% %U%
/** \file
 ** Convert SUMMITT model (forest, surface model, or octree) 
 ** to Inventor format.
 ** Can output point or polygon models, show coordinate axes,
 ** subsample input data, apply object transforms, output VRML,
 ** and a few other tricks.
 **/
#include "grape/sfcmodel.h"
#include "forest.h"

#define DEG2RAD(d)	((d)*3.141592654/180.0)

static const char usage[] = 
"Usage: %s -i input_file [-p] [-c] [-n] [-a len] [-t]\n"
"  [-r npoints] [-x] [-v] [-s x y z] [-f] [-o iv_file]\n\n"
"Converts SUMMITT model data (forest, surface model, or octree)\n"
"to Inventor or VRML format (for display)\n"
"-i = input data (forest, surface model, or octree) ('-' = stdin)\n"
"-p = force output to point model, ignoring any mesh data\n"
"-a = show model axes with length len\n"
"-c = use fixed colors (one per model for forest) instead of voxel colors\n"
"-n = use surface normals from model\n"
"-r = decimate input to about npoints (thumbnail) per model\n"
"-t = apply texture maps (input must be a forest)\n"
"-x = transform to object space instead of model space\n"
"-s = use initial X Y Z scaling, e.g. 1 -1 1 for LH models\n"
"-f = flip triangles to face -Z\n"
"-v = output in VRML format instead of Inventor\n"
"-o = output Inventor filename; default is standard output\n";

int point_model;		// output as point data?
int fixed_color;		// use fixed colors?
int vrml;			// output in VRML format?
int use_normals;		// use surface normals?
int reduce;			// reduce density to about this many points?
int redmod;			// reduction point ID modulo divider
int obj_space;			// transform to object space?
int flip;			// flip faces to -Z?
float axes;			// coordinate axis line lengths (0=off)
int texture;			// apply texture map?
static int point_id;		// voxel index
double xscale, yscale, zscale;	// overall scale option
ZMatrix m2o;			// current surface model-to-object transform
CAHVOR *cmod;			// current patch camera model
int xres, yres;			// current patch texture dimensions

// fixed color list
// 0=red 1=green 2=blue 3=yellow 4=violet 5=cyan 6=grey
const char *clist[] = { 
	"1 0 0", "0 1 0", "0 0 1", ".8 .8 0", 
	".8 0 .8", "0 .8 .8", ".4 .4 .4" };
#define NUM_COLORS 7
int cur_color = -1;

/// output inventor (or VRML) ASCII header
static void iv_header(FILE *fp)
{
	if (vrml)
		fputs("#VRML V1.0 ascii\n", fp);
	else
		fputs("#Inventor V2.0 ascii\n", fp);
	fputs("ShapeHints {\nvertexOrdering COUNTERCLOCKWISE\n"
		"shapeType SOLID\nfaceType CONVEX}\n", fp);
	if (!vrml)
		fprintf(fp, "LightModel {model %s}\n", 
			(fixed_color | use_normals) ? "PHONG" : "BASE_COLOR");
	if (xscale != 0.0)
		fprintf(fp, "Scale {scaleFactor %g %g %g}\n",
			xscale, yscale, zscale);
	if (fixed_color)
		fputs("MaterialBinding {value OVERALL}\n", fp);
	else if (!texture)
		fputs("MaterialBinding {value PER_VERTEX}\n", fp);
}

/// recursive octree node count
void count_nodes(Octree_Data *od, int *np)
{
	for (NodeSpec *ns=od->get_node_data(); ns; ns=ns->next)
		(*np)++;

	for (int i=0; i<NUMBER_OF_SPACE_PARTITIONS; i++) {
		if (od->get_child((Octree_Child_Ref)i))
			count_nodes(od->get_child((Octree_Child_Ref)i), np);
	}
}

/// recursive output of octree voxel coordinates (model frame)
void write_coords(Octree_Data *od, FILE *fp)
{
	double gc[3];
	for (NodeSpec *ns=od->get_node_data(); ns; ns=ns->next) {
		ns->id = point_id++;
		if (point_id % redmod == 0) {
			ns->get_global_center(gc);
			fprintf(fp, "%f %f %f,\n", gc[0], gc[1], gc[2]);
		}
	}

	for (int i=0; i<NUMBER_OF_SPACE_PARTITIONS; i++) {
		if (od->get_child((Octree_Child_Ref)i))
			write_coords(od->get_child((Octree_Child_Ref)i), fp);
	}
}

/// recursive output of octree surface normal vectors (model frame)
void write_normals(Octree_Data *od, FILE *fp)
{
	double n[3];
	for (NodeSpec *ns=od->get_node_data(); ns; ns=ns->next) {
		if (ns->id % redmod == 0) {
			ns->get_normal(n);
			fprintf(fp, "%f %f %f,\n", n[0], n[1], n[2]);
		}
	}

	for (int i=0; i<NUMBER_OF_SPACE_PARTITIONS; i++) {
		if (od->get_child((Octree_Child_Ref)i))
			write_normals(od->get_child((Octree_Child_Ref)i), fp);
	}
}

/// recursive output of octree colors
void write_colors(Octree_Data *od, FILE *fp)
{
	double r, g, b;
	for (NodeSpec *ns=od->get_node_data(); ns; ns=ns->next) {
		if (ns->id % redmod == 0) {
			ns->get_color(&r, &g, &b);
			fprintf(fp, "%.3f %.3f %.3f,\n", r, g, b);
		}
	}

	for (int i=0; i<NUMBER_OF_SPACE_PARTITIONS; i++) {
		if (od->get_child((Octree_Child_Ref)i))
			write_colors(od->get_child((Octree_Child_Ref)i), fp);
	}
}

/// recursive output of octree texture coordinates
void write_tex(Octree_Data *od, FILE *fp)
{
	double gc[3], cc[3], st[2];
	for (NodeSpec *ns=od->get_node_data(); ns; ns=ns->next) {
		if (ns->id % redmod == 0) {
			// get model coords
			ns->get_global_center(gc);
			// transform to object (camera) space
			MultPoints(gc, m2o, cc);
			// use camera model to project 3D point to image
			cmod->To_2D(cc, st);
			// convert pixel->fraction, deal with left-handedness
			fprintf(fp, "%.3f %.3f,\n", 
				st[0] / xres, 1.0 - st[1] / yres);
		}
	}

	for (int i=0; i<NUMBER_OF_SPACE_PARTITIONS; i++) {
		if (od->get_child((Octree_Child_Ref)i))
			write_tex(od->get_child((Octree_Child_Ref)i), fp);
	}
}

/// write surface model faces
void write_faces(Mesh_Triangle *tlist, FILE *fp)
{
	fputs("IndexedFaceSet {coordIndex [\n", fp);
	Mesh_Triangle *tmp;
	if (flip) {
		for (tmp=tlist; tmp; tmp=tmp->next) {
			fprintf(fp, "%d,%d,%d,-1,\n",
				tmp->get_vertex_1()->id,
				tmp->get_vertex_3()->id,
				tmp->get_vertex_2()->id);
		}
		if (!fixed_color && !texture) {
			fputs("] materialIndex [\n", fp);
			for (tmp=tlist; tmp; tmp=tmp->next) {
				fprintf(fp, "%d,%d,%d,-1,\n",
					tmp->get_vertex_1()->id,
					tmp->get_vertex_3()->id,
					tmp->get_vertex_2()->id);
			}
		}
	} else {
		for (tmp=tlist; tmp; tmp=tmp->next) {
			fprintf(fp, "%d,%d,%d,-1,\n",
				tmp->get_vertex_1()->id,
				tmp->get_vertex_2()->id,
				tmp->get_vertex_3()->id);
		}
		if (!fixed_color && !texture) {
			fputs("] materialIndex [\n", fp);
			for (tmp=tlist; tmp; tmp=tmp->next) {
				fprintf(fp, "%d,%d,%d,-1,\n",
					tmp->get_vertex_1()->id,
					tmp->get_vertex_2()->id,
					tmp->get_vertex_3()->id);
			}
		}
	}
	fputs("]}\n", fp);
}

/// output axis vectors, and optional origin box
static void write_axes(int origin, float length, FILE *fp)
{
	fprintf(fp, 
		  "Coordinate3 {point [0 0 0, %f 0 0, 0 %f 0, 0 0 %f]}\n"
		  "Material {diffuseColor 1 0.3 0.3} "
			"IndexedLineSet {coordIndex [0, 1]}\n"
		  "Material {diffuseColor 0.3 1 0.3} "
			"IndexedLineSet {coordIndex [0, 2]}\n"
		  "Material {diffuseColor 0.3 0.3 1} "
			"IndexedLineSet {coordIndex [0, 3]}\n"
		  "Material {specularColor 0.1 0.1 0.1 diffuseColor %s}\n",
		  length, length, length, clist[cur_color]);
	if (origin)
		fprintf(fp, 
		  "Cube {width %f height %f depth %f}\n",
		  length/20.0, length/20.0, length/20.0);
}

/// write transform for model in forest
// (transforms affect object in reverse order)
void write_xform(ObjNode *xfnode, FILE *fp)
{
	// object to world transform
	fprintf(fp, "Translation {translation %f %f %f}\n",
		(float)xfnode->x, (float)xfnode->y, (float)xfnode->z);
	fprintf(fp, "Scale {scaleFactor %f %f %f}\n",
		(float)xfnode->xscale, (float)xfnode->yscale, 
		(float)xfnode->zscale);
	fprintf(fp, "Rotation { rotation 0 0 1 %f }\n",
		DEG2RAD(xfnode->zrot));
	fprintf(fp, "Rotation { rotation 0 1 0 %f }\n",
		DEG2RAD(xfnode->yrot));
	fprintf(fp, "Rotation { rotation 1 0 0 %f }\n",
		DEG2RAD(xfnode->xrot));

	// add axes in object (camera) frame
	if (axes)
		write_axes(0, axes, fp);

	// model to object transform
	Obj *obj = xfnode->get_object();
	fprintf(fp, "Rotation { rotation 0 0 1 %f }\n",
		DEG2RAD(obj->zrot));
	fprintf(fp, "Rotation { rotation 0 1 0 %f }\n",
		DEG2RAD(obj->yrot));
	fprintf(fp, "Rotation { rotation 1 0 0 %f }\n",
		DEG2RAD(obj->xrot));
	fprintf(fp, "Scale {scaleFactor %f %f %f}\n",
		(float)obj->xscale, (float)obj->yscale, (float)obj->zscale);
	fprintf(fp, "Translation {translation %f %f %f}\n",
		-(float)obj->x, -(float)obj->y, -(float)obj->z);
}

/// output one surface model
void write_model(SfcModel *m, FILE *fp, int model_points, Patch *p=NULL)
{
	fputs("Separator {\n", fp);
	
	cur_color = (cur_color+1) % NUM_COLORS;

	if (axes) {
		write_axes(1, axes, fp);
	} else if (fixed_color) {
		fprintf(fp, "Material {specularColor 0.1 0.1 0.1 "
			"diffuseColor %s}\n", clist[cur_color]);
	} 

	fputs("Coordinate3 {point [\n", fp);
	point_id = 0;
	if (reduce && model_points > reduce)
		redmod = model_points / reduce;
	else
		redmod = 1;	// use every point
	write_coords(m->get_data(), fp);
	fputs("]}\n", fp);

	if (use_normals) {
		fputs("Normal {vector [\n", fp);
		write_normals(m->get_data(), fp);
		fputs("]}\n", fp);
	}

	// (need patch for texture mapping info)
	if (texture && p) {
		// need model->object transform for mapping
		m->GetModelToObjectTransform(m2o);
		// need camera model and texture dimensions
		cmod = &p->cmod;
		xres = p->xres;
		yres = p->yres;
		fprintf(fp, "Texture2 {filename \"%s\"}\n", p->get_name());
		fputs("TextureCoordinate2 {point [\n", fp);
		write_tex(m->get_data(), fp);
		fputs("]}\n", fp);

	} else if (!fixed_color) {
		fputs("Material { specularColor 0.1 0.1 0.1\n", fp);
		fputs("diffuseColor [\n", fp);
		write_colors(m->get_data(), fp);
		fputs("]}\n", fp);
	}

	// if didn't supply transform objnode, but want object coords,
	// setup an objnode with identity obj->world xform
	ObjNode *xfnode = (ObjNode *)p;
	if (obj_space && xfnode == NULL) {
		static ObjNode tnode;
		tnode.set_object((Obj *)m);
		xfnode = &tnode;
	}
	if (xfnode) 
		write_xform(xfnode, fp);

	if (point_model || m->mesh.tri_list == NULL)
		fputs("PointSet {}\n", fp);
	else
		write_faces(m->mesh.tri_list, fp);

	fputs("}\n", fp);
}

int main(int argc, char **argv)
{
	FILE *ofp;
	char *infile = NULL, *outfile = NULL;
	int i;

	for(i=1; i<argc; i++) {
		if(!strcmp(argv[i],"-i")) {
			infile = argv[++i];
		} else if(!strcmp(argv[i],"-o")) {
			outfile = argv[++i];
		} else if(!strcmp(argv[i],"-p")) {
			point_model = 1;
		} else if(!strcmp(argv[i],"-c")) {
			fixed_color = 1;
		} else if(!strcmp(argv[i],"-n")) {
			use_normals = 1;
		} else if(!strcmp(argv[i],"-a")) {
			axes = atof(argv[++i]);
		} else if(!strcmp(argv[i],"-t")) {
			texture = 1;
		} else if(!strcmp(argv[i],"-r")) {
			reduce = atoi(argv[++i]);
			point_model = 1;	// can't do triangles
		} else if(!strcmp(argv[i],"-x")) {
			obj_space = 1;
		} else if(!strcmp(argv[i],"-s")) {
			xscale = atof(argv[++i]);
			yscale = atof(argv[++i]);
			zscale = atof(argv[++i]);
		} else if(!strcmp(argv[i],"-f")) {
			flip = 1;
		} else if(!strcmp(argv[i],"-v")) {
			vrml = 1;
		} else {
			fprintf(stderr, "Unknown argument %s\n", argv[i]);
			fprintf(stderr, usage, argv[0]);
			exit(1);
		}
	}

	if (infile == NULL) {
		fprintf(stderr, "Input file not specified\n");
		fprintf(stderr, usage, argv[0]);
		exit(1);
	}

	// start input
	FILE_Dataport infp;
	if (!strcmp(infile, "-")) {
		infp.open(stdin);
	} else if (!infp.ropen(infile)) {
		fprintf(stderr, "%s: Whoops - Can't open %s for reading\n", 
			argv[0], infile);
		exit(1);
	}
	
	// start output
	if (outfile) {
        //make sure output file will have proper permissions
        umask(002);
		if ((ofp = fopen(outfile, "w")) == NULL) {
			fprintf(stderr, "%s: Can't create output %s\n", 
				argv[0], outfile);
			exit(1);
		}
	} else {
		ofp = stdout;
	}
	iv_header(ofp);

 	// check input file type
 	char token[4096];
 	get_next_token(&infp, token);
	if (!strcmp(token, "GRP_V1")) {		// input is forest
		// to reduce memory usage, load patches one at a time
		Forest forest;
		if (!forest.parse_in(&infp, FALSE))
			exit(1);
		infp.close();
		i = forest.get_num_children();
		while (--i >= 0) {
			fprintf(stderr, "Loading forest patch %d\n", i);
			Patch *p = forest.get_patch(i);
			if (!p->parse_reference(&infp))
				continue;	// parse in failed
			SfcModel *mdl = (SfcModel *)p->get_object();
			// need to count nodes - get_node_count()
			// not valid after forest parse_in
			int n = 0;
			count_nodes(mdl->get_data(), &n);
			write_model(mdl, ofp, n, p);
			forest.remove_child(i);
		}

	} else if (!strcmp(token, "SFC_MODEL_V1")) {
		SfcModel sfc;
		if (!sfc.parse_in(&infp))
			exit(1);
		write_model(&sfc, ofp, sfc.get_node_count());

	} else if (!strcmp(token, "OCTREE_V1")) {
		SfcModel sfc;
		if (!sfc.Octree::parse_in(&infp))
			exit(1);
		texture = 0;	// not enough info for texture mapping
		write_model(&sfc, ofp, sfc.get_node_count());
	} else {
		fprintf(stderr, "%s: Whoops - unknown input type token >%d<\n",
			argv[0], token);
		exit(1);
	}
	return 0;
}
