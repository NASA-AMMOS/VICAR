// make_tris.C 1.21 03/04/24 12:23:49
/** \file
* Application wrapper for Marching Triangles mesh generator to
* build triangle mesh from octree or forest of octrees.
*/
#include "summitt_func.h"

static const char usage[] = 
"Usage: %s [-v] [-n norm_threshold] [-p max_proj_steps] [-a accum_levels]\n"
" [-check_all] [-z] [-i input_file] [-o output_obj_file] [-s output_sfc_file]\n\n"
"Builds triangle mesh from octree or octree forest 'input_file'\n"
"(default stdin), writing .OBJ model and/or surface model ('-' for stdout).\n"
"-v = verbose output\n"
"-n = max allowed angle in degrees between normals (default=90)\n"
"-p = max steps to project new edge for extension (default=5)\n"
"-a = number of levels on merged output to accumulate\n"
"     (this also causes any input mesh to be discarded)\n"
"-check_all = check input meshes against all other models,\n"
"             not just those at same/higher resolution\n"
"-z = +Z is down (for input normals)\n";

static Forest forest;

// Options from command line:

// threshold of cosine of angle between normals to accept candidate
static double cos_threshold = 0.0;

// max projection steps for candidate delaunay steps
static int max_proj_steps = 5;

// number of bottom octree levels to accumulate after merging
static int accum_levels = 0;

// validate input triangles against all other trees (including low-res)?
static int check_all_levels = FALSE;

// +Z points down on input normals?
static int zdown = FALSE;

// undocumented test option (-x) to bypass construction of new mesh data
static int skip_new_mesh = FALSE;

int main(int argc, char **argv)
{
	char *infile=NULL, *objfile=NULL, *sfcfile=NULL;
	int	i;

	for(i=1; i<argc; i++) {
		if(!strcmp(argv[i],"-i")) {
			infile = argv[++i];
		} else if(!strcmp(argv[i],"-o")) {
			objfile = argv[++i];
		} else if(!strcmp(argv[i],"-s")) {
			sfcfile = argv[++i];
		} else if(!strcmp(argv[i],"-n")) {
			cos_threshold = cos(TORADIANS * atof(argv[++i]));
		} else if(!strcmp(argv[i],"-p")) {
			max_proj_steps = atoi(argv[++i]);
		} else if(!strcmp(argv[i],"-a")) {
			accum_levels = atoi(argv[++i]);
		} else if(!strcmp(argv[i],"-check_all")) {
			check_all_levels = TRUE;
		} else if(!strcmp(argv[i],"-check_all")) {
			zdown = TRUE;
		} else if(!strcmp(argv[i],"-v")) {
			verbose = TRUE;
		} else if(!strcmp(argv[i],"-x")) {
			skip_new_mesh = TRUE;
		} else {
			fprintf(stderr, "Unknown argument %s\n", argv[i]);
			fprintf(stderr, usage, argv[0]);
			exit(1);
		}
	}

	if (sfcfile == NULL && objfile == NULL) {
		fprintf(stderr,
			"%s: Whoops - No output specified\n", argv[0]);
		exit(1);
	}

	// read in input - octree, surface model, or forest
	FILE_Dataport *fp = new FILE_Dataport();
	if(infile) {
		if (!fp->ropen(infile)) {
			fprintf(stderr, 
				"%s: Whoops - Can't open %s for reading\n", 
				argv[0], infile);
			exit(1);
		}
	} else {
		fp->open(stdin);
		infile = (char *)"stdin";
	}
	SfcModel *sfc;
	ObjNode *node;

	if (verbose)
		fprintf(stderr,"Loading input data\n");

 	// check input file type
 	char token[4096];
 	get_next_token(fp, token);
	if (!strcmp(token, "GRP_V1")) {
		if (!forest.parse_in(fp))
			exit(1);
	} else if (!strcmp(token, "SFC_MODEL_V1")) {
		sfc = new SfcModel;
		if (!sfc->parse_in(fp))
			exit(1);
		node = new ObjNode;
		node->set_object(sfc);
		forest.add_child(node);
	} else if (!strcmp(token, "OCTREE_V1")) {
		sfc = new SfcModel;
		// already read first token, so can't use sfc->parse_in :(
		if (!sfc->Octree::parse_in(fp))
			exit(1);
		node = new ObjNode;
		node->set_object(sfc);
		forest.add_child(node);
	} else {
		fprintf(stderr, "%s: Whoops - unknown input type token >%d<\n",
			argv[0], token);
		exit(1);
	}

	// count up total number of input points
	int total_points = 0;
	for (i=0; i<forest.get_num_children(); i++) {
		sfc = (SfcModel *)forest.get_child(i)->get_object();
		if (sfc->mesh.pt_count == 0)	// no point list yet?
			sfc->add_pt_list();
		total_points += sfc->mesh.pt_count;
	}
	if (verbose)
		fprintf(stderr,"Read %d surface(s), %d voxels total\n", 
			forest.get_num_children(), total_points);
	if (total_points <= 2) {
		fprintf(stderr, "%s: Whoops, not enough input points\n",
			argv[0]);
		exit(1);
	}

	// Invoke the marching triangles code
	MT_Mesh *tm = make_triangle_model_from_forest(&forest,
		cos_threshold, max_proj_steps, accum_levels,
		check_all_levels, zdown, skip_new_mesh);

	if (tm == NULL) {
		fprintf(stderr, "%s: Whoops - Problem generating mesh.\n",
			argv[0]);
		exit(1);
	}

	// optionally output wavefront object model
	if (objfile) {
		if (verbose)
			fprintf(stderr, "Writing OBJ model\n");
		if (!strcmp(objfile, "-"))
			tm->print_obj_file(stdout, infile);
		else 
			tm->print_obj_file(objfile, infile);
	}

	// optionally output surface (octree and mesh) 
	if (sfcfile) {
        	if (!fp->wopen(sfcfile)) {
			fprintf(stderr, 
				"Unable to open output file %s.\n", sfcfile);
			exit(1);
		}
		sfc = (SfcModel *)forest.get_child(0)->get_object();
		// set surface triangle list to "master" list
		sfc->mesh.tri_list = tm->tri_list;
		if (verbose)
			fprintf(stderr, "Writing surface model\n");
		sfc->parse_out(fp);
		fp->close();
	}
	return 0;
}
