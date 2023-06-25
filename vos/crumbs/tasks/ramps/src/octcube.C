// octcube.C 1.1 02/10/11 13:04:21
/** \file
 ** Build Inventor models from octree, 
 ** with cubes indicating occupied subvolumes.
 **/
#include <stdio.h>
#include "summitt_func.h"

static const char usage[] = 
"Usage: %s [-v] [-i input_file] [-n levels] -o output_base\n"
"Builds inventor models showing octree population at each level.\n"
"-n = max number of levels\n"
"-v = verbose output\n";

#define COLOR	"0.0 1.0 0.0"	// cube color RGB

static Octree octree;		// the input octree 
static int outlevel;		// current output level
static FILE *fp;		// current output file
int verbose;

/// Recursive model building from octree
static void do_node(Octree_Data *od, int level, double width,
		double cx, double cy, double cz)
{
	int ref;
	
	// not down far enough yet?
	if (level < outlevel) {
		// recursively process each valid child of node
		width /= 2.0;
		for (ref=UPPER_LEFT_BACK; ref<NUMBER_OF_SPACE_PARTITIONS; 
					ref++) {
			Octree_Data *kid = od->get_child((Octree_Child_Ref)ref);
			if (kid) {
				do_node(kid, level+1, width,
					cx + X_Offset[ref]*width,
					cy + Y_Offset[ref]*width,
					cz + Z_Offset[ref]*width);
			}
		}
		return;
	}

	// okay, if any children here, draw a box
	for (ref=UPPER_LEFT_BACK; ref<NUMBER_OF_SPACE_PARTITIONS; ref++) {
		if (od->get_child((Octree_Child_Ref)ref)) {
			double cwid = width * 1.8;
			fprintf(fp, "Separator { "
				"Translation { translation %f %f %f }"
				" Cube { width %f height %f depth %f } }\n",
				cx, cy, cz, cwid, cwid, cwid);
			return;
		}
	}
}

/// Construct height image and color image from forest
static void build_model(char *name)
{
    //make sure output file will have proper permissions
    umask(002);
	fp = fopen(name, "w");
	if (fp == NULL) {
		fprintf(stderr, "Can't create %s\n", name);
		exit(1);
	}

	if (verbose)
		fprintf(stderr, "Creating model %s\n", name);
		
	fprintf(fp, "#Inventor V2.0 ascii\n");
	fprintf(fp, "Material {diffuseColor %s}\n", COLOR);
	
	do_node(octree.get_data(), 0, 2.0, 0.0, 0.0, 0.0);
	
	fclose(fp);
}

int main(int argc, char **argv)
{
	int i;
	int levels;		// max levels to output
	char *infile = NULL;	// input name
	char *ivbase = NULL;	// output base name

	for (i=1; i<argc; i++) {
		if(!strcmp(argv[i],"-i") && i+1 < argc) {
			infile = argv[++i];
		} else if(!strcmp(argv[i],"-o") && i+1 < argc) {
			ivbase = argv[++i];
		} else if(!strcmp(argv[i],"-n") && i+1 < argc) {
			levels = atoi(argv[++i]);
		} else if(!strcmp(argv[i],"-v")) {
			verbose = TRUE;
		} else {
			fprintf(stderr, "Unknown argument %s\n", argv[i]);
			fprintf(stderr, usage, argv[0]);
			exit(1);
		}
	}

	if (ivbase == NULL) {
		fprintf(stderr, "Required argument missing\n");
		fprintf(stderr, usage, argv[0]);
		exit(1);
	}

	// read octree
	FILE_Dataport ifp;
	if(infile) {
		if (!ifp.ropen(infile)) {
			fprintf(stderr, 
				"%s: Whoops - Can't open %s for reading\n", 
				argv[0], infile);
			exit(1);
		}
	} else {
		ifp.open(stdin);
		infile = "stdin";
	}

	if (verbose)
		fprintf(stderr,"Loading input data\n");

 	// check input file type
 	char token[4096];
 	get_next_token(&ifp, token);
	if (!strcmp(token, "SFC_MODEL_V1"))
	 	get_next_token(&ifp, token);
	if (strcmp(token, "OCTREE_V1")) {
		fprintf(stderr, "%s: Whoops - unknown input "
			" type token >%d<\n", argv[0], token);
		exit(1);
	}
	// read in octree (ignore any mesh)
	if (!octree.parse_in(&ifp))
		exit(1);
	ifp.close();

	// create output files
	if (levels < 1)
		levels = octree.get_max_levels();
	for (outlevel=1; outlevel<levels; outlevel++) {
		sprintf(token, "%s%02d.iv", ivbase, outlevel);
		build_model(token);
	}
		
	return 0;
}
