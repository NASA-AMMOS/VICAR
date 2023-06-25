// sfc2vst.C 1.2 02/06/14 08:27:38
/** \file
* Standalone program to convert a list of surface models to ViSTa file.
*/
#include <stdio.h>
#include "summitt_func.h"

#define MAX_LOD		30

static const char usage[] = 
"Usage: %s [-v] [-f] -o VST_output_name -t texture_file[:fov]\n"
"    -c cmod_file [-ext texture-suffix] sfcmodel [sfcmodel...]\n\n"
"Converts surface model meshes to VST format.\n"
"-o = VST output filename\n"
"-t = texture map filename\n"
"-c = camera model (CAHVOR) filename\n"
":fov = corresponding model's horizontal field-of-view in degrees,\n"
"       if model is from XYZ map. fov='-' for left-handed height map,\n"
"       no fov for right-handed height map.\n"
"-ext = replace texture filename suffix\n"
"-v = verbose output.\n"
"sfcmodel = GRAPE surface model files, at increasing detail levels.\n";

int verbose;
static SfcModel *sfc[MAX_LOD];	// surface models loaded from input files

// Load input surface model from file to sfc[] array
static void load_model(int id, char *infile)
{
	if (verbose)
		fprintf(stderr, "Loading model %d from %s\n", id, infile);

	FILE_Dataport fp;
	if (!fp.ropen(infile)) {
		fprintf(stderr, "sfc2vst: Can't open %s for reading\n", infile);
		exit(1);
	}

	char token[4096];
	get_next_token(&fp, token);
	if (strcmp(token, "SFC_MODEL_V1")) {
		fprintf(stderr, "sfc2vst: Input %s isn't a surface model\n", 
			infile);
		exit(1);
	}
	sfc[id] = new SfcModel;
	sfc[id]->parse_in(&fp);
}

int main(int argc, char **argv)
{
	int i;
	char *outfile = NULL;
	char *texfile = NULL;
	char *cmodfile = NULL;
	char *texext = NULL;

	for (i=1; i<argc; i++) {
		if (!strcmp(argv[i], "-o")) {
			outfile = argv[++i];
		} else if (!strcmp(argv[i], "-t")) {
			texfile = argv[++i];
		} else if (!strcmp(argv[i], "-c")) {
			cmodfile = argv[++i];
		} else if (!strcmp(argv[i], "-v")) {
			verbose = 1;
		} else if (!strcmp(argv[i], "-ext")) {
			texext = argv[++i];
		} else if (argv[i][0] == '-') {
			fprintf(stderr, "Unknown argument %s\n", argv[i]);
			fprintf(stderr, usage, argv[0]);
			exit(1);
		} else {	// end of options
			break;
		}
	}

	// remaining args are surface model files
	char **mdlname = argv + i;
	int num_models = argc - i;
	if (num_models < 1 || num_models >= MAX_LOD) {
		fprintf(stderr, "Need 1 to %d LOD models\n", MAX_LOD-1);
		fprintf(stderr, usage, argv[0]);
		return 1;
	}

	if (!outfile || !texfile || !cmodfile) {
		fprintf(stderr, "Required argument not specified\n");
		fprintf(stderr, usage, argv[0]);
		return 1;
	}
	// get fov/model-type
	double fov;
	char *c = strrchr(texfile, ':');
	if (c) {
		if (c[1] == '-')
			fov = -1.0;	// left-handed height model
		else
			fov = atof(c+1);	// range model
		*c = 0;			// remove :fov
	} else {
		fov = 0.0;		// right-handed height model
	}

	// get texture map size
	GeoImage *rgb = new GeoImage(texfile);
	int xres, yres;
	if (rgb->get_res(&xres, &yres) < 1) {
		fprintf(stderr, "Can't open texture file %s\n", texfile);
		return 1;
	}

	// load camera model
	CAHVOR cmod(cmodfile);

	// load input models
	for (i=0; i<num_models; i++)
		load_model(i, mdlname[i]);

	// run the converter
	return summitt_sfc2vst(num_models, sfc, outfile, texfile, texext, 
			xres, yres, fov*TORADIANS, NULL, &cmod);
}
