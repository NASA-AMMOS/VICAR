// cm2pnt.C 1.3 02/05/15 13:50:47
/** \file
 ** Test program to convert camera model file to SUMMITT pointing.
 **
 ** This standalone program uses the summitt_cmod_to_pointing()
 ** service to extract camera pointing data from a camera model,
 ** and convert the camera model from world to camera frame.
 **/
#include <stdio.h>
#include "image/image.h"
#include "image/types/all.h"
#include "summitt_func.h"
#include "cahvor.h"

// PDS header keys for CAHVOR camera model components
#define CMOD_C_KEY      "MODEL_COMPONENT_1"
#define CMOD_A_KEY      "MODEL_COMPONENT_2"
#define CMOD_H_KEY      "MODEL_COMPONENT_3"
#define CMOD_V_KEY      "MODEL_COMPONENT_4"
#define R2S_GROUP       "ROVER_COORDINATE_SYSTEM"
#define R2S_TRANS       "ORIGIN_OFFSET_VECTOR"
#define R2S_ROT         "ORIGIN_ROTATION_QUATERNION"

static const char usage[] = "usage: %s [-v] [-m cahv_input]\n"
"  [-i img_input] [-o cahv_output]\n\n"
"-m = CAHVOR camera model input file\n"
"-i = image input file with CAHV* model in header (e.g. VICAR, PDS)\n"
"-o = output CAHVOR model\n"
"-v = verbose output\n"
"Supply camera model using -m or -i\n";

int verbose;
static CAHVOR cmod;	// input camera model (world frame)

/// Extract camera model component from PDS file header string,
// list is in the form "(val, val, val)".
// Return 0 if okay
static void extract_cmod(PDSFile *f, char *key, double a[3])
{
        char *val = f->get_value(key);
        if (val == NULL) {
                fprintf(stderr, "Didn't find PDS file key %s\n", key);
                exit(1);
        }
        sscanf(val, "(%lf,%lf,%lf", &a[0], &a[1], &a[2]);
}

/// get camera model from PDS image file header comments
static void get_img_cmod(char *name, CAHVOR *model)
{
	Image img;
	PDSFile *pds = new PDSFile;
	pds->set_image(&img);
	if (pds->read_header(name)) {
		fprintf(stderr, "%s not found or not a valid PDS image\n",
			name);
		exit(1);
	}

	extract_cmod(pds, CMOD_C_KEY, model->c);
	extract_cmod(pds, CMOD_A_KEY, model->a);
	extract_cmod(pds, CMOD_H_KEY, model->h);
	extract_cmod(pds, CMOD_V_KEY, model->v);

	// If XYZ has ROVER_COORDINATE_SYSTEM transform,
	// model is in rover frame; convert to site frame
	char *val = pds->get_value(R2S_TRANS, R2S_GROUP);
	if (val) {
		if (verbose)
			fprintf(stderr, "Transforming camera model "
                                        "to site frame\n");
                double t[3], r[4];
                sscanf(val, "(%lf,%lf,%lf", &t[0], &t[1], &t[2]);
                val = pds->get_value(R2S_ROT, R2S_GROUP);
                sscanf(val, "(%lf,%lf,%lf,%lf",
                                &r[0], &r[1], &r[2], &r[3]);
                CAHVOR xcmod;
                double t0[3] = { 0.0, 0.0, 0.0 };
                double r0[4] = { 1.0, 0.0, 0.0, 0.0 };
                model->move(t0, r0, t, r, &xcmod);
                *model = xcmod;
        }
}

int main(int argc, char **argv)
{
	char *cfile = NULL;
	char *ifile = NULL;
	char *ofile = NULL;

	for (int i=1; i<argc; i++) {
		if (!strcmp(argv[i], "-v"))
			verbose = 1;
		else if (!strcmp(argv[i], "-m"))
			cfile = argv[++i];
		else if (!strcmp(argv[i], "-i"))
			ifile = argv[++i];
		else if (!strcmp(argv[i], "-o"))
			ofile = argv[++i];
		else {
			fprintf(stderr, "Unknown argument %s\n", argv[i]);
			fprintf(stderr, usage, argv[0]);
			return 1;
		}
	}

	// if filename given, extract camera model from that
	if (cfile) {
		if (cmod.read(cfile)) {
			fprintf(stderr, "error reading camera model %s\n", 
				cfile);
			exit(1);
		}
	} else if (ifile) {
		get_img_cmod(ifile, &cmod);
	} else {
		fprintf(stderr, usage, argv[0]);
		return 1;
	}

	if (verbose)
		fprintf(stderr, "C = %f %f %f\nA = %f %f %f\nH = %f %f %f\n",
			cmod.c[0], cmod.c[1], cmod.c[2], 
			cmod.a[0], cmod.a[1], cmod.a[2], 
			cmod.h[0], cmod.h[1], cmod.h[2]);

	// build rotation matrix, transform model to camera frame
	double rot[3];
	CAHVOR ccmod;
	if (summitt_cmod_to_pointing(&cmod, rot, &ccmod)) {
		fprintf(stderr, usage, argv[0]);
		return 1;
	}

	// output in format suitable for SUMMITT programs
	printf("-r %f %f %f -t %f %f %f\n", 
		rot[0], rot[1], rot[2], cmod.c[0], cmod.c[1], cmod.c[2]);

	// optionally output modified model
	if (ofile) {
		if (ccmod.write(ofile, "camera frame")) {
			fprintf(stderr, "error writing camera model %s\n", 
				ofile);
			exit(1);
		}
	}

	if (verbose) {
		// Verify rotation using SUMMITT functions to rotate
		// camera frame "forward" and "up" vectors.
		// Check these against "cmodpose (cam_model)"
		ZMatrix m;
		MakeRotationMatrix(m, rot[0], rot[1], rot[2]);
		double fwd[3] = { 1.0, 0.0, 0.0 };
		double up[3] = { 0.0, 0.0, 1.0 };
		double a[3];
		MultPoints(fwd, m, a);
		fprintf(stderr, "Fwd vector=%f %f %f\n", a[0], a[1], a[2]);
		MultPoints(up, m, a);
		fprintf(stderr, "Up vector=%f %f %f\n", a[0], a[1], a[2]);
	}

	return 0;
}
