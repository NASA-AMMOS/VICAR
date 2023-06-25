// These support functions are used by terrain merge master and 
// standalone slave.
//

#include <stdio.h>
#include "image/image.h"
#include "image/types/all.h"
#include "summitt_func.h"
#include "zvproto.h"


// PDS header keys for CAHVOR camera model components
#define CMOD_C_KEY      "MODEL_COMPONENT_1"
#define CMOD_A_KEY      "MODEL_COMPONENT_2"
#define CMOD_H_KEY      "MODEL_COMPONENT_3"
#define CMOD_V_KEY      "MODEL_COMPONENT_4"
#define CMOD_O_KEY      "MODEL_COMPONENT_5"
#define CMOD_R_KEY      "MODEL_COMPONENT_6"

// other PDS header keys for patch fields
#define IDENT_KEY	"PRODUCT_ID"
#define L2S_GROUP	"LANDER_COORDINATE_SYSTEM"
#define R2S_GROUP	"ROVER_COORDINATE_SYSTEM"
#define R2S_TRANS	"ORIGIN_OFFSET_VECTOR"
#define R2S_ROT		"ORIGIN_ROTATION_QUATERNION"
#define SITE_GROUP	"DERIVED_IMAGE_PARMS"
#define SITE_NAME	"REFERENCE_COORD_SYSTEM_NAME"
#define SITE_INDEX	"REFERENCE_COORD_SYSTEM_INDEX"

#define MAX_PATH 1024

// Extract camera model component from PDS/VICAR file header string,
// list is in the form "(val, val, val)".
// Return 0 if okay
// ** FIX to use new PDS camera model services **
static int extract_cmod(PDSFile *f, char *key, double a[3])
{

        int status, _unit;
        char fname[MAX_PATH];
        char msg[250];

        f->get_filename(fname);
        
        status = zvunit(&_unit, "O", 1, "U_NAME", fname, NULL);
        status = zvopen(_unit, "OP", "READ", "U_FORMAT", "DOUB","OPEN_ACT", "SA", NULL);
        if (status != 1) {
            sprintf(msg, "Error opening EDR - %s", fname);
            zvmessage(msg, "");
            zabend();
            return -1;
        }
        // Populate a-vector
        status = zlget(_unit, "PROPERTY", key, a,
                     "ELEMENT", 1, "NELEMENT", -1, "FORMAT", "DOUB",
                     "PROPERTY", "GEOMETRIC_CAMERA_MODEL", "ERR_ACT", "", NULL);
        zvclose(_unit, NULL);
        if (status != 1) {
	    fprintf(stderr, "Didn't find VICAR file key %s\n", key);
	    return -1;
        }
	return 0;
}

// Extract full camera model from PDS/VICAR header,
// promoting CAHV model to CAHVOR if O & R are missing
// (duplicates code in tmproc.C)
void get_pds_cmod(PDSFile *f, CAHVOR *cmod)
{
	extract_cmod(f, CMOD_C_KEY, cmod->c);
	extract_cmod(f, CMOD_A_KEY, cmod->a);
	extract_cmod(f, CMOD_H_KEY, cmod->h);
	extract_cmod(f, CMOD_V_KEY, cmod->v);
	if (extract_cmod(f, CMOD_O_KEY, cmod->o))
		vector_copy(cmod->a, cmod->o);
	if (extract_cmod(f, CMOD_R_KEY, cmod->r))
		memset(cmod->r, 0, sizeof(cmod->r));
}

// If PDS/VICAR XYZ has ROVER_COORDINATE_SYSTEM transform,
// convert camera model from rover frame to site frame
void cmod_rov_to_site(PDSFile *f, CAHVOR *cmod)
{
        int status, _unit;
        char fname[MAX_PATH];
        char msg[250];
	    double t[3], r[4];
        f->get_filename(fname);

        status = zvunit(&_unit, "O", 1, "U_NAME", fname, NULL);
        status = zvopen(_unit, "OP", "READ", "U_FORMAT", "DOUB","OPEN_ACT", "SA", NULL);
        if (status != 1) {
            sprintf(msg, "Error opening EDR - %s", fname);
            zvmessage(msg, "");
            zabend();
        }

        // check for LANDER's case first
        status = zlget(_unit, "PROPERTY", R2S_TRANS, t,
                     "ELEMENT", 1, "NELEMENT", -1, "FORMAT", "DOUB",
                     "PROPERTY", L2S_GROUP, "ERR_ACT", "", NULL);

        status = zlget(_unit, "PROPERTY", R2S_ROT, r,
                     "ELEMENT", 1, "NELEMENT", -1, "FORMAT", "DOUB",
                     "PROPERTY", L2S_GROUP, "ERR_ACT", "", NULL);

        // check for ROVER
        if (status  != 1) { //check for ROVER
            status = zlget(_unit, "PROPERTY", R2S_TRANS, t,
                     "ELEMENT", 1, "NELEMENT", -1, "FORMAT", "DOUB",
                     "PROPERTY", R2S_GROUP, "ERR_ACT", "", NULL);

            status = zlget(_unit, "PROPERTY", R2S_ROT, r,
                     "ELEMENT", 1, "NELEMENT", -1, "FORMAT", "DOUB",
                     "PROPERTY", R2S_GROUP, "ERR_ACT", "", NULL);
        }
        
        // move the camera
        if (status  == 1) {
	    CAHVOR xcmod;
	    double t0[3] = { 0.0, 0.0, 0.0 };
	    double r0[4] = { 1.0, 0.0, 0.0, 0.0 };
        fprintf(stderr, "Moving the camera using:\n");
	    fprintf(stderr, "ORIGIN_OFFSET_VECTOR: %f, %f, %f\n",status, t[0],t[1],t[2]);
	    fprintf(stderr, "ORIGIN_ROTATION_QUATERNION: %f, %f, %f, %f\n",r[0],r[1],r[2], r[3]);
	    cmod->move(t0, r0, t, r, &xcmod);
	    *cmod = xcmod;
        }

        zvclose(_unit, NULL);
}

/// extract site number from PDS header, zero if not found
int get_pds_site(PDSFile *f)
{
        int status, _unit;
        int site_index;
        char fname[MAX_PATH];
        char msg[250];
        char val[MAX_PATH];
	fprintf(stderr, "get_pds_site()");
        f->get_filename(fname);

        status = zvunit(&_unit, "O", 1, "U_NAME", fname, NULL);
        status = zvopen(_unit, "OP", "READ", "U_FORMAT", "DOUB","OPEN_ACT", "SA", NULL);
        if (status != 1) {
            sprintf(msg, "Error opening EDR - %s", fname);
            zvmessage(msg, "");
            zabend();
            return -1;
        }

        status = zlget(_unit, "PROPERTY", SITE_NAME, val,
                     "ELEMENT", 1, "NELEMENT", -1, "FORMAT", "STRING",
                     "PROPERTY", SITE_GROUP, "ERR_ACT", "", NULL);
	if ((status != 1) || strcmp(val, "SITE_FRAME")) {
		fprintf(stderr, "Warning, XYZ reference = %s, "
			"expected SITE_FRAME\n", val);
		return 0;
	}
        status = zlget(_unit, "PROPERTY", SITE_INDEX, &site_index,
                     "ELEMENT", 1, "NELEMENT", -1, "FORMAT", "INT",
                     "PROPERTY", SITE_GROUP, "ERR_ACT", "", NULL);
	if (status != 1) {
		fprintf(stderr, "Warning, didn't find VICAR file key %s/%s\n", 
			SITE_GROUP, SITE_INDEX);
		return 0;
	}
        zvclose(_unit, NULL);
	return site_index;
}

// Lookup site vector in preprocessed SVF
void get_site_vector(const char *svf_file, int sitenum, double site[3])
{
	FILE *fp = fopen(svf_file, "r");
	if (fp == NULL) {
		fprintf(stderr, "Can't open SVF temp file %s\n", svf_file);
		return;
	}
	char buf[256];
	while (fgets(buf, sizeof(buf), fp)) {
		int snum;
		double x, y, z;
		if (sscanf(buf, "%d %lf %lf %lf", &snum, &x, &y, &z) != 4) {
			fprintf(stderr, "SVF temp file %s invalid at %s\n",
				svf_file, buf);
			break;
		}
		if (snum == sitenum) {	// found it
			site[0] = x;
			site[1] = y;
			site[2] = z;
			fclose(fp);
			return;
		}
	}
	fprintf(stderr, "Invalid/unknown site ID %d\n", sitenum);
	fclose(fp);
}
