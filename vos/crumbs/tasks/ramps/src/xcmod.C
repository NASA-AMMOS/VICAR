// xcmod.C 1.2 03/04/10 09:40:38
/** \file
// Extract camera model from PDS file header to separate file
*/

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include "image/image.h"
#include "image/types/all.h"
#include "grape/vector_ops.h"
#include "cahvor.h"

// PDS header keys for CAHVOR camera model components
#define CMOD_C_KEY	"MODEL_COMPONENT_1"
#define CMOD_C_KEY2	"MODEL_COMPONENT_C"
#define CMOD_A_KEY	"MODEL_COMPONENT_2"
#define CMOD_A_KEY2	"MODEL_COMPONENT_A"
#define CMOD_H_KEY	"MODEL_COMPONENT_3"
#define CMOD_H_KEY2	"MODEL_COMPONENT_H"
#define CMOD_V_KEY	"MODEL_COMPONENT_4"
#define CMOD_V_KEY2	"MODEL_COMPONENT_V"
#define CMOD_O_KEY	"MODEL_COMPONENT_5"
#define CMOD_R_KEY	"MODEL_COMPONENT_6"

// Extract camera model component from PDS file header string,
// list is in the form "(val, val, val)".
// Return 0 if okay
static int extract_cmod(PDSFile *f, char *key, double a[3])
{
	char *val = f->get_value(key);
	if (val == NULL) {
		fprintf(stderr, "Didn't find PDS file key %s\n", key);
		return -1;
	}
	sscanf(val, "(%lf,%lf,%lf", &a[0], &a[1], &a[2]);
	return 0;
}

int main(int argc, char **argv)
{
	if (argc != 3) {
		fprintf(stderr, "usage: %s input-PDS-file output-cmod-file\n", 
			argv[0]);
		return 1;
	}

	// Read in the PDS file
	Image img;
	PDSFile *pds = new PDSFile;
	pds->set_image(&img);
	if (pds->read_header(argv[1])) {
		fprintf(stderr, "%s not found or not a valid PDS image\n",
			argv[1]);
		return 1;
	}

	// Get CAHVOR camera model from PDS header
	// (promote CAHV model to CAHVOR if O & R are missing)
	static CAHVOR cmod;
	if (extract_cmod(pds, CMOD_C_KEY, cmod.c))
		extract_cmod(pds, CMOD_C_KEY2, cmod.c);
	if (extract_cmod(pds, CMOD_A_KEY, cmod.a))
		extract_cmod(pds, CMOD_A_KEY2, cmod.a);
	if (extract_cmod(pds, CMOD_H_KEY, cmod.h))
		extract_cmod(pds, CMOD_H_KEY2, cmod.h);
	if (extract_cmod(pds, CMOD_V_KEY, cmod.v))
		extract_cmod(pds, CMOD_V_KEY2, cmod.v);
	if (extract_cmod(pds, CMOD_O_KEY, cmod.o))
		vector_copy(cmod.a, cmod.o);
	if (extract_cmod(pds, CMOD_R_KEY, cmod.r))
		memset(cmod.r, 0, sizeof(cmod.r));

	// setup comment and write camera model file
	char buf[1024];
	sprintf(buf, "Extracted from %s", argv[1]);
	return cmod.write(argv[2], buf);
}
