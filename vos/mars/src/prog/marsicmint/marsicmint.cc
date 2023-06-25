/* marsicmint */
#include "vicmain_c"

#include <math.h>
#include <iostream>
using namespace std;

#include "mars_support.h"

#include "PigMission.h"
#include "PigFileModel.h"
#include "PigLabelModel.h"
#include "PigCameraModel.h"
#include "PigPointingModel.h"
#include "PigSurfaceModel.h"
#include "PigVector.h"
#include "RadiometryModel.h"
#include "PigCoordSystem.h"
#include "mat3.h"

#include "return_status.h"

#include "taeconf.inp"
#include "parblk.inc"
#include "pgminc.inc"

/* buffer sizes in main program */
#define MAX_INPUTS 2
#define MAX_NS 0
#define MAX_NL 0		/* arbitrary; lines are not buffered */

#ifndef MIN
#define MIN(x,y) ((x)<(y) ? (x) : (y))
#endif

#ifndef MAX
#define MAX(x,y) ((x)>(y) ? (x) : (y))
#endif

extern "C" {
    int q_real(struct PARBLK *p, char *name, int count, double *real, int mode);
    int q_intg(struct PARBLK *p, char *name, int count, int *ival, int mode);
    int q_init(struct PARBLK *p, int pool_size, int mode);
}

void main44()
{
    int count, def;

    int nids;
    char mission[64], instrument[64];
    char filename[PIG_MAX_FILENAME_SIZE+1];

    // Inputs

    PigFileModel *file_models[MAX_INPUTS];
    PigCameraModel *camera_in[MAX_INPUTS];
    PigPointingModel *pointing_in[MAX_INPUTS];
    int homogeneous_inputs = TRUE;
    PigCoordSystem *cs;
    PigSurfaceModel *surface_model;

    zvmessage("MARSICMINT version 1", "");

    // Get the input file list, and set up initial camera/pointing models
    // for each input.

    mars_setup(nids, file_models, camera_in, pointing_in, surface_model,
	       NULL, cs,mission, instrument, homogeneous_inputs,
               MAX_NL, MAX_NS, MAX_INPUTS);
    
    if (nids != 2) {
	zvmessage("MARSICMINT requires 2 inputs", "");
	zabend();
    }

    int nl = file_models[0]->getNL();
    int ns = file_models[0]->getNS();

    if (nl != file_models[1]->getNL() || ns != file_models[1]->getNS()) {
	zvmessage("Inputs must be the same size!", "");
	zabend();
    }

    float thresh;
    zvp("THRESH", &thresh, &count);

    int do_fraction = zvptst("DO_FRACTION");

    // Set up output to TAE

    struct PARBLK par_block;
    q_init(&par_block, P_BYTES, P_ABORT);

    int unit0 = file_models[0]->getUnit();

    file_models[0]->closeFile();		// just in case
    zvopen(unit0, "u_format", "real", NULL);
    file_models[0]->setFileOpen(TRUE);

    int unit1 = file_models[1]->getUnit();

    file_models[1]->closeFile();		// just in case
    zvopen(unit1, "u_format", "real", NULL);
    file_models[1]->setFileOpen(TRUE);

    SimpleImage<float> *line0 = new SimpleImage<float>(1, ns);
    SimpleImage<float> *line1 = new SimpleImage<float>(1, ns);

    long int thresh_pix = thresh * nl * ns;
    long int int_pix = 0;

    int result = 0;

    for (int i=0; i < nl; i++) {

	zvread(unit0, line0->linePtr(0), "LINE", i+1, "BAND", 1, NULL);
	zvread(unit1, line1->linePtr(0), "LINE", i+1, "BAND", 1, NULL);

	float *p0 = line0->linePtr(0);
	float *p1 = line1->linePtr(0);
	for (int j=0; j < ns; j++) {
	    if (*p0 != 0 && *p1 != 0)
		int_pix++;
	    p0++;
	    p1++;
	}
	if (int_pix >= thresh_pix) {		// got it already
	    result = 1;
	    if (!do_fraction)
	        break;
	}
	long int remaining = ((long)nl-i-1)*ns;
	if (int_pix + remaining < thresh_pix) {	// can't possibly get there
	    result = 0;
	    if (!do_fraction)
	        break;
	}
    }

    // send to TAE

    q_intg(&par_block, "RESULT", 1, &result, P_ADD);

    // Fraction

    if (do_fraction) {
	double frac = ((double)int_pix) / (((double)nl)*ns);
	q_real(&par_block, "FRACTION", 1, &frac, P_ADD);
    }

    zvq_out(&par_block);

}

