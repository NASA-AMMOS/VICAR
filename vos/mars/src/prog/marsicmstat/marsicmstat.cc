/* marsicmstat */
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
#define MAX_INPUTS 1
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
    int q_string(struct PARBLK *p, char *name, int count, char *str[], int mode);
    int q_init(struct PARBLK *p, int pool_size, int mode);
}

void main44()
{
    int def;

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

    zvmessage("MARSICMSTAT version 1", "");

    // Get the input file list, and set up initial camera/pointing models
    // for each input.  Although we accept one and only one input, mars_setup
    // does lots of other nice things for us.

    mars_setup(nids, file_models, camera_in, pointing_in, surface_model,
	       NULL, cs,mission, instrument, homogeneous_inputs,
               MAX_NL, MAX_NS, MAX_INPUTS);
    
    int do_bounds = zvptst("BOUNDS");

    int nl = file_models[0]->getNL();
    int ns = file_models[0]->getNS();

    // Set up output to TAE

    struct PARBLK par_block;
    q_init(&par_block, P_BYTES, P_ABORT);

    if (do_bounds) {
  
	// Figure out the bounding box for the image
	// Note that we only bother checking the first band.  If either are 0
	// they both should be.

	int top = nl;
	int bottom = 0;
	int left = ns;
	int right = 0;

	int unit = file_models[0]->getUnit();

	file_models[0]->closeFile();		// just in case
	zvopen(unit, "u_format", "real", NULL);
	file_models[0]->setFileOpen(TRUE);

	SimpleImage<float> *img_line = new SimpleImage<float>(1,
						file_models[0]->getNS());

	for (int i=0; i < nl; i++) {

	    zvread(unit, img_line->linePtr(0), "LINE", i+1, "BAND", 1, NULL);

	    for (int j=0; j < ns; j++) {
		float dn = img_line->get(0, j);
		if (dn != 0.0) {
		    if (j < left)
			left = j;
		    if (j > right)
			right = j;
		    if (i < top)
			top = i;
		    if (i > bottom)
			bottom = i;
		}
	    }
	}

	// if we found nothing, set all bounds to 0 - which means -1 since we
	// add 1 below

	if (top == nl && bottom == 0 && left == ns && right == 0) {
	    top = bottom = left = right = -1;
	}

	// Make the values all 1-based
	top++;
	bottom++;
	left++;
	right++;

	// send to TAE

	q_intg(&par_block, "BOUND_TOP", 1, &top, P_ADD);
	q_intg(&par_block, "BOUND_BOTTOM", 1, &bottom, P_ADD);
	q_intg(&par_block, "BOUND_LEFT", 1, &left, P_ADD);
	q_intg(&par_block, "BOUND_RIGHT", 1, &right, P_ADD);

    }

    // Get the other output items

    double scale = file_models[0]->getCorrelationAverageScale(0.0);
    int corr_count = file_models[0]->getCorrelationPixelCount(0);

    const LblDerivedImage_typ *di = file_models[0]->getLblDerivedImage();
    double pct = 0;
    if (di->CorrelationOverlapPercentage.Valid)
	pct = di->CorrelationOverlapPercentage.Value;

    char input1[255], input2[255];
    strcpy(input1, "");
    strcpy(input2, "");

    if (di->InputProductId[0].Valid)
	strcpy(input1, di->InputProductId[0].Value);
    if (di->InputProductId[1].Valid)
	strcpy(input2, di->InputProductId[1].Value);
    char *input1p = input1;
    char *input2p = input2;

    const LblIdentification_typ *ident = file_models[0]->getLblIdentification();
    char lmst[255];
    strcpy(lmst, "");
    if (ident->LocalMeanSolarTime.Valid)
	strcpy(lmst, ident->LocalMeanSolarTime.Value);
    char *lmstp = lmst;

    q_intg(&par_block, "NLOUT", 1, &nl, P_ADD);
    q_intg(&par_block, "NSOUT", 1, &ns, P_ADD);
    q_real(&par_block, "OVR_PCT", 1, &pct, P_ADD);
    q_intg(&par_block, "CORR_COUNT", 1, &corr_count, P_ADD);
    q_real(&par_block, "AVG_SCALE", 1, &scale, P_ADD);
    q_string(&par_block, "LMST", 1, &lmstp, P_ADD);
    q_string(&par_block, "INPUT1", 1, &input1p, P_ADD);
    q_string(&par_block, "INPUT2", 1, &input2p, P_ADD);

    zvq_out(&par_block);


}

