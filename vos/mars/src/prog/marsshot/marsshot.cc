/* marsshot */
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
#define MAX_NS 65536
#define MAX_NL 65536		/* arbitrary; lines are not buffered */

#ifndef MIN
#define MIN(x,y) ((x)<(y) ? (x) : (y))
#endif

#ifndef MAX
#define MAX(x,y) ((x)>(y) ? (x) : (y))
#endif

extern "C" {
    int q_real(struct PARBLK *p, char *name, int count, double *real, int mode);
    int q_init(struct PARBLK *p, int pool_size, int mode);
}

void main44()
{
    int i, j;
    int status, count, def;
    const size_t msgLen = 150;
    char msg[msgLen];

    int nids;
    char mission[64], instrument[64];
    char filename[PIG_MAX_FILENAME_SIZE+1];
    int nl, ns, nb;

    // Inputs

    PigFileModel *file_models[MAX_INPUTS];
    PigFileModel *file_models_nonav[MAX_INPUTS];
    PigCameraModel *camera_in[MAX_INPUTS];
    PigCameraModel *camera_in_nonav[MAX_INPUTS];
    PigPointingModel *pointing_in[MAX_INPUTS];
    PigPointingModel *pointing_in_nonav[MAX_INPUTS];
    double FOV[MAX_INPUTS];				
    int homogeneous_inputs = TRUE;
    PigCoordSystem *cs;
    PigSurfaceModel *surface_model;

    // Outputs

    double out_line, out_samp;
    double image_pos[2];

    // User Parameters

    double pointing_params[PIG_MAX_PARAMS], orig_params[PIG_MAX_PARAMS];
    double raw_pointing_params[PIG_MAX_PARAMS];
    int num_params, num_orig_params;
    double hotspot[2];		//line, samp

    zvmessage("MARSSHOT version 1", "");

    // Get the input file list, and set up initial camera/pointing models
    // for each input.  Although we accept one and only one input, mars_setup
    // does lots of other nice things for us.

    mars_setup(nids, file_models, camera_in, pointing_in, surface_model,
	       NULL, cs,mission, instrument, homogeneous_inputs,
               MAX_NL, MAX_NS, MAX_INPUTS);
    
    int interp = zvptst("INTERP");

    if (!interp && nids != 1) {
	zvmessage("MARSSHOT requires exactly 1 inpupt image for no interp", "");
	zabend();
    }
    if (interp && nids != 2) {
	zvmessage("MARSSHOT requires exactly 2 input images for interp", "");
	zabend();
    }

    // If we're in interp mode, we have to read the *original* pointing params
    // (without the nav file).  This is a bit challenging in the framework.
    // Then we compute a delta pointing param for each input, between the
    // nav and nonav pointings.  rparams is raw, pparams is pointed, dparams
    // is the delta

    double rparams1[PIG_MAX_PARAMS], rparams2[PIG_MAX_PARAMS];
    double pparams1[PIG_MAX_PARAMS], pparams2[PIG_MAX_PARAMS];
    double dparams1[PIG_MAX_PARAMS], dparams2[PIG_MAX_PARAMS];
    int d1 = 0, d2 = 0;
    if (interp) {

	for (int i=0; i < nids; i++) {		// better be nids=2!
	    file_models_nonav[i] = PigFileModel::create(
						file_models[i]->getFilename());
	    if (file_models_nonav[i] == NULL) {
		snprintf(msg, msgLen, "Unable to create nonav file model for input %d",i);
		zvmessage(msg, "");
		zabend();
	    }
	    camera_in_nonav[i] = PigCameraModel::create(file_models_nonav[i],
							NULL);
	    if (camera_in_nonav[i] == NULL) {
		snprintf(msg, msgLen, "Unable to create nonav cmod for input %d", i);
		zvmessage(msg, "");
		zabend();
	    }
	    pointing_in_nonav[i] = PigPointingModel::create(camera_in_nonav[i],
			file_models_nonav[i], NULL, true);
	    if (pointing_in_nonav[i] == NULL) {
		snprintf(msg, msgLen, "Unable to create nonav pointing for input %d", i);
		zvmessage(msg, "");
		zabend();
	    }
            pointing_in_nonav[i]->pointCamera(file_models_nonav[i]);

	}

	// Now figure out how to interp between the two

	int n1, n2, d1, d2;

	n1 = pointing_in[0]->getPointingParamCount();
	n2 = pointing_in_nonav[0]->getPointingParamCount();
	if (n1 != n2) {
	    zvmessage("# of pointing params differ for first input", "");
	    zvmessage("Nav file must have changed pointing model type, which is not allowed", "");
	    zabend();
	}
	pointing_in[0]->getPointingParameters(pparams1, PIG_MAX_PARAMS);
	pointing_in_nonav[0]->getPointingParameters(rparams1, PIG_MAX_PARAMS);
	for (int i=0; i < n1; i++) {
	    dparams1[i] = pparams1[i] - rparams1[i];
	}
	d1 = n1;

	n1 = pointing_in[1]->getPointingParamCount();
	n2 = pointing_in_nonav[1]->getPointingParamCount();
	if (n1 != n2) {
	    zvmessage("# of pointing params differ for second input", "");
	    zvmessage("Nav file must have changed pointing model type, which is not allowed", "");
	    zabend();
	}
	pointing_in[1]->getPointingParameters(pparams2, PIG_MAX_PARAMS);
	pointing_in_nonav[1]->getPointingParameters(rparams2, PIG_MAX_PARAMS);
	for (int i=0; i < n1; i++) {
	    dparams2[i] = pparams2[i] - rparams2[i];
	}
	d2 = n1;

	if (d1 != d2) {
	    zvmessage("# of pointing params differ between first and second input", "");
	    zvmessage("which is not allowed in this program", "");
	    zabend();
	}
    }

    // Get parameters

    hotspot[0] = 0.0;
    hotspot[1] = 0.0;
    int set = FALSE;

    zvpcnt("INSTRUMENT", &count);
    if (count == 1) {		// instrument given

// Supercam values below per email from Olivier Gasnault, 2020-08-28.
// Yes, all three are the same!

	if (zvptst("SCAM-LIBS")) {
	    hotspot[0] = 1055.0;	// line
	    hotspot[1] = 1060.0;	// samp
	} else if (zvptst("SCAM-IR")) {
	    hotspot[0] = 1055.0;	// line
	    hotspot[1] = 1060.0;	// samp
	} else if (zvptst("SCAM-OTHER")) {
	    hotspot[0] = 1055.0;	// line
	    hotspot[1] = 1060.0;	// samp
	} else if (zvptst("CCAM-LIBS")) {
	    hotspot[0] = 492.0;		// line
	    hotspot[1] = 521.0;		// samp
	} else {
	    zvmessage("Internal error - unrecognized instrument", "");
	    zabend();		// PDF should prevent this from happening
	}
	set = TRUE;
    }

    zvpcnt("HOTSPOT", &count);
    if (count == 2) {			// don't allow just one...
	zvparmd("HOTSPOT", hotspot, &count, &def, 2, 0);
	if (set) {
	    zvmessage("Hotspot overridden via command line", "");
	}
    }

    snprintf(msg, msgLen, "Hotspot is line = %f, samp = %f in sensor coordinates",
	hotspot[0], hotspot[1]);
    zvmessage(msg, "");

    // Subtract 1 to make hotspot 0-based

    hotspot[0] -= 1.0;
    hotspot[1] -= 1.0;

    // Convert hotspot to actual image coordinates

    int dx = file_models[0]->getFirstLineSample(1) - 1;
    int dy = file_models[0]->getFirstLine(1) - 1;
    double hscale = file_models[0]->getDownsampleXFactor(1.0);
    double vscale = file_models[0]->getDownsampleYFactor(1.0);

    // Ths scale *should* never be 0.  However, inexplicably, the MSL
    // CCAM RMI sets these values to UNK... which shows up as 0.  (not the
    // default 1 because there *is* a value, but it can't be converted).
    // So if the value we got it 0, quietly set it to 1 instead.

    if (hscale == 0.0) hscale = 1.0;
    if (vscale == 0.0) vscale = 1.0;

    hotspot[0] = (hotspot[0]-dy) / vscale;
    hotspot[1] = (hotspot[1]-dx) / hscale;

    snprintf(msg, msgLen, "Hotspot is line = %f, samp = %f in actual image coordinates",
	hotspot[0]+1.0, hotspot[1]+1.0);		// report as 1-based
    zvmessage(msg, "");

    // Save a copy of the original cmod

    PigCameraModel *orig_cmod = camera_in[0]->clone();

    num_orig_params = pointing_in[0]->getPointingParamCount();
    pointing_in[0]->getPointingParameters(orig_params, PIG_MAX_PARAMS);

    // Now point the camera per the given pointing

    zvparmd("POINTING",raw_pointing_params, &num_params, &def,PIG_MAX_PARAMS,0);

    if (num_params > num_orig_params) {
	zvmessage("too many params specified in POINTING", "");
	zabend();
    }

    // Copy over for no-interp case, or if not enough params specified
    for (int i=0; i < num_params; i++) {
	pointing_params[i] = raw_pointing_params[i];
    }

    // If requested, interpolate the pointing parameters.  We do this by
    // applying the delta for input 1 (effectively applying the navtable
    // correction to it), and then use the ratio of request to input 1
    // compared to input 2 vs input 1 for raw, to apply the same amount to
    // the nav delta.

    if (interp) {

	for (int i=0; i < num_params; i++) {
	    double denom = rparams2[i] - rparams1[i];
	    double ratio = 0.0;
	    if (denom != 0)
	        ratio = (raw_pointing_params[i] - rparams1[i]) / denom;

	    pointing_params[i] = raw_pointing_params[i] +
				 dparams1[i] +
				 ratio * (dparams2[i] - dparams1[i]);
	}
    }

    snprintf(msg, msgLen, "Pointing parameters, type = %s\n",
			pointing_in[0]->getModelName());
    zvmessage(msg, "");
    if (interp)
        zvmessage("Param Name           Original   Revised    Interp", "");
    else
        zvmessage("Param Name           Original   Revised", "");
    for (int i=0; i < num_orig_params; i++) {
	char val1[20], val2[20];
	if (i < num_params) {
	    snprintf(val1, 20, "%10.5f", raw_pointing_params[i]);
	    snprintf(val2, 20, "%10.5f", pointing_params[i]);
	} else {
	    snprintf(val1, 20, "Unchanged ");
	    snprintf(val2, 20, "Unchanged ");
	}
	if (interp)
	    snprintf(msg, msgLen, "%20s %10.5f %s %s",
		pointing_in[0]->getPointingParamName(i),
		orig_params[i], val1, val2);
	else
	    snprintf(msg, msgLen, "%20s %10.5f %s",
		pointing_in[0]->getPointingParamName(i), orig_params[i], val1);
	zvmessage(msg, "");
    }

    zvmessage("", "");

    if (num_params != 0)
	pointing_in[0]->setPointingParameters(pointing_params, num_params);

    // Do the projection...

    PigPoint origin;
    PigVector look;
    PigPoint surf_pt;
    int hits;

    camera_in[0]->LStoLookVector(hotspot[0], hotspot[1], origin, look, cs);
    hits = surface_model->intersectRay(origin, look, surf_pt);
    orig_cmod->XYZtoLS(surf_pt, (hits <= 0), &image_pos[0], &image_pos[1], cs);

    snprintf(msg, msgLen, "Adjusting for file offset, y=%lf, x=%lf",
		file_models[0]->getYOffset(), file_models[0]->getXOffset());
    zvmessage(msg, "");

    image_pos[0] -= file_models[0]->getYOffset();
    image_pos[1] -= file_models[0]->getXOffset();

    // Convert to 1-based

    image_pos[0] += 1.0;
    image_pos[1] += 1.0;

    // Report results

    snprintf(msg, msgLen,"LINE = %lf", image_pos[0]);
    zvmessage(msg, "");
    snprintf(msg, msgLen, "SAMP = %lf", image_pos[1]);
    zvmessage(msg, "");
    snprintf(msg, msgLen, "%lf %lf", image_pos[0], image_pos[1]);
    zvmessage(msg, "");
      
    // Output to TAE

    struct PARBLK par_block;
    q_init(&par_block, P_BYTES, P_ABORT);
    q_real(&par_block, "IMAGE_POS", 2, image_pos, P_ADD);
    zvq_out(&par_block);
}

