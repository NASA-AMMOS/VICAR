/* marsrad */
#include "vicmain_c"

#include "mars_support.h"

#include "PigMission.h"
#include "PigFileModel.h"
#include "PigLabelModel.h"
#include "RadiometryModel.h"
#include "PigCameraModel.h"
#include "PigPointingModel.h"

#include "lbl_image_data.h"
#include "return_status.h"

#include <stdio.h>
#include "SimpleImage.h"

// buffer sizes in main program - these should be made dynamic, eventually!
#define MAX_INPUTS 1000
#define MAX_OPEN 1		/* do one at a time... */
#define MAX_NL MARS_MAX_NL
#define MAX_NS MARS_MAX_NS

////////////////////////////////////////////////////////////////////////

void main44()
{
    int i, j, k, count;
    const size_t msgLen = 150;
    char msg[msgLen];

    int band = 1;
    int nids, nods;		// # of inputs, outputs (must match!)
    char mission[64], instrument[64];
    int is_float;
    int bits;
    int maxval;

    // Inputs

    PigFileModel *file_models[MAX_INPUTS];
    PigCameraModel *camera_in[MAX_INPUTS];		// not used
    PigPointingModel *pointing_in[MAX_INPUTS];		// not used
    PigLabelModel *labelModel = NULL;
    int homogeneous_inputs = TRUE;
    RadiometryModel *radiometric[MAX_INPUTS];
    PigCoordSystem *dummy_cs;
    SimpleImage<short int> *short_int_images[MAX_INPUTS];
    memset(short_int_images, 0, sizeof(short_int_images));
    SimpleImage<float> *float_images[MAX_INPUTS];
    memset(float_images, 0, sizeof(float_images));

    // Outputs

    char *filenames[MAX_INPUTS];
    int out_units[MAX_INPUTS];
    LblImageData_typ ImageData;

    zvmessage("MARSRAD version 1", "");

    // Get the input file list, and set up initial camera/pointing models
    // for each input (not used), and the radiometry model (which is).

    mars_setup(nids, file_models, camera_in, pointing_in, radiometric, dummy_cs,
                mission, instrument,homogeneous_inputs,
                MAX_NL, MAX_NS, MAX_INPUTS);

    // Get the mission pointer.
    PigMission *m = PigMission::getMissionObject(mission);

    // Get the output file list

    mars_get_filelist("OUT", nods, filenames, MAX_INPUTS, TRUE);

    if (nids != nods) {
	snprintf(msg, msgLen, "Number of inputs (%d) must match # of outputs (%d).",
			nids, nods);
	zvmessage(msg, "");
	zabend();
    }

    // get parameter overrides if any

    int band_count = 1;
    zvp("BAND", &band, &count);
    if (count == 0) {
        // No input band specified; process all bands.
        band_count = file_models[0]->getNB();
	band = 1;
        snprintf(msg, msgLen, "Number of bands to be processed is (%d)", band_count);
        zvmessage(msg, "");
    }
    else {
        // error check band number
        if (band > file_models[0]->getNB()) {
            snprintf(msg, msgLen, "Input band (%d) is greater than number of bands in "
                    "input image. Band set to 1.", band);
            zvmessage(msg, "");
            band = 1;
        }
    }

    is_float = zvptst("REAL");		// output format

    if (is_float) {		// Check DNSCALE and warn if bad
	double scale = radiometric[0]->getDnScalingFactor();
	if (scale != 1.0) {
	    zvmessage("WARNING: DNSCALE should normally be 1.0 when REAL "
                      "format is used", "");
	    zvmessage("Verify this is what you want.  Specified value IS used.",
                      "");
	}
    }

    zvp("BITS", &bits, &count);
    if (count == 0)
	bits = 0;				// not set
    maxval = 0;
    if (!is_float && bits == 0)
	bits = 15;				// max for short int
    if (bits > 0 && bits <= 31)			// 32 effectively means no limit
	maxval = (1<<bits) - 1;

    // Print out input status from labels

    mars_print_inputs(nids, pointing_in, camera_in, file_models, 
                      homogeneous_inputs, mission, instrument);

    // Since we process only one at a time, the loops are much simpler than
    // e.g. marsmos.  If MAX_OPEN is not 1 for some reason, the "pass loops"
    // can be put back in here.

    for (k = 0; k < nids; k++) {
        if (radiometric[0] != NULL) {
	    radiometric[0]->print();
        }

        // open output file.  Make sure we set up the right input as
        // primary input, so the labels will be right.

        zvselpiu(file_models[k]->getUnit());
        zvunit(&out_units[k], "CORRECTED", k, "U_NAME", filenames[k], NULL);
        char *fmt = "HALF";
        if (is_float)
            fmt = "REAL";
        zvopen(out_units[k], "OP", "WRITE", "U_FORMAT", "REAL", 
               "OPEN_ACT", "AS", "U_NB", band_count, "U_ORG", "BSQ", "O_FORMAT", fmt,
                NULL);
        zvplabel(out_units[k], 0, 1);

	// Write the rad labels.
	if (m)
	    labelModel = m->createLabelModel(out_units[k]);
	if (labelModel) {
	    labelModel->setRadiometric(file_models, nids,
				       &radiometric[k], 1);
	    int b = bits;
	    if (b == 0)
		b = 15;			// backward compatibility
	    labelModel->setBitMask(b);	// removes label for float type
	}

	// Read the input into memory, and apply the radiometric correction
	for (int n = 0; n < band_count; n++) {
            mars_read_inputs(k, k, file_models, float_images, MAX_NL, MAX_NS, 
                             n+band,  radiometric);

	    // Clamp the values if needed
            if (!is_float && bits > 0) {
                for (j=0; j<file_models[k]->getNL(); j++) {
                    for (i=0; i<file_models[k]->getNS(); i++) {
                        float dn = float_images[k]->get(j, i);
                        if (dn > (float)maxval) 
                            float_images[k]->set(j, i, (float)maxval);
                        if (dn < 0) 
                            float_images[k]->set(j, i, 0);
                    }
                }
            }

            // Now simply write the file back out
            for (j=0; j<file_models[k]->getNL(); j++) {
		zvwrit(out_units[k], float_images[k]->linePtr(j), "BAND", n+1, 
                       "LINE", j+1, NULL);
            }
        }

   	//Free up all dynamically allocated memory for simple image
	float_images[k]->free();

	// And close the file
	zvclose(out_units[k], "CLOS_ACT", "FREE", NULL);
    }
}

