/* mslrough */
#include "vicmain_c"

#include "mars_support.h"

#include "PigMission.h"
#include "PigFileModel.h"
#include "PigLabelModel.h"
#include "PigCameraModel.h"
#include "PigPointingModel.h"
#include "PigVector.h"
#include "PigCoordSystem.h"
#include "PigCSReference.h"
#include <stdlib.h>
#include <unistd.h>


/* buffer sizes in main program */
#define MAX_INPUTS 1
#define MAX_NL MARS_MAX_NL
#define MAX_NS MARS_MAX_NS

const unsigned short int
    SO_EU_WU__SHFT = 14, 
    SO_EU_WD__SHFT = 12, 
    SO_ED_WU__SHFT = 10, 
    SO_ED_WD__SHFT =  8, 
    SI_EU_WU__SHFT =  6,
    SI_EU_WD__SHFT =  4, 
    SI_ED_WU__SHFT =  2,
    SI_ED_WD__SHFT =  0;

// 2-bit masks
const unsigned short int
    SO_EU_WU__MSK = 3 << SO_EU_WU__SHFT, 
    SO_EU_WD__MSK = 3 << SO_EU_WD__SHFT, 
    SO_ED_WU__MSK = 3 << SO_ED_WU__SHFT, 
    SO_ED_WD__MSK = 3 << SO_ED_WD__SHFT, 
    SI_EU_WU__MSK = 3 << SI_EU_WU__SHFT,
    SI_EU_WD__MSK = 3 << SI_EU_WD__SHFT, 
    SI_ED_WU__MSK = 3 << SI_ED_WU__SHFT,
    SI_ED_WD__MSK = 3 << SI_ED_WD__SHFT;

// 1-bit masks
const unsigned char
    SO_EU_WU__MSK1 = 128, 
    SO_EU_WD__MSK1 =  64, 
    SO_ED_WU__MSK1 =  32, 
    SO_ED_WD__MSK1 =  16, 
    SI_EU_WU__MSK1 =   8,
    SI_EU_WD__MSK1 =   4, 
    SI_ED_WU__MSK1 =   2,
    SI_ED_WD__MSK1 =   1;

////////////////////////////////////////////////////////////////////////

void main44()
{

    int status, count, def;
    const size_t msgLen = 256;
    char msg[msgLen];

    int nids;
    char mission[64], instrument[64];

    // Inputs

    PigFileModel *file_models[MAX_INPUTS];
    PigCameraModel *camera_in[MAX_INPUTS];
    PigPointingModel *pointing_in[MAX_INPUTS];
    int homogeneous_inputs = TRUE;
    PigCoordSystem *cs;
    int reach_unit;

    // For buffers to read all bands of the input.

    unsigned short int  **reach;

    int nl, ns, nb;

    // Outputs

    int out_unit;
    unsigned char *good;
    unsigned char *configs=NULL;

    
    zvmessage("MARSGREACH version 1", "");

    // Get the input file, and set up initial camera/pointing models
    // for each input.  Although we accept one input only, mars_setup
    // does lots of other nice things for us.

    mars_setup(nids, file_models, camera_in, pointing_in, NULL, cs,
                mission, instrument, homogeneous_inputs,
                MAX_NL, MAX_NS, MAX_INPUTS);

    PigMission *m = PigMission::getMissionObject(mission);

    // get input image dimensions

    nl = file_models[0]->getNL();
    ns = file_models[0]->getNS();
    nb = file_models[0]->getNB();

    // Get the list of the selected input bands for processing.
    // Others are ignored.

    const int MAX_REACH_BANDS=16;  // For now 16, just a number >= 6
    int selected_bnds[MAX_REACH_BANDS], num_selected_bnds=0;
    status = zvparm("BANDS", selected_bnds, &num_selected_bnds, &def, MAX_REACH_BANDS, 0);

    // Allocate memory for input reachability.  Allocate memory 
    // only for the bands that are selected for processing.

    reach = (unsigned short int **)malloc(nb * sizeof(unsigned short int *));
    if (reach == NULL) {
	snprintf(msg, msgLen, "Unable to allocate memory for input!");
        zvmessage(msg, "");
        zabend();
    }
    for (int band = 0; band < nb; band++) {
        bool skip_this_band = true;
        for (int i = 0; (i < num_selected_bnds) && skip_this_band; i++) {
            if (band+1 == selected_bnds[i])
                skip_this_band = false;
        }
        if (skip_this_band) {
	    snprintf(msg, msgLen, "Skipping band %d!  Not selected for processing.", band+1);
	    zvmessage(msg, "");
            reach[band] = NULL;
        } else {
	    snprintf(msg, msgLen, "Processing band %d!  Selected for processing.", band+1);
	    zvmessage(msg, "");
            reach[band] = (unsigned short int *)malloc(nl * ns * sizeof(unsigned short int));
	    if (reach[band] == NULL) {
	        snprintf(msg, msgLen, "Unable to allocate memory for input band %d!", band+1);
	        zvmessage(msg, "");
	        zabend();
	    }
        }
    }

    // Read in the input reachability file.

    file_models[0]->closeFile();
    reach_unit = file_models[0]->getUnit();
    zvopen(reach_unit, "OP", "READ", "OPEN_ACT", "AS", 
           "U_FORMAT", "HALF", "U_ORG", "BSQ", NULL);
    file_models[0]->setFileOpen(TRUE);

    for (int band = 0; band < nb; band++) {
        if (reach[band] == NULL)
            continue;
	for (int line = 0; line < nl; line++) {
	    zvread(reach_unit, (reach[band]) + (line * ns),
			"BAND", band+1, "LINE", line+1, NULL);
	}
    }

    bool two_band_out = zvptst("BEST_CONF");

    // Open the output file.
    // OUT is a single 1 or 2 band image file.

    zvpcnt("OUT", &count);
    zvunit(&out_unit, "OUT", 1, NULL);
    zvopen(out_unit, "OP", "WRITE",  "OPEN_ACT", "SA",
	   "U_NS", ns,  "U_NL", nl,  "U_NB", two_band_out?2:1,
	   "U_FORMAT", "BYTE", "O_FORMAT", "BYTE",
	   "U_ORG", "BSQ", NULL);
    zvplabel(out_unit, 0, 1);

    // write output label

    PigLabelModel *labelModel = m->createLabelModel(out_unit);
    labelModel->setGReach(file_models, nids, 
                          selected_bnds, num_selected_bnds, two_band_out);

    // Allocate memory for output data.

    good = (unsigned char *)malloc(nl * ns * sizeof(unsigned char));
    if (good == NULL) {
        snprintf(msg, msgLen, "Unable to allocate memory for output.");
        zvmessage(msg, "");
        zabend();
    }
    if (two_band_out) {
        configs = (unsigned char *)malloc(nl * ns * sizeof(unsigned char));
        if (configs == NULL) {
            snprintf(msg, msgLen, "Unable to allocate memory for the optional band of the output.");
            zvmessage(msg, "");
            zabend();
        }
    }

    // Compute goodness data one pixel at a time

    unsigned short int temp, temp_pxl;
    for (int pxl = 0; pxl < nl * ns; pxl++) {

        // For each arm configuration, find the the worst instrument
        // reachability.

        unsigned short int
            so_eu_wu__wrst = 3, 
            so_eu_wd__wrst = 3, 
            so_ed_wu__wrst = 3, 
            so_ed_wd__wrst = 3, 
            si_eu_wu__wrst = 3,
            si_eu_wd__wrst = 3, 
            si_ed_wu__wrst = 3,
            si_ed_wd__wrst = 3;
        for (int band = 0; band < nb; band++) {
            if (reach[band] == NULL)
                continue;

            temp_pxl = (unsigned short int)reach[band][pxl];

            temp = (temp_pxl & SO_EU_WU__MSK) >> SO_EU_WU__SHFT;
            if (temp < so_eu_wu__wrst)  so_eu_wu__wrst = temp;

            temp = (temp_pxl & SO_EU_WD__MSK) >> SO_EU_WD__SHFT;
            if (temp < so_eu_wd__wrst)  so_eu_wd__wrst = temp;

            temp = (temp_pxl & SO_ED_WU__MSK) >> SO_ED_WU__SHFT;
            if (temp < so_ed_wu__wrst)  so_ed_wu__wrst = temp;

            temp = (temp_pxl & SO_ED_WD__MSK) >> SO_ED_WD__SHFT;
            if (temp < so_ed_wd__wrst)  so_ed_wd__wrst = temp;

            temp = (temp_pxl & SI_EU_WU__MSK) >> SI_EU_WU__SHFT;
            if (temp < si_eu_wu__wrst)  si_eu_wu__wrst = temp;

            temp = (temp_pxl & SI_EU_WD__MSK) >> SI_EU_WD__SHFT;
            if (temp < si_eu_wd__wrst)  si_eu_wd__wrst = temp;

            temp = (temp_pxl & SI_ED_WU__MSK) >> SI_ED_WU__SHFT;
            if (temp < si_ed_wu__wrst)  si_ed_wu__wrst = temp;

            temp = (temp_pxl & SI_ED_WD__MSK) >> SI_ED_WD__SHFT;
            if (temp < si_ed_wd__wrst)  si_ed_wd__wrst = temp;
        }

        // Find the arm configuration that has the best overall 
        // (for all instruments) reachability, and record the 
        // reachability value.

        unsigned short int best = so_eu_wu__wrst;
        if (so_eu_wd__wrst > best)  best = so_eu_wd__wrst;
        if (so_ed_wu__wrst > best)  best = so_ed_wu__wrst;
        if (so_ed_wd__wrst > best)  best = so_ed_wd__wrst;
        if (si_eu_wu__wrst > best)  best = si_eu_wu__wrst;
        if (si_eu_wd__wrst > best)  best = si_eu_wd__wrst;
        if (si_ed_wu__wrst > best)  best = si_ed_wu__wrst;
        if (si_ed_wd__wrst > best)  best = si_ed_wd__wrst;

        // Convert the best rechability values 0, 1, 2 or 3 to the
        // "standard" goodness values 0, 1, 3 or 5 respectively.

        good[pxl] = (unsigned char)( best*2 - (best==0 ? 0 : 1) );

        // Use bit-flags to record which arm configurations have
        // the best overall reachability.  This is needed only if
        // the second band, which is optional is requested.

        if (two_band_out) {
            if (good[pxl] == 0) {
                configs[pxl] = 0;
            } else {
                unsigned char best_configs = 0;
                if (so_eu_wu__wrst == best)  best_configs |= SO_EU_WU__MSK1;
                if (so_eu_wd__wrst == best)  best_configs |= SO_EU_WD__MSK1;
                if (so_ed_wu__wrst == best)  best_configs |= SO_ED_WU__MSK1;
                if (so_ed_wd__wrst == best)  best_configs |= SO_ED_WD__MSK1;
                if (si_eu_wu__wrst == best)  best_configs |= SI_EU_WU__MSK1;
                if (si_eu_wd__wrst == best)  best_configs |= SI_EU_WD__MSK1;
                if (si_ed_wu__wrst == best)  best_configs |= SI_ED_WU__MSK1;
                if (si_ed_wd__wrst == best)  best_configs |= SI_ED_WD__MSK1;
                configs[pxl] = best_configs;
            }
        }
    }

    // Transfer goodness data to the ouput file.

    for (int line = 0; line < nl; line++)
        zvwrit(out_unit, good + (line*ns), "BAND", 1, "LINE", line+1, NULL);
    if (two_band_out)
        for (int line = 0; line < nl; line++)
            zvwrit(out_unit, configs + (line*ns), "BAND", 2, "LINE", line+1, NULL);

    zvclose(reach_unit, NULL);
    zvclose(out_unit, NULL);

}
