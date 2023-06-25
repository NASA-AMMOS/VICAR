/* nsytrough */
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

#include "nsyt_instruments.h"
#include "nsyt_roughness.h"


/* buffer sizes in main program */
#define MAX_INPUTS 3
#define MAX_ZIX_INPUTS 1
#define MAX_NL MARS_MAX_NL
#define MAX_NS MARS_MAX_NS

void open_inputs(int nids, PigFileModel *file_models[], int unit[3],
        int band[3], char *type, int nbands);

void computeState(RoughnessParams &params,
		  SimpleImage<double> rough[3],
		  int rough_bands,
		  double thresh[3],
		  SimpleImage<double> &state);

#define EPSILON 1e-6

////////////////////////////////////////////////////////////////////////

void main44()
{
    int i, band, line;
    int count, def;
    const size_t msgLen = 256;
    char msg[msgLen];

    int nids, uix_nids, zix_nids;
    char mission[64], instrument[64];

    // Inputs

    PigFileModel *file_models[MAX_INPUTS];
    PigFileModel *uix_file_models[MAX_INPUTS];
    PigFileModel *zix_file_models[MAX_ZIX_INPUTS];
    PigCameraModel *camera_in[MAX_INPUTS];
    PigPointingModel *pointing_in[MAX_INPUTS];
    int homogeneous_inputs = TRUE;
    PigCoordSystem *cs, *xyz_cs, *uix_cs;
    int xyz_unit[3], uix_unit[3], zix_unit[1];
    int xyz_band[3], uix_band[3], zix_band[1];

    // Outputs
    int out_unit;
    int nlo, nso;

    // User Parameters
    RoughnessParams params;

    SimpleImage<double> xyz[3];		// input XYZ
    SimpleImage<PigPoint> xyz_img;	// input XYZ copy
    SimpleImage<double> uix[3];		// input UIx
    SimpleImage<double> zix;		// input ZIx
    SimpleImage<double> rough[3];	// output roughness
    SimpleImage<double> state;		// state band

    zvmessage("NSYTROUGH version 1", "");

    // Get the input file list, and set up initial camera/pointing models
    // for each input.  Although we accept one or three inputs only, mars_setup
    // does lots of other nice things for us.

    mars_setup(nids, file_models, camera_in, pointing_in, NULL, cs,
        mission, instrument, homogeneous_inputs,
        MAX_NL, MAX_NS, MAX_INPUTS);

    PigMission *m = PigMission::getMissionObject(mission);

    params.inst = HP3;
    const char *instType = "HP3";
    if (zvptst("SEIS")) {
	params.inst = SEIS;
        instType = "SEIS";
    }
    if (zvptst("WTS")) {
	params.inst = WTS;
        instType = "WTS";
    }

    int rough_bands = 2;		// roughness bands without the status

    // For UIx we must do part of what mars_setup does for the INP parameter...

    char **uix_filenames = new char *[MAX_INPUTS];
    if (uix_filenames == NULL) {
        zvmessage("Memory error in setup, uix filename array", "");
        zabend();
    }
    mars_get_filelist("UIX", uix_nids, uix_filenames, MAX_INPUTS, FALSE);

    for (int i = 0; i < uix_nids; i++) {
        uix_file_models[i] = PigFileModel::create(uix_filenames[i]);
        if (uix_file_models[i] == NULL) {
            snprintf(msg, msgLen, "Unable to create file model for UIx input %d", i);
            zvmessage(msg, "");
            zabend();
        }
    }

    // Ditto for ZIx...

    char **zix_filenames = new char *[MAX_ZIX_INPUTS];
    if (zix_filenames == NULL) {
        zvmessage("Memory error in setup, zix filename array", "");
        zabend();
    }
    mars_get_filelist("ZIX", zix_nids, zix_filenames, MAX_ZIX_INPUTS, FALSE);

    for (int i = 0; i < zix_nids; i++) {
        zix_file_models[i] = PigFileModel::create(zix_filenames[i]);
        if (zix_file_models[i] == NULL) {
            snprintf(msg, msgLen, "Unable to create file model for ZIx input %d", i);
            zvmessage(msg, "");
            zabend();
        }
    }

    // Get coord system for input XYZ file

    PigCSReference *ref;
    file_models[0]->getDerivedImageCS(ref);
    xyz_cs = m->getCoordSystem(ref);

    snprintf(msg, msgLen, "Interpreting XYZ values using the %s coordinate frame: %s",
        xyz_cs->getFrameName(), ref->getFullName());
    zvmessage(msg, "");

    // Get coord system for input UIx file

    PigCSReference *ref2;
    uix_file_models[0]->getDerivedImageCS(ref2);
    uix_cs = m->getCoordSystem(ref2);

    snprintf(msg, msgLen, "Interpreting UIx values using the %s coordinate frame: %s",
        uix_cs->getFrameName(), ref2->getFullName());
    zvmessage(msg, "");

    // Coord system for ZIx doesn't matter; we assume it's the same frame
    // as the CS we use (because we can't translate just a Z).

    // Get the coordinate system to use.
    snprintf(msg, msgLen, "Generating Roughness using the %s coordinate frame.",
      cs->getFrameName());
    zvmessage(msg, "");

    // Get parameters

    zvparmd("POINT_EPSILON", &params.point_epsilon, &count, &def, 1, 0);
    zvparmd("SINKAGE", &params.sinkage, &count, &def, 1, 0);

    zvp("LEAF_MAX_SIZE", &params.leaf_max_size, &count);

    zvparmd("SEIS_BODY_THR", &params.seis_body_thresh,&count,&def,1,0);
    zvparmd("SEIS_FEET_THR", &params.seis_feet_thresh,&count,&def,1,0);
    zvparmd("WTS_BODY_THR", &params.wts_body_thresh,&count,&def,1,0);
    zvparmd("WTS_FEET_THR", &params.wts_feet_thresh,&count,&def,1,0);
    zvparmd("HP3_BODY_THR", &params.hp3_body_thresh,&count,&def,1,0);
    zvparmd("HP3_FEET_THR", &params.hp3_feet_thresh,&count,&def,1,0);

    zvparmd("CLOCK_RANGE", params.clock_range, &count, &def, 2, 0);
    zvparmd("CLOCK_STEP", &params.clock_step, &count, &def, 1, 0);
 
    // Default hp3 body step is half the range (so there's just 3 values...
    // each end, and the center).  That's because there's way more overlap than
    // is needed using the standard clock step.

    zvparmd("HP3_BODY_STEP", &params.hp3_body_step, &count, &def, 1, 0);
    if (count == 0) {
	params.hp3_body_step = (params.clock_range[1]-params.clock_range[0])/2;
	if (params.hp3_body_step < EPSILON)
	    params.hp3_body_step = .01;		// just make sure no inf loop
	if (params.inst == HP3) {
	    snprintf(msg,msgLen,"Setting HP3 body clock step to %f",params.hp3_body_step);
	    zvmessage(msg, "");
	}
    }

    snprintf(msg, msgLen, "Clock range: %f,%f deg,  Clock step: %f deg",
		params.clock_range[0], params.clock_range[1],
		params.clock_step);
    zvmessage(msg, "");

    zvp("MAX_WINDOW", &params.max_window_size, &count);
    zvparmd("BAD_ROUGH", &params.bad_roughness, &count, &def, 1, 0);
    zvparmd("FILTER_SCALE", &params.filter_scale, &count, &def, 1, 0);
    zvp("MIN_POINTS", &params.min_close_points, &count);
    zvparmd("X_CENTER", &params.x_center, &count, &def, 1, 0);
    zvparmd("Y_CENTER", &params.y_center, &count, &def, 1, 0);
    zvparmd("BOX_RADIUS", &params.box_radius, &count, &def, 1, 0);

    // Open input file(s).  An XYZ data could be either 1 3-band file
    // or 3 single band files

    open_inputs(nids, file_models, xyz_unit, xyz_band, "XYZ", 3);

    // Now open the UIx's properly

    open_inputs(uix_nids, uix_file_models, uix_unit, uix_band, "UIX", 3);

    if ((file_models[0]->getNL() != uix_file_models[0]->getNL()) ||
        (file_models[0]->getNS() != uix_file_models[0]->getNS())) {

        zvmessage("XYZ and UIx files are not the same size", "");
        zabend();
    }

    // Now open the ZIx's properly

    open_inputs(zix_nids, zix_file_models, zix_unit, zix_band, "ZIX", 1);

    if ((file_models[0]->getNL() != zix_file_models[0]->getNL()) ||
        (file_models[0]->getNS() != zix_file_models[0]->getNS())) {

        zvmessage("XYZ and ZIx files are not the same size", "");
        zabend();
    }

    // Open output file. OUT is a single 3 or 4 band float file

    zvpcnt("OUT", &count);
    zvunit(&out_unit, "OUT", 1, NULL);
    zvopen(out_unit, "op", "write",
         "u_ns", file_models[0]->getNS(), "u_nl",file_models[0]->getNL(),
         "u_nb", rough_bands+1,
         "open_act", "sa", "u_org", "bsq",
         "u_format", "doub", "o_format", "real", NULL);
    zvplabel(out_unit, 0, 1);

    // write output label

    PigLabelModel *labelModel = m->createLabelModel(out_unit);

    // gather all filemodels for handing over to label model
    PigFileModel *file_models_all[MAX_INPUTS*2+MAX_ZIX_INPUTS];
    int index = 0;
    for (int cnt = 0; cnt < nids; cnt++)
        file_models_all[index++] = file_models[cnt];
    for (int cnt = 0; cnt < uix_nids; cnt++)
        file_models_all[index++] = uix_file_models[cnt];
    for (int cnt = 0; cnt < zix_nids; cnt++)
        file_models_all[index++] = zix_file_models[cnt];
    int nids_all = index;

    // pick the coordinate system to use.
    {
        labelModel->setInstPlacement(file_models_all, nids_all, 
                                     "INST_ROUGHNESS_MAP", instType, cs);
    }

    // get input image dimensions
    nlo = file_models[0]->getNL();
    nso = file_models[0]->getNS();

    // Allocate memory for input XYZ, UIx, and ZIx, and output roughness.
    // The entire images must be in memory for the subroutine.

    for (i=0; i<3; i++) {
        xyz[i].alloc(nlo, nso);
        uix[i].alloc(nlo, nso);
    }
    xyz_img.alloc(nlo, nso);
    zix.alloc(nlo, nso);

    for (i=0; i < rough_bands; i++)
        rough[i].alloc(nlo, nso);

    state.alloc(nlo, nso);

    // Read in the XYZ file(s)...

    for (band = 0; band < 3; band++) {
        for (line = 0; line < nlo; line++) {
            zvread(xyz_unit[band], xyz[band].linePtr(line),
                   "BAND", xyz_band[band], "LINE", line+1, NULL);
        }
    }

    // Read in the UIx file(s)...

    for (band = 0; band < 3; band++) {
        for (line = 0; line < nlo; line++) {
            zvread(uix_unit[band], uix[band].linePtr(line),
                   "BAND", uix_band[band], "LINE", line+1, NULL);
        }
    }

    // Read in the ZIx file(s)...
    // If sinkage is non-0, we add it in to the zix here.  Saves us the trouble
    // of doing so later, in the inner loops.

    for (line = 0; line < nlo; line++) {
        zvread(zix_unit[0], zix.linePtr(line),
               "BAND", zix_band[0], "LINE", line+1, NULL);
	if (params.sinkage != 0.0) {
	    for (int samp = 0; samp < nso; samp++) {
		zix.set(line, samp, zix.get(line, samp) + params.sinkage);
	    }
	}
    }

    // Convert coord systems if necessary...

    // Note that we transfer xyz to xyz_img anyway, so no need to modify xyz.

    if (cs != xyz_cs) {
        for (line = 0; line < nlo; line++) {
            for (int samp = 0; samp < nso; samp++) {
		PigPoint old_xyz(xyz[0].get(line,samp),
				 xyz[1].get(line,samp),
				 xyz[2].get(line,samp));

                // If the point is 0,0,0 (meaning not valid), leave it alone

		PigPoint new_xyz(0,0,0);
                if (old_xyz.getX() != 0.0 ||
                    old_xyz.getY() != 0.0 ||
                    old_xyz.getZ() != 0.0) {

                    new_xyz = cs->convertPoint(old_xyz, xyz_cs);
		}
		xyz_img.set(line, samp, new_xyz);

            }
        }
    }
    else {
	// Must transfer the xyz image to xyz_img
	for (line = 0; line < nlo; line++) {
	    for (int samp = 0; samp < nso; samp++) {
		xyz_img.set(line,samp, PigPoint(xyz[0].get(line,samp),
					        xyz[1].get(line,samp),
					        xyz[2].get(line,samp)));
	    }
	}
    }

    if (cs != uix_cs) {
        for (line = 0; line < nlo; line++) {
            for (int samp = 0; samp < nso; samp++) {
		PigPoint old_uix(uix[0].get(line,samp),
				 uix[1].get(line,samp),
				 uix[2].get(line,samp));

                // If the point is 0,0,0 (meaning not valid), leave it alone

                if (old_uix.getX() != 0.0 ||
                    old_uix.getY() != 0.0 ||
                    old_uix.getZ() != 0.0) {

                    PigPoint new_uix = cs->convertPoint(old_uix, uix_cs);
		    uix[0].set(line, samp, new_uix.getX());
		    uix[1].set(line, samp, new_uix.getY());
		    uix[2].set(line, samp, new_uix.getZ());
                }
            }
        }
    }

    InstrumentShape shape;
    if (params.inst == SEIS) {

	zvmessage("******** COMPUTING SEIS BODY ********", "");

	// Compute seis footplane
	// Note that the seis footplane is rotationally symmetric about
	// the origin... so we don't need to compute any clocks.

	params.iparams.readInstrumentShape(shape, SEIS, INST_BODY);
	double no_clock[2] = {0.0, 0.0};
	computeHillRoughness(params, xyz_img, uix, zix, shape, rough[INST_BODY],
			no_clock, 1.0);

	zvmessage("******** COMPUTING SEIS FEET ********", "");

	// Compute seis footpatch.  Note that we use the SEIS normal for
	// both; it doesn't matter that much for the footpatch case.

	params.iparams.readInstrumentShape(shape, SEIS, INST_FEET);
	computeStdRoughness(params, xyz_img, uix, shape, rough[INST_FEET],
			params.clock_range, params.clock_step);

	// Compute state band

	double thresh[2];
	thresh[0] = params.seis_body_thresh;
	thresh[1] = params.seis_feet_thresh;
	computeState(params, rough, rough_bands, thresh, state);

    } else if (params.inst == WTS) {

	zvmessage("******** COMPUTING WTS BODY ********", "");

	//!!!! Skirt probably IS rotationally symmetric about its own origin...
	//!!!! but how that relates to the offset is currently unclear.
	//!!!! Right now it probably rotates around the SEIS center, but it
	//!!!! probably should rotate around its own center, making clocking
	//!!!! unnecessary.  So for now we disable clocking for this case.

	params.iparams.readInstrumentShape(shape, WTS, INST_BODY);
	double no_clock[2] = {0.0, 0.0};
	computeHillRoughness(params, xyz_img, uix, zix, shape, rough[INST_BODY],
			no_clock, 1.0);

	zvmessage("******** COMPUTING WTS FEET ********", "");

	// Compute WTS footpatch.

	params.iparams.readInstrumentShape(shape, WTS, INST_FEET);
	computeStdRoughness(params, xyz_img, uix, shape, rough[INST_FEET],
			params.clock_range, params.clock_step);

	// Compute state band

	double thresh[2];
	thresh[0] = params.wts_body_thresh;
	thresh[1] = params.wts_feet_thresh;
	computeState(params, rough, rough_bands, thresh, state);

    } else {			// HP3

	// Compute hp3 footplane.  Note that we use a special body step
	// because it needs much less clocking

	zvmessage("******** COMPUTING HP3 BODY ********", "");
	params.iparams.readInstrumentShape(shape, HP3, INST_BODY);
	computeHillRoughness(params, xyz_img, uix, zix, shape, rough[INST_BODY],
			params.clock_range, params.hp3_body_step);

	// Compute hp3 footpatch

	zvmessage("******** COMPUTING HP3 FEET ********", "");
	params.iparams.readInstrumentShape(shape, HP3, INST_FEET);
	computeStdRoughness(params, xyz_img, uix, shape, rough[INST_FEET],
			params.clock_range, params.clock_step);

	// Compute state band

	double thresh[3];
	thresh[0] = params.hp3_body_thresh;
	thresh[1] = params.hp3_feet_thresh;
	thresh[2] = 1e30;
	computeState(params, rough, rough_bands, thresh, state);
    }

    // Write out the roughness file

    // Status band first

    for (line = 0; line < nlo; line++) {
	zvwrit(out_unit, state.linePtr(line),
		"BAND", 1, "LINE", line+1, NULL);
    }

    for (band = 0; band < rough_bands; band++) {
        for (line = 0; line < nlo; line++) {
            zvwrit(out_unit, rough[band].linePtr(line),
            	"BAND", band+2, "LINE", line+1, NULL);
	}
    }

    zvclose(out_unit, NULL);

}

////////////////////////////////////////////////////////////////////////
// Open one set of inputs, either XYZ or UIx...
////////////////////////////////////////////////////////////////////////

void open_inputs(int nids, PigFileModel *file_models[], int unit[3],
        int band[3], char *type, int nbands)
{
    const size_t msgLen = 256;
    char msg[msgLen];

    if (nids == 1) {

        // Make sure file is open with U_FORMAT of DOUB to match our buffer.

        // get Unit id
        unit[0] = file_models[0]->getUnit();

        if (file_models[0]->isFileOpen())
            file_models[0]->closeFile();
        zvopen(unit[0], "op", "read", "open_act", "sa",
                    "io_act", "sa", "u_format", "doub", NULL);
        file_models[0]->setFileOpen(TRUE);

        if (file_models[0]->getNB() != nbands) {
            snprintf(msg, msgLen, "A single %s file must have %d bands", type, nbands);
            zvmessage(msg, "");
            zabend();
        }

        // Initialize xyz_unit array
        unit[2] = unit[1] = unit[0];

        // Initialize band array
        band[0] = 1;
        band[1] = 2;
        band[2] = 3;
    }
    else if (nids == 3) {
        for (int i = 0; i < 3; i++) {

            // make sure that file is open
            if (file_models[i]->isFileOpen())
                file_models[i]->closeFile();

            // get Unit id
            unit[i] = file_models[i]->getUnit();

            zvopen(unit[i], "op", "read", "open_act", "sa",
                        "io_act", "sa", "u_format", "doub", NULL);
            file_models[i]->setFileOpen(TRUE);

            if (file_models[i]->getNB() != 1) {
                snprintf(msg, msgLen, "A three-file %s must have one band each", type);
                zvmessage(msg, "");
                zabend();
            }

            // check that all files are the same size
            if ((file_models[i]->getNL() != file_models[0]->getNL()) ||
                (file_models[i]->getNS() != file_models[0]->getNS())) {
                zvmessage("Input is of different size than Input #1", "");
                zabend();
            }
            band[i] = 1;
        }

    }
    else {
	if (nbands == 3)
           snprintf(msg, msgLen, "NSYTROUGH requires either 1 3-band file or 3 single band files as input for %s", type);
	else if (nbands == 1)
           snprintf(msg, msgLen, "NSYTROUGH requires one single band file as input for %s", type);
	else
	    snprintf(msg, msgLen, "INTERNAL ERROR: nbands");

        zvmessage(msg, "");
        zabend();
    }
}

////////////////////////////////////////////////////////////////////////
// Compute the state band.  Values:
// 0 = no value
// 1 = exceeds more than one criteria
// 2 = exceeds first criterion
// 3 = exceeds second criterion
// 4 = exceeds third criterion (if present)
// 5 = within all limits (good)
////////////////////////////////////////////////////////////////////////

void computeState(RoughnessParams &params,
		  SimpleImage<double> rough[3],
		  int rough_bands,
		  double thresh[3],
		  SimpleImage<double> &state)
{
    int nl = state.getNL();
    int ns = state.getNS();
    for (int line=0; line < nl; line++) {
        for (int samp=0; samp < ns; samp++) {

	    int value = 5;

	    for (int i=0; i < rough_bands; i++) {
		double r = rough[i].get(line,samp);
		if (fabs(r - params.bad_roughness) < EPSILON) {
		    value = 0;
		    break;		// if any band is bad, result is bad
		}
		if (r > thresh[i]) {	// out of threshold
		    if (value != 5)
			value = 1;	// something else is bad too
		    else
			value = i+2;
		}
	    }
	    state.set(line, samp, (double)value);
	}
    }
}


