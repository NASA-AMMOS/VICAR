/* nsytfoot */
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
#include "nsyt_utils.h"

#include <iostream>

/* buffer sizes in main program */
#define MAX_INPUTS 3
#define MAX_NL MARS_MAX_NL
#define MAX_NS MARS_MAX_NS

void open_inputs(int nids, PigFileModel *file_models[], int unit[3],
        int band[3], char *type);

static std::vector<SampleCircle> plot_points_of_instrument_feet(
    InstrumentParams &params,
    const InsightInstrument instrument_type,
    int do_body)
{
    std::vector<SampleCircle> circles;
    if (instrument_type == SEIS)
    {
	if (do_body) {
	    InstrumentShape shape;
	    params.readInstrumentShape(shape, SEIS, INST_BODY);
	    circles = shape.circles;
	} else {
            // NOTE: We just want the feet and not the tether.
            params.plot_seis_feet(circles);
	}
    }
    else if (instrument_type == WTS)
    {
	if (do_body) {
	    InstrumentShape shape;
	    params.readInstrumentShape(shape, WTS, INST_BODY);
	    circles = shape.circles;
	} else {
            params.plot_wts_feet(circles);
	}
    }
    else
    {
        assert(instrument_type == HP3);

	if (do_body) {
	    InstrumentShape shape;
	    params.readInstrumentShape(shape, HP3, INST_BODY);
	    circles = shape.circles;
	} else {
            params.plot_hp3_narrow_feet_pair(circles);
            params.plot_hp3_wide_feet_pair(circles);
	}
    }

    return circles;
}

void main44()
{
    int i, band, line;
    int count, def;
    const size_t msgLen = 256;
    char msg[msgLen];

    int nids;
    char mission[64], instrument[64];

    // Inputs

    PigFileModel *file_models[MAX_INPUTS];
    PigCameraModel *camera_in[MAX_INPUTS];
    PigPointingModel *pointing_in[MAX_INPUTS];
    int homogeneous_inputs = TRUE;
    PigCoordSystem *cs, *xyz_cs;
    int xyz_unit[3];
    int xyz_band[3];

    // Outputs
    int out_unit;
    int nlo, nso;

    // User Parameters
    InsightInstrument inst;
    InstrumentParams iparams;
    double point_epsilon;
    double clock_range[2];
    double clock_step;
    double delta_wts_range[2];
    double delta_wts_step;

    int do_both;

    SimpleImage<double> input_xyz[3];
    SimpleImage<unsigned char> output_mask;

    zvmessage("NSYTFOOT version 1", "");

    // Get the input file list, and set up initial camera/pointing models
    // for each input.  Although we accept one or three inputs only, mars_setup
    // does lots of other nice things for us.

    mars_setup(nids, file_models, camera_in, pointing_in, NULL, cs,
        mission, instrument, homogeneous_inputs,
        MAX_NL, MAX_NS, MAX_INPUTS);

    PigMission *m = PigMission::getMissionObject(mission);

    // Get coord system for input XYZ file

    PigCSReference *ref;
    file_models[0]->getDerivedImageCS(ref);
    xyz_cs = m->getCoordSystem(ref);

    snprintf(msg, msgLen, "Interpreting XYZ values using the %s coordinate frame: %s",
        xyz_cs->getFrameName(), ref->getFullName());
    zvmessage(msg, "");

    // Get the coordinate system to use.
    snprintf(msg, msgLen, "Generating footprints using the %s coordinate frame.",
      cs->getFrameName());
    zvmessage(msg, "");

    do_both = FALSE;
    inst = HP3;
    if (zvptst("SEIS")) {
        inst = SEIS;
    }
    else if (zvptst("WTS")) {
        inst = WTS;
    }
    else if (zvptst("BOTH")) {
	inst = SEIS;
	do_both = TRUE;
    }

    zvparmd("POINT_EPSILON", &point_epsilon, &count, &def, 1, 0);

    zvparmd("CLOCK_RANGE", clock_range, &count, &def, 2, 0);
    zvparmd("CLOCK_STEP", &clock_step, &count, &def, 1, 0);

    // If delta wts range/step (clock for WTS in delta mode only) are not
    // given, default them to clock_range/step.

    zvparmd("DELTA_WTS_RANGE", delta_wts_range, &count, &def, 2, 0);
    if (count != 2) {
	delta_wts_range[0] = clock_range[0];
	delta_wts_range[1] = clock_range[1];
	if (do_both) {	// don't bother reporting if not
	    snprintf(msg, msgLen, "DELTA_WTS_RANGE defaulted to %f,%f",
		delta_wts_range[0], delta_wts_range[1]);
	    zvmessage(msg, "");
	}
    }
    zvparmd("DELTA_WTS_STEP", &delta_wts_step, &count, &def, 1, 0);
    if (count != 1) {
	delta_wts_step = clock_step;
	if (do_both) {	// don't bother reporting if not
	    snprintf(msg, msgLen, "DELTA_WTS_STEP defaulted to %f",
		delta_wts_step);
	    zvmessage(msg, "");
	}
    }

    snprintf(msg, msgLen, "Clock range: %f,%f deg,  Clock step: %f deg",
		clock_range[0], clock_range[1],
		clock_step);
    zvmessage(msg, "");

    // Open input file(s).  An XYZ data could be either 1 3-band file
    // or 3 single band files

    open_inputs(nids, file_models, xyz_unit, xyz_band, "XYZ");

    // Open output file. OUT is a single 1-band byte file

    zvpcnt("OUT", &count);
    zvunit(&out_unit, "OUT", 1, NULL);
    zvopen(out_unit, "op", "write",
         "u_ns", file_models[0]->getNS(), "u_nl",file_models[0]->getNL(),
         "u_nb", 1,
         "open_act", "sa", "u_org", "bsq",
         "u_format", "byte", "o_format", "byte", NULL);
    zvplabel(out_unit, 0, 1);

    // write output label

    PigLabelModel *labelModel = m->createLabelModel(out_unit);

    // gather all filemodels for handing over to label model
    int nids_all = nids;
    PigFileModel *file_models_all[MAX_INPUTS*2];
    for (int cnt = 0; cnt < nids; cnt++)
        file_models_all[cnt] = file_models[cnt];

    // pick the coordinate system to use.
    {
        labelModel->setMasked(file_models_all, nids_all);
    }

    // get input image dimensions
    nlo = file_models[0]->getNL();
    nso = file_models[0]->getNS();

    for (i=0; i<3; i++) {
        input_xyz[i].alloc(nlo, nso);
    }

    output_mask.alloc(nlo, nso);
    output_mask.zero();

    // Read in the XYZ file(s)...

    for (band = 0; band < 3; band++) {
        for (line = 0; line < nlo; line++) {
            zvread(xyz_unit[band], input_xyz[band].linePtr(line),
                   "BAND", xyz_band[band], "LINE", line+1, NULL);
        }
    }

    // Convert coord systems if necessary...

    if (cs != xyz_cs) {
        for (line = 0; line < nlo; line++) {
            for (int samp = 0; samp < nso; samp++) {
                PigPoint old_xyz(input_xyz[0].get(line,samp),
                                 input_xyz[1].get(line,samp),
                                 input_xyz[2].get(line,samp));

                // If the point is 0,0,0 (meaning not valid), leave it alone

                if (old_xyz.getX() != 0.0 ||
                    old_xyz.getY() != 0.0 ||
                    old_xyz.getZ() != 0.0) {

                    PigPoint new_xyz = cs->convertPoint(old_xyz, xyz_cs);
                    input_xyz[0].set(line, samp, new_xyz.getX());
                    input_xyz[1].set(line, samp, new_xyz.getY());
                    input_xyz[2].set(line, samp, new_xyz.getZ());
                }
            }
        }
    }

    iparams.readInstrumentParams();

    // Find the starting position

    double xy[2], ls[2];

    zvparmd("XY", xy, &count, &def, 2, 0);
    if (count == 2) {		// XY given, find the LS
	zvpcnt("LS", &count);
	if (count != 0) {
	    zvmessage("Can't give both XY and LS", "");
	    zabend();
	}
	double close = 1e38;
	for (int ll=0; ll < input_xyz[0].getNL(); ll++) {
	    for (int ss=0; ss < input_xyz[0].getNS(); ss++) {
		double xx = input_xyz[0].get(ll,ss);
		double yy = input_xyz[1].get(ll,ss);
		double zz = input_xyz[2].get(ll,ss);
		if (xx == 0.0 && yy == 0.0 && zz == 0.0)
		    continue;			// skip point
		double dist = (xy[0]-xx)*(xy[0]-xx) + (xy[1]-yy)*(xy[1]-yy);
		if (dist < close) {
		    ls[0] = ll;
		    ls[1] = ss;
		    close = dist;
		}
	    }
	}
	if (sqrt(close) > point_epsilon) {
	    snprintf(msg, msgLen, "No pixel found within point_epsilon (%f) of given XY coordinate", point_epsilon);
	    zvmessage(msg, "");
	    zabend();
	}
    } else {			// XY not given, use LS
        zvparmd("LS", ls, &count, &def, 2, 0);
printf("count=%d def=%d\n", count, def);	//!!!!
	if (count != 2) {
	    zvmessage("One of XY or LS must be given!", "");
	    zabend();
	}
	double xx = input_xyz[0].get(ls[0],ls[1]);
	double yy = input_xyz[1].get(ls[0],ls[1]);
	double zz = input_xyz[2].get(ls[0],ls[1]);
	if (xx == 0.0 && yy == 0.0 && zz == 0.0) {
	    zvmessage("Given LS point has no XY value!", "");
	    zabend();
	}
	xy[0] = xx;
	xy[1] = yy;
    }

    // Now actually do the work.  There's a loop here to cover the "both" case.

    int nloops = 1;
    if (do_both) nloops = 2;

    int do_body = zvptst("DO_BODY");

    for (int loop=0; loop<nloops; loop++) {
	if (loop == 1) {	// for "both" case
	    inst = WTS;
	    clock_range[0] = delta_wts_range[0];
	    clock_range[1] = delta_wts_range[1];
	    clock_step = delta_wts_step;
	}

	const std::vector<SampleCircle> feet =
		plot_points_of_instrument_feet(iparams, inst, do_body);

        // Convert radial and cross-radial offsets to x and y offsets
        // if inst is WTS. If it is not WTS, then offset_x and offset_y
        // are 0.0.
        double offset_x = iparams.getOffsetX(xy[0], xy[1], inst);
        double offset_y = iparams.getOffsetY(xy[0], xy[1], inst);

	for (double clock = clock_range[0];
		    clock <= clock_range[1];
		    clock += clock_step) {

	    const auto new_feet = iparams.rotateInstrument(
	        feet, xy[0], xy[1], inst, clock);

            //apply offsets if inst is WTS
            const auto circles = iparams.applyWTSOffsets(new_feet, offset_x, 
                offset_y, inst);

	    FOR_ITERATOR(circle_it,circles) {

		const auto circle = *circle_it;
		double x = circle.x + xy[0];
		double y = circle.y + xy[1];

		for (int l=0; l<output_mask.getNL(); l++) {
		    for (int s=0; s<output_mask.getNS(); s++) {
			double xx=input_xyz[0].get(l,s);
			double yy=input_xyz[1].get(l,s);
			if ((xx-x)*(xx-x)+(yy-y)*(yy-y) <
				circle.radius*circle.radius) {
			    output_mask.set(l,s,255-loop);
			}
		    }
		}
	    }
	}
    }

    for (line = 0; line < nlo; line++) {
        zvwrit(out_unit, output_mask.linePtr(line),
                "BAND", 1, "LINE", line+1, NULL);
    }

    zvclose(out_unit, NULL);

}

////////////////////////////////////////////////////////////////////////
// Open one set of XYZ inputs
////////////////////////////////////////////////////////////////////////

void open_inputs(int nids, PigFileModel *file_models[], int unit[3],
        int band[3], char *type)
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

        if (file_models[0]->getNB() != 3) {
            snprintf(msg, msgLen, "A single %s file must have three bands", type);
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
        snprintf(msg, msgLen, "NSYTTILT requires either 1 3-band file or 3 single band files as input for %s", type);
        zvmessage(msg, "");
        zabend();
    }
}

