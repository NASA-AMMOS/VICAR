/* nsyttilt */
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
#include "nsyt_tilt.h"
#include "nsyt_utils.h"

#include <iostream>

/* buffer sizes in main program */
#define MAX_INPUTS 3
#define MAX_NL MARS_MAX_NL
#define MAX_NS MARS_MAX_NS

void open_inputs(int nids, PigFileModel *file_models[], int unit[3],
        int band[3], char *type);

static std::vector<SampleCircle> plot_points_of_instrument_feet(
    const InstrumentParams &params,
    const InsightInstrument instrument_type)
{
    std::vector<SampleCircle> circles;
    if (instrument_type == SEIS)
    {
        // NOTE: We just want the feet and not the tether.
        params.plot_seis_feet(circles);
    }
    else if (instrument_type == WTS)
    {
        params.plot_wts_feet(circles);
    }
    else
    {
        assert(instrument_type == HP3);

        params.plot_hp3_narrow_feet_pair(circles);
        params.plot_hp3_wide_feet_pair(circles);
    }

    return circles;
}

/** Return state image.
 *
 * 0 = no value
 * 1 = n/a
 * 2 = n/a
 * 3 = exceeds tilt threshold
 * 4 = n/a
 * 5 = tilt within limits (good)
 */
static SimpleImage<double> compute_state(
    const TiltParams &params,
    SimpleImage<double> &input_max_tilt)  // Not const because "get()" isn't.
{
    SimpleImage<double> output_state(input_max_tilt.getNL(),
                                     input_max_tilt.getNS());

    for (int line = 0; line < output_state.getNL(); line++)
    {
        for (int samp = 0; samp < output_state.getNS(); samp++)
        {
            const double tilt = input_max_tilt.get(line, samp);
	    double state = 0;		// no-data value
	    if (tilt != params.bad_tilt)
		state = (tilt > params.tilt_threshold) ? 3. : 5.;

            output_state.set( line, samp, state);
        }
    }

    return output_state;
}

void main44()
{
    int i, band, line, samp;
    int count, def;
    int MSG_LEN = 256;
    char msg[MSG_LEN];

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
    int uix_out_unit, zix_out_unit;
    int upx_out_unit, xpx_out_unit;

    // User Parameters
    TiltParams params;
    int delta_tilt;

    SimpleImage<double> input_xyz[3];
    SimpleImage<double> output_min_max_tilt[2];
    SimpleImage<double> output_avg_normal[3];
    SimpleImage<double> output_z;
    SimpleImage<double> output_upx[3];
    SimpleImage<double> output_xpx[3];

    zvmessage("NSYTTILT version 2020-05-26", "");

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

    snprintf(msg, MSG_LEN, "Interpreting XYZ values using the %s coordinate frame: %s",
        xyz_cs->getFrameName(), ref->getFullName());
    zvmessage(msg, "");

    // Get the coordinate system to use.
    snprintf(msg, MSG_LEN, "Generating tilt using the %s coordinate frame.",
      cs->getFrameName());
    zvmessage(msg, "");

    delta_tilt = zvptst("DELTA");

    params.inst = HP3;
    const char *instType = "HP3";
    if (zvptst("SEIS"))
    {
        params.inst = SEIS;
        instType = "SEIS";
    }
    else if (zvptst("WTS"))
    {
        params.inst = WTS;
        instType = "WTS";
    }

    if (delta_tilt && params.inst != SEIS) {
	zvmessage("Delta-tilt mode can only be used with SEIS instrument","");
	zabend();
    }

    const int num_tilt_bands = 2;

    zvparmd("POINT_EPSILON", &params.point_epsilon, &count, &def, 1, 0);

    zvp("LEAF_MAX_SIZE", &params.leaf_max_size, &count);

    zvparmd("TILT_THRESHOLD", &params.tilt_threshold, &count, &def, 1, 0);
    zvparmd("SINKAGE", &params.possible_sinkage, &count, &def, 1, 0);
    zvp("FOOT_WINDOW", &params.foot_window, &count);
    zvparmd("BAD_TILT", &params.bad_tilt, &count, &def, 1, 0);
    zvparmd("FILTER_SCALE", &params.filter_scale, &count, &def, 1, 0);

    zvparmd("CLOCK_RANGE", params.clock_range, &count, &def, 2, 0);
    zvparmd("CLOCK_STEP", &params.clock_step, &count, &def, 1, 0);

    // If delta wts range/step (clock for WTS in delta mode only) are not
    // given, default them to clock_range/step.

    zvparmd("DELTA_WTS_RANGE", params.delta_wts_range, &count, &def, 2, 0);
    if (count != 2) {
	params.delta_wts_range[0] = params.clock_range[0];
	params.delta_wts_range[1] = params.clock_range[1];
	if (delta_tilt) {	// don't bother reporting if not
	    snprintf(msg, MSG_LEN, "DELTA_WTS_RANGE defaulted to %f,%f",
		params.delta_wts_range[0], params.delta_wts_range[1]);
	    zvmessage(msg, "");
	}
    }
    zvparmd("DELTA_WTS_STEP", &params.delta_wts_step, &count, &def, 1, 0);
    if (count != 1) {
	params.delta_wts_step = params.clock_step;
	if (delta_tilt) {	// don't bother reporting if not
	    snprintf(msg, MSG_LEN, "DELTA_WTS_STEP defaulted to %f",
		params.delta_wts_step);
	    zvmessage(msg, "");
	}
    }

    snprintf(msg, MSG_LEN, "Tilt Threshold: %f, Sinkage: %f, Foot_Window: %d",
		params.tilt_threshold, params.possible_sinkage,
		params.foot_window);
    zvmessage(msg, "");

    snprintf(msg, MSG_LEN, "Clock range: %f,%f deg,  Clock step: %f deg",
		params.clock_range[0], params.clock_range[1],
		params.clock_step);
    zvmessage(msg, "");

    // Open input file(s).  An XYZ data could be either 1 3-band file
    // or 3 single band files

    open_inputs(nids, file_models, xyz_unit, xyz_band, "XYZ");

    // get input image dimensions
    nlo = file_models[0]->getNL();
    nso = file_models[0]->getNS();

    // Open output file. OUT is a single 3 band float file

    zvpcnt("OUT", &count);
    zvunit(&out_unit, "OUT", 1, NULL);
    zvopen(out_unit, "op", "write",
         "u_ns", nso, "u_nl", nlo,
         "u_nb", num_tilt_bands + 1,
         "open_act", "sa", "u_org", "bsq",
         "u_format", "doub", "o_format", "real", NULL);
    zvplabel(out_unit, 0, 1);

    char p_coord[256];
    zvp("P_COORD", p_coord, &count);
    PigCoordSystem *p_cs = m->getCoordSystem(file_models[0], p_coord);
    if (p_cs == NULL) {
        snprintf(msg, MSG_LEN, "Invalid P_COORD: %s", p_coord);
        zvmessage(msg, "");
        zabend();
    }

    // write output label

    PigLabelModel *labelModel = m->createLabelModel(out_unit);

    // gather all filemodels for handing over to label model
    int nids_all = nids;
    PigFileModel *file_models_all[MAX_INPUTS*2];
    for (int cnt = 0; cnt < nids; cnt++)
        file_models_all[cnt] = file_models[cnt];

    // pick the coordinate system to use.
    {
        if (delta_tilt)
            labelModel->setInstPlacement(file_models_all, nids_all, 
                                         "INST_DELTA_TILT_MAP", instType, cs);
        else
            labelModel->setInstPlacement(file_models_all, nids_all, 
                                         "INST_TILT_MAP", instType, cs);
    }

    // Open normal output file (site frame), if given. UIX_OUT is a single 
    // 3 band float file

    char uvw_filename[PIG_MAX_FILENAME_SIZE];
    uix_out_unit = -1;			// flag no output
    zvp("UIX_OUT", uvw_filename, &count);
    if (count == 1) {
	if (delta_tilt) {
	    zvmessage("UIX_OUT not available in delta-tilt mode", "");
	    zabend();
	}
	zvunit(&uix_out_unit, "UIX_OUT", 1, "u_name", uvw_filename, NULL);
        zvopen(uix_out_unit, "op", "write",
         "u_ns", nso, "u_nl", nlo,
         "u_nb", 3,
         "open_act", "sa", "u_org", "bsq",
         "u_format", "doub", "o_format", "real", NULL);
        zvplabel(uix_out_unit, 0, 1);
        PigLabelModel *labelModel2 = m->createLabelModel(uix_out_unit);
        labelModel2->setInstPlacement(file_models_all, nids_all, "INST_UVW_MAP", 
                                      instType, cs);
    }

    // Open Z output file, if given. ZIX_OUT is a single 1 band float file

    char z_filename[PIG_MAX_FILENAME_SIZE];
    zix_out_unit = -1;			// flag no output
    zvp("ZIX_OUT", z_filename, &count);
    if (count == 1) {
	if (delta_tilt) {
	    zvmessage("ZIX_OUT not available in delta-tilt mode", "");
	    zabend();
	}
	zvunit(&zix_out_unit, "ZIX_OUT", 1, "u_name", z_filename, NULL);
        zvopen(zix_out_unit, "op", "write",
         "u_ns", nso, "u_nl", nlo,
         "u_nb", 1,
         "open_act", "sa", "u_org", "bsq",
         "u_format", "doub", "o_format", "real", NULL);
        zvplabel(zix_out_unit, 0, 1);
        PigLabelModel *labelModel2 = m->createLabelModel(zix_out_unit);
        labelModel2->setInstPlacement(file_models_all, nids_all, "INST_Z_MAP", 
                                      instType, cs);
    }

    // Open normal output file (lander frame), if given. UPX_OUT is a single
    // 3 band float file

    char upx_filename[PIG_MAX_FILENAME_SIZE];
    upx_out_unit = -1;                  // flag no output
    zvp("UPX_OUT", upx_filename, &count);
    if (count == 1) {
        if (delta_tilt) {
            zvmessage("UPX_OUT not available in delta-tilt mode", "");
            zabend();
        }

        zvunit(&upx_out_unit, "UPX_OUT", 1, "u_name", upx_filename, NULL);
        zvopen(upx_out_unit, "op", "write", "u_ns", nso, "u_nl", nlo, "u_nb", 3,
               "open_act", "sa", "u_org", "bsq", "u_format", "doub", "o_format",
               "real", NULL);
        zvplabel(upx_out_unit, 0, 1);
        PigLabelModel *labelModel2 = m->createLabelModel(upx_out_unit);
        labelModel2->setInstPlacement(file_models_all, nids_all, "INST_UVW_MAP",
                                      instType, p_cs);
    }

    // Open Z output file (lander frame), if given. ZIX_OUT is a single 1 band 
    // float file
    
    char xpx_filename[PIG_MAX_FILENAME_SIZE];
    xpx_out_unit = -1;                  // flag no output
    zvp("XPX_OUT", xpx_filename, &count);
    if (count == 1) {
        if (delta_tilt) {
            zvmessage("XPX_OUT not available in delta-tilt mode", "");
            zabend();
        }
        zvunit(&xpx_out_unit, "XPX_OUT", 1, "u_name", xpx_filename, NULL);
        zvopen(xpx_out_unit, "op", "write", "u_ns", nso, "u_nl", nlo, "u_nb", 3,
               "open_act", "sa", "u_org", "bsq", "u_format", "doub", "o_format",
               "real", NULL);
        zvplabel(xpx_out_unit, 0, 1);
        PigLabelModel *labelModel2 = m->createLabelModel(xpx_out_unit);
        labelModel2->setInstPlacement(file_models_all, nids_all, "INST_XYZ_MAP",
                                      instType, p_cs);
    }

    for (i = 0; i < 3; i++) {
        input_xyz[i].alloc(nlo, nso);
    }
    for (i = 0; i < num_tilt_bands; i++) {
        output_min_max_tilt[i].alloc(nlo, nso);
    }
    for (i = 0; i < 3; i++) {
        output_avg_normal[i].alloc(nlo, nso);
    }
    output_z.alloc(nlo, nso); 
    for (i = 0; i < 3; i++) {
        output_upx[i].alloc(nlo, nso);
    }
    for (i = 0; i < 3; i++) {
        output_xpx[i].alloc(nlo, nso);
    }

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
            for (samp = 0; samp < nso; samp++) {
                PigPoint old_xyz(input_xyz[0].get(line, samp),
                                 input_xyz[1].get(line, samp),
                                 input_xyz[2].get(line, samp));

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

    params.iparams.readInstrumentParams();

    if (delta_tilt) {

        const std::vector<SampleCircle> points_of_seis_feet =
            plot_points_of_instrument_feet(params.iparams, SEIS);
        const std::vector<SampleCircle> points_of_wts_feet =
            plot_points_of_instrument_feet(params.iparams, WTS);

        // Sanity check.
        const size_t num_expected = 3;
        assert(points_of_seis_feet.size() == num_expected);
        assert(points_of_wts_feet.size() == num_expected);

        compute_delta_tilt(params, input_xyz, points_of_seis_feet, 
                           points_of_wts_feet, SEIS, WTS, output_min_max_tilt);
    } else {

        const std::vector<SampleCircle> points_of_instrument_feet =
            plot_points_of_instrument_feet(params.iparams, params.inst);

        // Sanity check.
        const size_t num_expected = (params.inst == HP3) ? 4 : 3;
        assert(points_of_instrument_feet.size() == num_expected);

        compute_tilt(params, input_xyz, points_of_instrument_feet, 
                     output_min_max_tilt, output_avg_normal, output_z, 
                     output_xpx, cs, p_cs, (xpx_out_unit != -1) ? 1 : 0);

    }
    SimpleImage<double> state = compute_state(params,
                                              output_min_max_tilt[1]);

    for (line = 0; line < nlo; line++) {
        zvwrit(out_unit, state.linePtr(line),
                "BAND", 1, "LINE", line+1, NULL);
    }

    for (band = 0; band < num_tilt_bands; band++) {
        for (line = 0; line < nlo; line++) {
            zvwrit(out_unit, output_min_max_tilt[band].linePtr(line),
                    "BAND", band+2, "LINE", line+1, NULL);
        }
    }

    zvclose(out_unit, NULL);

    if (uix_out_unit != -1) {
        for (band = 0; band < 3; band++) {
            for (line = 0; line < nlo; line++) {
                zvwrit(uix_out_unit, output_avg_normal[band].linePtr(line),
                    "BAND", band+1, "LINE", line+1, NULL);
	    }
        }
	zvclose(uix_out_unit, NULL);
    }

    if (zix_out_unit != -1) {
        for (line = 0; line < nlo; line++) {
            zvwrit(zix_out_unit, output_z.linePtr(line),
                    "BAND", 1, "LINE", line+1, NULL);
        }
        zvclose(zix_out_unit, NULL);
    }

    if (upx_out_unit != -1) {
        // convert output_avg_normal from site to lander frame
        for (line = 0; line < nlo; line++) {
            for (samp = 0; samp < nso; samp++) {
                PigVector old_uvw(output_avg_normal[0].get(line, samp),
                                 output_avg_normal[1].get(line, samp),
                                 output_avg_normal[2].get(line, samp));

                if (old_uvw.getX() != 0.0 || 
                    old_uvw.getY() != 0.0 ||
                    old_uvw.getZ() != 0.0) {

                    PigVector new_uvw = p_cs->convertVector(old_uvw, cs);
                    output_upx[0].set(line, samp, new_uvw.getX());
                    output_upx[1].set(line, samp, new_uvw.getY());
                    output_upx[2].set(line, samp, new_uvw.getZ());
                }
            }
        }

        for (band = 0; band < 3; band++) {
            for (line = 0; line < nlo; line++) {
                zvwrit(upx_out_unit, output_upx[band].linePtr(line),
                       "BAND", band+1, "LINE", line+1, NULL);
            }
        }
        zvclose(upx_out_unit, NULL);
    }

    if (xpx_out_unit != -1) {
        for (band = 0; band < 3; band++) {
            for (line = 0; line < nlo; line++) {
                zvwrit(xpx_out_unit, output_xpx[band].linePtr(line),
                       "BAND", band+1, "LINE", line+1, NULL);
            }
        }
        zvclose(xpx_out_unit, NULL);
    }
}

////////////////////////////////////////////////////////////////////////
// Open one set of XYZ inputs
////////////////////////////////////////////////////////////////////////

void open_inputs(int nids, PigFileModel *file_models[], int unit[3],
        int band[3], char *type)
{
    int MSG_LEN = 256;
    char msg[MSG_LEN];

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
            snprintf(msg, MSG_LEN, "A single %s file must have three bands", type);
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
                snprintf(msg, MSG_LEN, "A three-file %s must have one band each", type);
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
        snprintf(msg, MSG_LEN, "NSYTTILT requires either 1 3-band file or 3 single band "
                "files as input for %s", type);
        zvmessage(msg, "");
        zabend();
    }
}
