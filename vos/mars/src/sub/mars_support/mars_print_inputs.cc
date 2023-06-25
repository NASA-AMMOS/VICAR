////////////////////////////////////////////////////////////////////////
// mars_print_inputs.cc
//
// Prints various information about the inputs and their pointing.
////////////////////////////////////////////////////////////////////////

#include "mars_support.h"

#include "zvproto.h"

#include "PigFileModel.h"
#include "PigPointingModel.h"
#include "PigCameraModel.h"
#include "PigCoordSystem.h"

void mars_print_inputs(int nids, PigPointingModel *pointing_in[],
		PigCameraModel *camera_in[], PigFileModel *file_models[],
		int homogeneous_inputs, char *mission, char *instrument)
{
    int i;
    int a_good_file_idx;
    char msg[255];

    for (a_good_file_idx = 0; a_good_file_idx < nids; a_good_file_idx++)
        if ((file_models[a_good_file_idx] != NULL) && (pointing_in[a_good_file_idx] != NULL))
            break;
    if (a_good_file_idx >= nids) {
	zvmessage("No open files and/or pointing models! Nothing to print!", "");
	return;
    }

    PigMission *m = PigMission::getMissionObject(
			file_models[a_good_file_idx]->getMissionName());
    PigCoordSystem *inst_cs = pointing_in[a_good_file_idx]->getCoordSystem();
    PigCoordSystem *fixed_cs = m->getFixedCS();
    zvmessage("Input data acquired from image labels:", "");
    zvmessage(mission, "");
    zvmessage(instrument, "");
    zvmessage("Input images in order of priority of usage", "");
    if (!homogeneous_inputs)
        zvmessage("(Instrument coord labels apply only to first input and matching instruments)","");
    zvmessage("                        ---Surface Fixed--- ---Instrument[0]---",
                                                                        "");
    sprintf(msg, "Input   y_off     x_off   azimuth elevation %9.9s %9.9s %9.9s %9.9s %9.9s %9.9s %9.9s",
//!!!! COUNT THE PARAMS CORRECTLY
                pointing_in[a_good_file_idx]->getPointingParamName(0),
                pointing_in[a_good_file_idx]->getPointingParamName(1),
                pointing_in[a_good_file_idx]->getPointingParamName(2),
                pointing_in[a_good_file_idx]->getPointingParamName(3),
                pointing_in[a_good_file_idx]->getPointingParamName(4),
                pointing_in[a_good_file_idx]->getPointingParamName(5),
                pointing_in[a_good_file_idx]->getPointingParamName(6));
    zvmessage(msg, "");
    for (i=0; i < nids; i++) {
        if ((pointing_in[i] == NULL) || (file_models[i] == NULL)) {
            sprintf(msg, "%3d  Not enough information to print for this one!", i+1);
            zvmessage(msg, "");
            continue;
        }
        double params[7];
        params[0] = params[1] = params[2] = params[3] = params[4] = params[5] = params[6] = 0.0;
        pointing_in[i]->getPointingParameters(params, 7);
	PigVector orient = fixed_cs->convertVector(
			camera_in[i]->getCameraOrientation(),
			camera_in[i]->getCoordSystem());

        sprintf(msg, "%3d %9f %9f %9f %9f %9f %9f %9f %9f %9f %9f %9f", i+1,
		file_models[i]->getYOffset(), file_models[i]->getXOffset(),
                PigRad2Deg(fixed_cs->getAz(orient)),
                PigRad2Deg(fixed_cs->getEl(orient)),
                params[0], params[1], params[2], params[3], params[4], params[5], params[6]);
//!!!! COUNT THE PARAMS CORRECTLY
        zvmessage(msg, "");
    }
}

