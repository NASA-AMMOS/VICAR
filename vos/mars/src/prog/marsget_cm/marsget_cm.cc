/* marsget_cm */
#include "vicmain_c"

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
#include "PigCSReference.h"
#include "PigRoverStateManager.h"

#include "mat3.h"

#include "return_status.h"

#include <math.h>
#include <iostream>
#include <fstream>

using namespace std;

/* buffer sizes in main program */
#define MAX_INPUTS 3
#define MAX_NL MARS_MAX_NL
#define MAX_NS MARS_MAX_NS
#define MAX_OBUF 30000

int o_unit;
int status;
int indices[10];
int num_indices;

////////////////////////////////////////////////////////////////////////

void main44()
{
    int count;
    char msg[1024];
    string json_output = "";

    char filename[PIG_MAX_FILENAME_SIZE+1];
    char pm_type[255];
    char outfile[PIG_MAX_FILENAME_SIZE+1];

    PigFileModel *file_model = NULL;
    PigCameraModel *camera_model = NULL;
    PigPointingModel *pointing_model = NULL;

    zvmessage("MARSGET_CM version 1", "");

    zvp("INP", filename, &count);
    zvp("PM_TYPE", pm_type, &count);
    zvp("OUT", outfile, &count);

    char *pm_type_p = pm_type;
    if (count == 0) pm_type_p = NULL;

    file_model = PigFileModel::create(filename);
    if (file_model == NULL)
    {
        snprintf(msg, 1024, "Unable to create file model for input '%s'", filename);
        zvmessage(msg, "");
        zabend();
    }

    PigMission *m = PigMission::getMissionObject(file_model->getMissionName());

    file_model->getRoverMotionCounter(indices, num_indices);


    PigRoverStateManager *rsm = m->getRoverStateManager();
    rsm->addFileCoordSystems(file_model);

    // Surprisingly, we don't actually need a CS here...
    camera_model = PigCameraModel::create(file_model, NULL);
    if (camera_model == NULL)
    {
        zvmessage("Unable to create camera model", "");
        zabend();
    }

    pointing_model = PigPointingModel::create(
            camera_model, file_model, pm_type_p, true);
    if (pointing_model == NULL)
    {
        zvmessage("Unable to create pointing model", "");
        zabend();
    }

    pointing_model->pointCamera(file_model);

    // Open output file for writing
    ofstream outfilestream;
    outfilestream.open(outfile);
    json_output.append("{");

    // Add the RMC
    json_output.append("\"rmc\":[");
    for (int i = 0; i < num_indices; i++) {
        json_output.append(to_string(indices[i]));

        if (i != num_indices - 1)
        {
            json_output.append(",");
        }
    }
    json_output.append("],");

    // These are the strings that should be stored in the DB to be able to
    // reconstruct the camera and pointing models
    json_output.append("\"host_id\":\"");
    json_output.append(m->getHostID());
    json_output.append("\",");

    json_output.append("\"mission\":\"");
    json_output.append(camera_model->getMissionName());
    json_output.append("\",");

    json_output.append("\"instrument\":\"");
    json_output.append(camera_model->getInstrumentName());
    json_output.append("\",");

    json_output.append("\"version\":\"");
    json_output.append(camera_model->getCameraVersion());
    json_output.append("\",");

    json_output.append("\"subtype\":\"");
    string subtype = camera_model->getCameraSubtype();
    subtype = subtype.substr(0, subtype.size() - 1);
    json_output.append(subtype);
    json_output.append("\",");

    json_output.append("\"construction\":\"");
    json_output.append(camera_model->getCameraConstruction());
    json_output.append("\",");

    json_output.append("\"calibration\":\"");
    json_output.append(camera_model->getCameraCalibration());
    json_output.append("\",");

    json_output.append("\"pm_type\":\"");
    json_output.append(pointing_model->getModelName());
    json_output.append("\",");

    // This is the cmod itself, just for reference.  It should not be stored
    // as a string but rather in individual fields.  I should probably add
    // some generic parameter access methods... TBD.  These are level 3 in
    // the payload.
    char str[1000];
    camera_model->writeToString(str, 1000);
    json_output.append("\"camera_model\":\"");
    json_output.append(str);
    json_output.append("\",");

    // These are the pointing parameters (level 1 in the payload).
    double params[20];
    int n = pointing_model->getPointingParamCount();
    pointing_model->getPointingParameters(params, n);

    json_output.append("\"pointing_params\":[");
    for (int i=0; i < n; i++)
    {
        json_output.append("{");
        json_output.append("\"index\":");
        json_output.append(to_string(i));
        json_output.append(",");
        json_output.append("\"name\":\"");
        json_output.append(pointing_model->getPointingParamName(i));
        json_output.append("\",");
        json_output.append("\"value\":");
        json_output.append(to_string(params[i]));
        json_output.append("}");

        if (i != n - 1)
        {
            json_output.append(",");
        }
    }
    json_output.append("],");

    // These are level 2 in the payload.  HOWEVER... need to replace
    // orientation with a better version, probably a quat
    PigPoint pos = camera_model->getCameraPosition();
    PigVector ori = camera_model->getCameraOrientation();

    json_output.append("\"position\": {");
    json_output.append("\"x\":");
    json_output.append(to_string(pos.getX()));
    json_output.append(",");
    json_output.append("\"y\":");
    json_output.append(to_string(pos.getY()));
    json_output.append(",");
    json_output.append("\"z\":");
    json_output.append(to_string(pos.getZ()));
    json_output.append("},");

    json_output.append("\"orientation\": {");
    json_output.append("\"x\":");
    json_output.append(to_string(ori.getX()));
    json_output.append(",");
    json_output.append("\"y\":");
    json_output.append(to_string(ori.getY()));
    json_output.append(",");
    json_output.append("\"z\":");
    json_output.append(to_string(ori.getZ()));
    json_output.append("}");

    json_output.append("}");

    outfilestream << json_output;
    outfilestream.close();
}


