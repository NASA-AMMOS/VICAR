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
#include "PigCSDefinition.h"
#include "PigCameraMapper.h"
#include "PigCameraMapEntry.h"

#include "mat3.h"

#include "return_status.h"

#include <math.h>
#include <iostream>
#include <fstream>

using namespace std;

////////////////////////////////////////////////////////////////////////

void main44()
{
    int count, def;
    char outfile[PIG_MAX_FILENAME_SIZE+1];

    zvmessage("MARSMAKE_CM version 1", "");

    char host_id[255];
    char mission[255];
    char instrument[255];
    char version[255];
    char subtype[255];
    char construction[255];
    char calibration[255];
    char pm_type[255];
    char *pm_type_p = NULL;

    zvp("host_id", host_id, &count);
    zvp("mission", mission, &count);
    zvp("instrument", instrument, &count);
    zvp("version", version, &count);
    zvp("subtype", subtype, &count);
    zvp("construction", construction, &count);
    zvp("calibration", calibration, &count);
    zvp("pm_type", pm_type, &count);
    zvp("out", outfile, &count);

    if (count != 0) pm_type_p = pm_type;

    PigXerces::initialize();
    PigCameraMapper *cmap = new PigCameraMapper(NULL, host_id);
    PigCameraMapEntry *cme = cmap->findFromID(host_id);
    PigMission *m = PigMission::getMissionObject(mission);

    // Need a dummy CS
    int rmc[] = { 1, 0, 0 };
    PigCSReference csrefs(m, "SITE", NULL, rmc, 1, NULL);
    PigCSReference csrefr(m, "ROVER", NULL, rmc, 3, NULL);
    PigCSDefinition csdef(m, &csrefr, &csrefs, PigVector(), PigQuaternion());

    PigRoverStateManager *rsm = m->getRoverStateManager();
    rsm->addSolution(&csdef);

    PigCoordSystem *cs = m->getCoordSystem(&csrefr);

    PigCameraModel *cmod = m->createCameraModel(instrument, version, subtype,
                                                NULL, construction, calibration, cs);

    PigPointingModel *pmod = m->createPointingModel(cmod, instrument,
                                                    pm_type_p, true);

    // Verify the parameters

    printf("mission: '%s'\n", cmod->getMissionName());
    printf("instrument: '%s'\n", cmod->getInstrumentName());
    printf("version: '%s'\n", cmod->getCameraVersion());
    printf("subtype: '%s'\n", cmod->getCameraSubtype());
    printf("construction: '%s'\n", cmod->getCameraConstruction());
    printf("calibration: '%s'\n", cmod->getCameraCalibration());
    printf("writing to: '%s'\n", outfile);

    printf("PM type: '%s'\n", pmod->getModelName());

    // The model MUST be pointed or otherwise set before it can be used!!!
    // Either of the below methods...

    // Level 1 - pointing parameters

    double params[20];
    int n = 0;
    zvparmd("PARAMS", params, &n, &def, 20, 0);

    if (n != 0) {

        // This sets pointing parameters (level 1)

        pmod->setPointingParameters(params, n);

        // print results

        char str[1000];
        cmod->writeToString(str, 1000);
        printf("writeToString: '%s'\n", str);

        // Open output file for writing
        ofstream outfilestream;
        outfilestream.open(outfile);

        string json_output = "{\"cmod\":\"";
        json_output.append(str);
        json_output.append("\"}");

        outfilestream << json_output;
        outfilestream.close();

        n = pmod->getPointingParamCount();
        pmod->getPointingParameters(params, n);

        for (int i=0; i < n; i++) {
            printf("param %d (%s) = %lf\n", i,
                   pmod->getPointingParamName(i), params[i]);
        }
    }

    // Level 2 - pose.  TEMPORARY - need to use quat and a better orientation
    // calculation...

    float xyz[3];
    PigPoint pos;
    PigVector ori;
    zvp("POS", xyz, &count);
    if (count != 0) {
        pos.setXYZ(xyz);
        zvp("ORI", xyz, &count);
        if (count != 0) {
            ori.setXYZ(xyz);
        }
    }
    if (count != 0) {
        cmod->setCameraPosition(pos, cs);
        cmod->setCameraOrientation(ori, cs);

        // print results

        char str[1000];
        cmod->writeToString(str, 1000);
        printf("writeToString: '%s'\n", str);
    }

    // Level 3 setting is TBD

}




