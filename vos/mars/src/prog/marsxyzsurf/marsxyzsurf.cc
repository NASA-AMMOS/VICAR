/* marsxyzsurf */
#include <math.h>
#include <iostream>
using namespace std;

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
#include "mat3.h"

#include "return_status.h"

#include "SimpleImage.h"

/* buffer sizes in main program */
#define MAX_INPUTS 1
#define MAX_NS 0 //no limit
#define MAX_NL 0

void main44()
{
    zvmessage("MARSXYZSURF version 1", "");

    int status, count, def;
    const int MSG_LEN = 150;
    char msg[MSG_LEN];

    int nids = 0;
    char mission[64], instrument[64];
    char filename[PIG_MAX_FILENAME_SIZE+1];

    // Inputs

    PigFileModel *file_models[MAX_INPUTS];
    PigCameraModel *camera_in[MAX_INPUTS];
    PigPointingModel *pointing_in[MAX_INPUTS];
    PigSurfaceModel *surface_model;
    int homogeneous_inputs = TRUE;
    PigCoordSystem *cs, *surf_cs; //cs: input image, surf_cs: surface

    // Outputs
    int out_unit[3], out_band[3];
    SimpleImage<double> xi, yi, zi;

    //read in inputs
    mars_setup(nids, file_models, camera_in, pointing_in, surface_model,
        NULL, cs, mission, instrument, homogeneous_inputs,
            MAX_NL, MAX_NS, MAX_INPUTS);

    if (nids != 1) {
	zvmessage("MARSXYZ requires 1 and only 1 image inputs", "");
	zabend();
    }
   
    surf_cs = surface_model->getCoordSystem();

    //create output file
    zvpcnt("OUT", &count);
    if (count == 1) {
	zvunit(&out_unit[0], "OUT", 1, NULL);
	zvopen(out_unit[0], "op", "write",
		"u_ns", file_models[0]->getNS(), "u_nl",file_models[0]->getNL(),
		"u_nb", 3,
		"open_act", "sa", "u_org", "bsq",
		"u_format", "doub", "o_format", "real", NULL);
	zvplabel(out_unit[0], 0, 1);
	out_unit[1] = out_unit[0];
	out_unit[2] = out_unit[0];
	out_band[0] = 1;
	out_band[1] = 2;
	out_band[2] = 3;
	 
	// write output label
	PigMission *m = PigMission::getMissionObject(mission);
	PigLabelModel *labelModel = m->createLabelModel(out_unit[0]);
	
    labelModel->setXYZ(file_models, nids, cs, "XYZ_MAP", 0.0,
			NULL);
	if (zvptst("WRITE_CM"))
	    labelModel->writeCM(camera_in[0], camera_in[0]->getCoordSystem());
    }
    else if (count == 3) {
        char* image_type[3] = {"X_MAP", "Y_MAP", "Z_MAP"};
        for (int i=0; i<3; i++) {
            zvunit(&out_unit[i], "OUT", i+1, NULL);
            zvopen(out_unit[i], "op", "write",
            "u_ns", file_models[0]->getNS(), "u_nl",file_models[0]->getNL(),
            "u_nb", 1,
            "open_act", "sa", "u_org", "bsq",
            "u_format", "doub", "o_format", "real", NULL);
            zvplabel(out_unit[i], 0, 1);
            out_band[i] = 1;

            // write output label
            PigMission *m = PigMission::getMissionObject(mission);
            PigLabelModel *labelModel = m->createLabelModel(out_unit[i]);
            // pick the coordinate system to use.
            labelModel->setXYZ(file_models, nids, cs, image_type[i], 0.0,
                NULL);
            if (zvptst("WRITE_CM"))
                labelModel->writeCM(camera_in[0],
                        camera_in[0]->getCoordSystem());
        }
    }
    else {
	    zvmessage("OUT must have 1 or 3 filenames", "");
	    zabend();
    }

    //loop through pixels and project to the ground
    int out_nl = file_models[0]->getNL();
    int out_ns = file_models[0]->getNS();

    xi.alloc(out_nl, out_ns);
    yi.alloc(out_nl, out_ns);
    zi.alloc(out_nl, out_ns);
   
    for (int j = 0; j < out_nl; j++) {

        double line = j + file_models[0]->getYOffset();	// 0-based

        for (int i = 0; i < out_ns; i++) {

            double samp = i + file_models[0]->getXOffset();	// 0-based

            PigPoint origin_surf;
	        PigVector look_surf;
	        camera_in[0]->LStoLookVector(line, samp, origin_surf, look_surf, surf_cs);

            PigPoint surf_pt_surf;
            int hits = surface_model->intersectRay(origin_surf, look_surf, surf_pt_surf);
	        int infinity = (hits <= 0);

            if(infinity)
            {
                xi.set(j,i,0.0);
	            yi.set(j,i,0.0);
	            zi.set(j,i,0.0);
            }
            else
            {
                PigPoint surf_pt_orig = cs->convertPoint(surf_pt_surf,surf_cs);
                xi.set(j,i, surf_pt_orig.getX());
	            yi.set(j,i, surf_pt_orig.getY());
	            zi.set(j,i, surf_pt_orig.getZ());
            }

        }
    }

     // Write out the xyz image

    for (int j=0; j < out_nl; j++) 
    {
        zvwrit(out_unit[0], xi.linePtr(j), "LINE",j+1, "BAND",out_band[0],NULL);
        zvwrit(out_unit[1], yi.linePtr(j), "LINE",j+1, "BAND",out_band[1],NULL);
        zvwrit(out_unit[2], zi.linePtr(j), "LINE",j+1, "BAND",out_band[2],NULL);
    }

    //close file
    zvclose(out_unit[0], NULL);
    if (out_unit[1] != out_unit[0])
	zvclose(out_unit[1], NULL);
    if (out_unit[2] != out_unit[0])
	zvclose(out_unit[2], NULL);
}