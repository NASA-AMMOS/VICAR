/* marsrange */
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
#include "mat3.h"

#include "return_status.h"

#include <math.h>
#include <iostream>
using namespace std;


/* buffer sizes in main program */
#define MAX_INPUTS 3
#define MAX_NL MARS_MAX_NL
#define MAX_NS MARS_MAX_NS
#define MAX_OBUF 30000


////////////////////////////////////////////////////////////////////////

void main44()
{
    int i, j;
    int count;
    const int MSG_LEN = 150;
    char msg[MSG_LEN];

    int nids;
    char mission[64], instrument[64];
    int nl, ns, nb;
    int band[3];

    // Inputs

    PigFileModel *file_models[MAX_INPUTS];
    PigCameraModel *camera_in[MAX_INPUTS];
    PigPointingModel *pointing_in[MAX_INPUTS];
    int homogeneous_inputs = TRUE;
    PigCoordSystem *cs, *xyz_cs;
    int xyz_unit[3];
    float point[3];

    // Outputs
    int unit_out;
    int nlo, nso;
    float diff_x, diff_y, diff_z;
    float dist[MAX_NS];
    float x[MAX_NS], y[MAX_NS], z[MAX_NS];

    // User Parameters
    PigPoint rangeOrigin;


    zvmessage("MARSRANGE version 2020-05-15", "");

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
    snprintf(msg, MSG_LEN, "Generating RANGE using the %s coordinate frame.",
	    cs->getFrameName());
    zvmessage(msg, "");

    // Get range origin point to use
    zvpcnt("origin", &count);
    if (count == 3) {
	zvp("origin", point, &count);
	rangeOrigin.setXYZ(point);
    }
    else {
	if (pointing_in[0] == NULL) {
	    zvmessage("No camera model to extract origin; use ORIGIN parameter", "");
	    zabend();
	}
	//Use camera position as a default range origin value 
	rangeOrigin = pointing_in[0]->getCameraPosition(cs);
    }

    // Check for XY only
    int zzero = zvptst("ZZERO");

    snprintf(msg, MSG_LEN, "Using POINT (%f, %f, %f) as range origin",
	    rangeOrigin.getX(), rangeOrigin.getY(),rangeOrigin.getZ());
    zvmessage(msg, "");


    // Open input file(s).  An XYZ data could be either 1 3-band file
    // or 3 single band files
    if (nids == 1) {
      
        //make sure that file is not open
        if(file_models[0]->isFileOpen())
	  file_models[0]->closeFile();
      
	//open the file
	file_models[0]->openFile();
	file_models[0]->setFileOpen(TRUE);
      
	//get Unit id
	xyz_unit[0] = file_models[0]->getUnit();

	//check for proper number of bands
	zvget(xyz_unit[0], "nl", &nl, "ns", &ns, "nb", &nb, NULL);
	if (nb != 3) {
	  zvmessage("A single XYZ file must have three bands", "");
	  zabend();
	}
 
	//Initialize xyz_unit array
	xyz_unit[2] = xyz_unit[1] = xyz_unit[0];
      
        //Initialize band array
	band[0] = 1;
	band[1] = 2;
	band[2] = 3;      
    }
    else if(nids == 3) {
      int cnt;
      for(cnt = 0; cnt <3; cnt++) {

        //make sure that file is not open
        if(file_models[cnt]->isFileOpen())
	  file_models[cnt]->closeFile();
      
	//open the file
	file_models[cnt]->openFile();
	file_models[cnt]->setFileOpen(TRUE);
      
	//get Unit id
	xyz_unit[cnt] = file_models[cnt]->getUnit();

	//check for proper number of bands
	zvget(xyz_unit[cnt], "nl", &nl, "ns", &ns, "nb", &nb, NULL);
	if (nb != 1) {
	    zvmessage("A three-file XYZ must have one band each (#1)", "");
	    zabend();
	}

	//check that all files are the same size
	if (nl != file_models[0]->getNL() || ns != file_models[0]->getNS()) {
	    zvmessage("Input is of different size than Input #1", "");
	    zabend();
	}
	band[cnt] = 1;
      }

    }
    else {
	zvmessage("MARSRANGE requires either 1 3-band file or 3 single band files", "");
	zabend();
    }


    // Open output files.
    // OUT should be 1 file
    zvpcnt("OUT", &count);
    if (count != 1) {
	zvmessage("OUT must have 1 filename", "");
	zabend();
    }


    count = 0;

    // get input image dimensions
    nlo = file_models[0]->getNL();
    nso = file_models[0]->getNS();
    snprintf(msg, MSG_LEN, "Output # lines & samples=%10d %10d", nlo, nso);
    zvmessage(msg, "");

    // check for limits
    if ((nlo > MAX_OBUF) || (nlo < 1) || (nso > MAX_OBUF) || (nso < 1)) {
	zvmessage("Unreasonable output file dimensions", "");
	zabend();
    }

    zvselpiu(file_models[0]->getUnit());	  // transfer labels
    zvunit(&unit_out, "OUT", 1, NULL);
    zvopen(unit_out, "OP", "WRITE", "U_FORMAT", "REAL",
               "U_NS", nso, "U_NL", nlo, "OPEN_ACT", "AS",
               "U_NB", 1, "U_ORG", "BSQ", "O_FORMAT", "REAL", NULL);
 
    // Write the output labels

    //write_marsrange_label(unit_out, file_models, NULL, nids, &proj);
    PigLabelModel *labelModel = m->createLabelModel(unit_out);
    labelModel->setRange(file_models, nids, rangeOrigin, cs);
    zvplabel(unit_out, 0, 1);

    // Process the image data

    for (j=0; j < file_models[0]->getNL(); j++) {		// line

	//read the line from the three bands of the single input file
	//or read the line from the three single band files
	zvread(xyz_unit[0], x, "LINE", j+1, "BAND", band[0], NULL);
	zvread(xyz_unit[1], y, "LINE", j+1, "BAND", band[1], NULL);
	zvread(xyz_unit[2], z, "LINE", j+1, "BAND", band[2], NULL);


        for (i=0; i < file_models[0]->getNS(); i++) {		// samp

	    // While you generally shouldn't compare floats for equality,
	    // 0.0 is being used as a specific flag value and is representable
	    // exactly...

	    if (x[i] == 0.0 && y[i] == 0.0 && z[i] == 0.0) {
	        dist[i] = 0.0;			// invalid point
	    }
	    else {
		if (cs != xyz_cs) {		// Convert coord sys's
		    PigPoint xyz(x[i], y[i], z[i]);
		    PigPoint new_xyz = cs->convertPoint(xyz, xyz_cs);
		    x[i] = new_xyz.getX();
		    y[i] = new_xyz.getY();
		    z[i] = new_xyz.getZ();
		}
	        // Compute the Cartesian distance
	        diff_x = x[i] - rangeOrigin.getX();
	        diff_y = y[i] - rangeOrigin.getY();
	        diff_z = z[i] - rangeOrigin.getZ();
		if (zzero)
		    diff_z = 0.0;

	        dist[i] = sqrt(diff_x*diff_x + diff_y*diff_y + diff_z*diff_z);
	    }
	}
	zvwrit(unit_out, dist, "LINE", j+1, "BAND", 1, NULL);
    }
    zvclose(unit_out, NULL);

}




