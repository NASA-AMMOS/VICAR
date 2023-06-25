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
#define MAX_INPUTS 1
#define MAX_NL MARS_MAX_NL
#define MAX_NS MARS_MAX_NS

#define MAX_OBUF 30000


////////////////////////////////////////////////////////////////////////

void main44()
{
  int i, j;
  int count;
#define MSG_SIZE 150
  char msg[MSG_SIZE];

  int nids;
  char mission[64], instrument[64];
  int nl, ns, nb;

  // Inputs

  PigFileModel *file_models[MAX_INPUTS];
  PigCameraModel *camera_in[MAX_INPUTS];
  PigPointingModel *pointing_in[MAX_INPUTS];
  int homogeneous_inputs = TRUE;
  PigCoordSystem *cs, *range_cs;
  int range_unit;
  float point[3];

  // Outputs
  int out_unit[3];
  int out_band[3];
  int nlo, nso;
  float range[MAX_NS];
  float x[MAX_NS], y[MAX_NS], z[MAX_NS];

  // User Parameters
  PigPoint rangeOrigin;


  zvmessage("MARSINVRANGE version 2020-04-30", "");

  // Get the input file list, and set up initial camera/pointing models
  // for each input.  Although we accept one input only, mars_setup
  // does lots of other nice things for us.

  mars_setup(nids, file_models, camera_in, pointing_in, NULL, cs,
	     mission, instrument, homogeneous_inputs,
	     MAX_NL, MAX_NS, MAX_INPUTS);

  PigMission *m = PigMission::getMissionObject(mission);

  // Get coord system for input range file

  PigCSReference *ref;
  file_models[0]->getDerivedImageCS(ref);
  range_cs = m->getCoordSystem(ref);

  snprintf(msg, MSG_SIZE, "Interpreting RANGE values using the %s coordinate frame: %s",
	   range_cs->getFrameName(), ref->getFullName());
  zvmessage(msg, "");

  // Get the coordinate system to use.
  snprintf(msg, MSG_SIZE, "Generating XYZ using the %s coordinate frame: %s",
	   cs->getFrameName(), cs->getFullName());
  zvmessage(msg, "");

  // Get range origin point to use.  Note, this is not the same as
  // what WAS used to create the range file... that would be pointless,
  // as you could then use the original XYZ file.  Instead, we want to
  // use the NEW camera position from the input file, which might have
  // been modified via nav files, in order to recompute the XYZ.
  // There is still an override, just in case.

  zvpcnt("origin", &count);
  if (count == 3) {
    zvp("origin", point, &count);
    rangeOrigin.setXYZ(point);
  }
  else {

    // Use camera position as a default range origin value 
    rangeOrigin = pointing_in[0]->getCameraPosition(cs);
  }

  snprintf(msg, MSG_SIZE, "Using POINT (%f, %f, %f) as range origin",
	   rangeOrigin.getX(), rangeOrigin.getY(),rangeOrigin.getZ());
  zvmessage(msg, "");


  // Open input file.

  // make sure that file is not open
  if (file_models[0]->isFileOpen())
    file_models[0]->closeFile();
      
  // open the file
  file_models[0]->openFile();
  file_models[0]->setFileOpen(TRUE);
      
  // get Unit id
  range_unit = file_models[0]->getUnit();

  // check for proper number of bands
  zvget(range_unit, "nl", &nl, "ns", &ns, "nb", &nb, NULL);
  if (nb != 1) {
    zvmessage("A range file must have one band", "");
    zabend();
  }
 
  // OUT can be 1 or 3 files, for a single, 3-banded file, or 3 single-band
  // files.

  zvpcnt("OUT", &count);
  if (count == 1) {
    zvunit(&out_unit[0], "OUT", 1, NULL);
    zvopen(out_unit[0], "op", "write",
	   "u_ns", file_models[0]->getNS(), "u_nl",file_models[0]->getNL(),
	   "u_nb", 3,
	   "open_act", "sa", "u_org", "bsq",
	   "u_format", "real", "o_format", "real", NULL);
    zvplabel(out_unit[0], 0, 1);
    out_unit[1] = out_unit[0];
    out_unit[2] = out_unit[0];
    out_band[0] = 1;
    out_band[1] = 2;
    out_band[2] = 3;

    // write output label
    PigMission *m = PigMission::getMissionObject(mission);
    PigLabelModel *labelModel = m->createLabelModel(out_unit[0]);
    // pick the coordinate system to use.
    labelModel->setXYZ(file_models, nids, cs, "XYZ_MAP", 0.0,
			file_models[0]->getStereoProductId());
    if (zvptst("WRITE_CM"))
      labelModel->writeCM(camera_in[0], camera_in[0]->getCoordSystem());
  }
  else if (count == 3) {
    char *image_type[3] = {"X_MAP", "Y_MAP", "Z_MAP"};
    for (i=0; i<3; i++) {
      zvunit(&out_unit[i], "OUT", i+1, NULL);
      zvopen(out_unit[i], "op", "write",
	     "u_ns", file_models[0]->getNS(), "u_nl",file_models[0]->getNL(),
	     "u_nb", 1,
	     "open_act", "sa", "u_org", "bsq",
	     "u_format", "real", "o_format", "real", NULL);
      zvplabel(out_unit[i], 0, 1);
      out_band[i] = 1;

      // write output label
      PigMission *m = PigMission::getMissionObject(mission);
      PigLabelModel *labelModel = m->createLabelModel(out_unit[i]);
      // pick the coordinate system to use.
      labelModel->setXYZ(file_models, nids, cs, image_type[i], 0.0,
			file_models[0]->getStereoProductId());
      if (zvptst("WRITE_CM"))
	labelModel->writeCM(camera_in[0],
			    camera_in[0]->getCoordSystem());
    }
  }
  else {
    zvmessage("OUT must have 1 or 3 filenames", "");
    zabend();
  }

  // get input image dimensions
  nlo = file_models[0]->getNL();
  nso = file_models[0]->getNS();
  snprintf(msg, MSG_SIZE, "Output # lines & samples=%10d %10d", nlo, nso);
  zvmessage(msg, "");

  // check for limits
  if ((nlo > MAX_OBUF) || (nlo < 1) || (nso > MAX_OBUF) || (nso < 1)) {
    zvmessage("Unreasonable output file dimensions", "");
    zabend();
  }

  // Process the image data

  for (j=0; j < file_models[0]->getNL(); j++) {		// line

    // read the line from the input file
    zvread(range_unit, range, "LINE", j+1, "BAND", 1, NULL);

    for (i=0; i < file_models[0]->getNS(); i++) {		// samp

      // While you generally shouldn't compare floats for equality,
      // 0.0 is being used as a specific flag value and is representable
      // exactly...

      if (range[i] == 0.0) {
	x[i] = 0.0;			// invalid point
	y[i] = 0.0;
	z[i] = 0.0;
      }
      else {
	double img_l = j + file_models[0]->getYOffset();
	double img_s = i + file_models[0]->getXOffset();

	// Compute the XYZ point.  We get the view ray direction and
	// then go a distance "range" along it.  We maybe should use
	// the returned position as the origin, since for CAHVORE
	// it is NOT the same for every pixel.  However, marsrange
	// does not do this - it assumes a fixed origin across the
	// image - so, since we're supposed to be an inverse of that,
	// we don't either.

	PigPoint img_origin;
	PigVector img_vector;
	camera_in[0]->LStoLookVector(img_l, img_s,
				     img_origin, img_vector, cs);

	img_vector.normalize();		// make sure it's a unit vector

	PigVector new_vector = img_vector * range[i];

	x[i] = rangeOrigin.getX() + new_vector.getX();
	y[i] = rangeOrigin.getY() + new_vector.getY();
	z[i] = rangeOrigin.getZ() + new_vector.getZ();

	// At first glance something like the below would seem necessary.  However,
	// the range_cs really is irrelevant... it is simply a distance, and we do
	// assume all coord sys's have the same unit of distance.  Since we don't
	// retrieve the range origin from the range file but instead recompute it
	// in cs, range_cs really isn't used at all and xyz is already in cs.
#if 0
	if (cs != range_cs) {		// Convert coord sys's
	  PigPoint xyz(x[i], y[i], z[i]);
	  PigPoint new_xyz = cs->convertPoint(xyz, range_cs);
	  x[i] = new_xyz.getX();
	  y[i] = new_xyz.getY();
	  z[i] = new_xyz.getZ();
	}
#endif
      }
    }
    zvwrit(out_unit[0], x, "LINE", j+1, "BAND", out_band[0], NULL);
    zvwrit(out_unit[1], y, "LINE", j+1, "BAND", out_band[1], NULL);
    zvwrit(out_unit[2], z, "LINE", j+1, "BAND", out_band[2], NULL);
  }
  zvclose(out_unit[0], NULL);
  if (out_unit[1] != out_unit[0])
    zvclose(out_unit[1],NULL);
  if (out_unit[2] != out_unit[0])
    zvclose(out_unit[2],NULL);

}
