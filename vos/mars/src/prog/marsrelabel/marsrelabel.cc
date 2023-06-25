/* marsrelabel */
#include "vicmain_c"

#include "mars_support.h"

#include "PigMission.h"
#include "PigFileModel.h"
#include "PigLabelModel.h"
#include "RadiometryModel.h"
#include "PigCameraModel.h"
#include "PigPointingModel.h"
#include "PigCoordSystem.h"
#include "PigCSReference.h"
#include "PigCSDefinition.h"
#include "PigSurfaceModel.h"

#include "lbl_image_data.h"
#include "return_status.h"

#include <stdio.h>

// buffer sizes in main program - these should be made dynamic, eventually!
#define MAX_INPUTS 1
#define MAX_OPEN 1
#define MAX_NL MARS_MAX_NL
#define MAX_NS MARS_MAX_NS

////////////////////////////////////////////////////////////////////////

void main44()
{
    int j;
    int count;
    int status;
    const size_t msgLen = 150;
    char msg[msgLen];

    int nids /* , nods */;		// # of inputs, outputs (must match!)
    char mission[64], instrument[64];

    // Inputs

    PigFileModel *file_models[MAX_INPUTS];
    PigCameraModel *camera_in[MAX_INPUTS];		// not used
    PigPointingModel *pointing_in[MAX_INPUTS];		// not used
    PigSurfaceModel *surface_model;
    int homogeneous_inputs = TRUE;
    // RadiometryModel *radiometric[MAX_INPUTS];
    PigCoordSystem *cs;

    // Outputs

    // char *filenames[MAX_INPUTS];
    // LblImageData_typ ImageData;
    char cm_filename[1024];

    zvmessage("MARSRELABEL version 2020-02-04", "");

    // Get the input file list, and set up initial camera/pointing models
    // for each input (not used), and the radiometry model (which is).

    mars_setup(nids, file_models, camera_in, pointing_in, surface_model, NULL, 
	       cs, mission, instrument, homogeneous_inputs,
               MAX_NL, MAX_NS, MAX_INPUTS);

    // Print out input status from labels

    mars_print_inputs(nids, pointing_in, camera_in, file_models,
			homogeneous_inputs, mission, instrument);

// I am not entirely sure, but I believe the code below is redundant.
// And may actually be harmful, now that a navtable may be applied.
// Needs to be tested/verified though.   rgd 2006-10-7
#if 0
    PigCameraModel *camera = PigCameraModel::create(file_models[0], NULL);

    // Create Pointing Model based on the input. 
    PigPointingModel *pointing_out = PigPointingModel::create(camera,
					    camera->getMissionName(), 
					    camera->getInstrumentName(),
					    NULL,
	                                    true);
    // Point camera model
    pointing_out->pointCamera(file_models[0]);
#else
    PigCameraModel *camera = camera_in[0];
#endif

    // open output file.  Make sure we set up the right input as
    // primary input, so the labels will be right.

    int unit_out;
    int nlo, nso, nb;

    // set output picture dimensions
    nlo = file_models[0]->getNL();
    nso = file_models[0]->getNS();
    nb = file_models[0]->getNB();
    snprintf(msg, msgLen, "Output # lines & samples=%10d %10d", nlo, nso);
    zvmessage(msg, "");

    zvselpiu(file_models[0]->getUnit());	  // transfer labels
    status=zvunit(&unit_out, "OUT", 1, NULL);
    status=zvopen(unit_out, "OP", "WRITE", "OPEN_ACT", "AS", 
		  "U_ORG", "BSQ",  NULL);

    // Write the output labels

    PigMission *m = PigMission::getMissionObject(mission);
    PigLabelModel *labelModel = m->createLabelModel(unit_out);

    if ( zvptst("ALL") || zvptst("CS") )
    {
        int instances[32];
	int number_of_props = 32;
	char prop_names[32][32 + 1];
	LblCoordinate_typ *lblCoordinate = new LblCoordinate_typ;
	char *found;

	//find all property names
	status = zlpinfo(unit_out,*prop_names, &number_of_props, 
			 "INST_NUM", instances, 
			 "ULEN",32+1, NULL); 
    
	for(int cnt = 0; cnt < number_of_props; cnt++) {
	    found = NULL;
	    found = strstr(prop_names[cnt], "COORDINATE_SYSTEM");
	    if(found != NULL) {
	        LblSetCoordinate(prop_names[cnt]);
		status = LblCoordinateApi(unit_out, LBL_READ,lblCoordinate, 1,(const char*)NULL);
		if (RTN_FAILURE(status)) {
		    snprintf(msg,msgLen, "%s", (char *)LblErrorMessage());
		    zvmessage(msg, "");
		}
		else {
//!!!! This is a horrid hack.  The CS system does not know about all possible
//!!!! frames (e.g. MER APXS).  And we have no mechanism by which to update
//!!!! those frames anyway.  So there's no need to rewrite them.  Rewrite
//!!!! only those frames that are related to mobility.  The hack is that we
//!!!! have to use specific, mission-dependent names here.  Should really have
//!!!! an "isMobilityFrame()" or some such available...
		    char *n = lblCoordinate->CoordinateSystemName.Value;
		    if (lblCoordinate->CoordinateSystemName.Valid &&
			(strcasecmp(n, "ROVER_FRAME") == 0 ||		// MER
			 strcasecmp(n, "ROVER_NAV_FRAME") == 0 ||	// MSL
			 strcasecmp(n, "ROVER_MECH_FRAME") == 0 ||	// MSL
			 strcasecmp(n, "PAYLOAD_FRAME") == 0 ||		// PHX
			 strcasecmp(n, "SITE_FRAME") == 0 ||		// all
			 strcasecmp(n, "LOCAL_LEVEL_FRAME") == 0)) {	// all
		        PigCSDefinition *csDef = 
			                 new PigCSDefinition(m, lblCoordinate);
			// We have to set the solution to NULL, otherwise it
			// will explicitly use "telemetry" and not be
			// overridable via the RSF file - which kind of
			// defeats the purpose of relabeling the CS in the
			// first place!
			//!!!! I think the above is no longer relevant...
			//!!!! rgd 2020-05-27

			PigCoordSystem *cs = m->getCoordSystem(csDef->getIdentity());
			labelModel->writeCS(cs);
		    }
		}
	    }
	}
    }
    if ( zvptst("ALL") || zvptst("CM") ) {
	if (zvptst("CHANGE_CM_CS"))
	    camera->setCoordSystem(cs);

        if (camera == NULL)
            zvmessage("Unable to update camera model label property group.", "");
        else {
	    // Transfer metadata unless we're asked not to
	    if (!zvptst("NO_CM_XFER")) {
		PigCameraModel *xform_model = camera_in[0]->clone();
		xform_model->readFromLabel(file_models[0], 1);
		xform_model->transferMetadata(camera);
	    }
            labelModel->writeCM(camera, camera->getCoordSystem());
	}
    }

    if ( zvptst("ALL") || zvptst("PM") ) {
        labelModel->writePointingModel(pointing_in[0]);
    }
    
    // For Watson Zstacks, the input cmod type is None.  For reasons unknown,
    // when the cmod is replaced by mars_setup(), it comes out with all vectors
    // 0 (except R, oddly).  If you remove the input cmod from the label, it
    // recomputes the cmod (wrong, because it's the arm state at cmod creation
    // time, but it's a valid model).  Bizarre.  In any case, we check here for
    // the A vector being all 0 as a flag of this case, and avoid doing the
    // azel check if so.

    if (zvptst("AZEL")) {
      if (camera == NULL) {
	zvmessage("No camera model, -AZEL ignored", "");
      } else if (camera->getCameraOrientation().magnitude() < 1e-3) {
	zvmessage("Invalid camera model (A vector 0), -AZEL ignored", "");
      } else {

	// Write out the instrument azimuth and elevation using the cmod

	// Get the CS's to use

	char csname[256];
	zvp("ROVER_CS_NAME", csname, &count);
	PigCoordSystem *rover_cs = NULL;
	if (count == 1) {
	    rover_cs = m->getCoordSystem(file_models[0], csname);
	    if (rover_cs == NULL) {
	        zvmessage("Unable to get rover CS for AZEL", "");
	    }
	}
	zvp("SITE_CS_NAME", csname, &count);
	PigCoordSystem *site_cs = NULL;
	if (count == 1) {
	    site_cs = m->getCoordSystem(file_models[0], csname);
	    if (site_cs == NULL) {
	        zvmessage("Unable to get site CS for AZEL", "");
	    }
	}


	int nl, ns;
	PigFileModel::getNominalCameraSize(m, instrument, nl, ns);
	
	// Downsample and subframe

	double xscale, yscale;
	xscale = file_models[0]->getDownsampleXFactor(1.0);
	yscale = file_models[0]->getDownsampleYFactor(1.0);
	int dx, dy;
	dx = file_models[0]->getFirstLineSample(1) - 1;
	dy = file_models[0]->getFirstLine(1) - 1;

	double y_center = (nl/2.0 - dy) / yscale - 0.5;
	double x_center = (ns/2.0 - dx) / xscale - 0.5;

	PigPoint origin;
	PigVector look;

	// Rover frame

	double rover_az = 0;
	double rover_el = 0;
	if (rover_cs != NULL) {
	    camera->LStoLookVector(y_center, x_center, origin, look, rover_cs);

	    rover_az = PigRad2Deg(rover_cs->getAz(look));
	    rover_el = PigRad2Deg(rover_cs->getEl(look));
	}

	// Site frame

	double site_az = 0;
	double site_el = 0;
	if (site_cs != NULL) {
	    camera->LStoLookVector(y_center, x_center, origin, look, site_cs);

	    site_az = PigRad2Deg(site_cs->getAz(look));
	    site_el = PigRad2Deg(site_cs->getEl(look));
	}

	LblDerivedGeometry_typ idr, ids;
	memset(&idr, 0, sizeof(LblDerivedGeometry_typ));
	memset(&ids, 0, sizeof(LblDerivedGeometry_typ));

	idr.LanderInstrumentAzimuth.Value = rover_az;
	idr.LanderInstrumentAzimuth.Valid = LBL_VALID;
	idr.LanderInstrumentElevation.Value = rover_el;
	idr.LanderInstrumentElevation.Valid = LBL_VALID;

	ids.LanderInstrumentAzimuth.Value = site_az;
	ids.LanderInstrumentAzimuth.Valid = LBL_VALID;
	ids.LanderInstrumentElevation.Value = site_el;
	ids.LanderInstrumentElevation.Valid = LBL_VALID;

	if (rover_cs != NULL) {
	    status = LblDerivedGeometryApi(unit_out, LBL_AUGMENT,
		    &idr, 1, "ROVER_DERIVED_GEOMETRY_PARMS");
	    if (RTN_FAILURE(status)) {
	        zvmessage("WARNING: Unable to write ROVER_DERIVED_GEOMETRY_PARMS", "");
	    }
	}

	if (site_cs != NULL) {
	    status = LblDerivedGeometryApi(unit_out, LBL_AUGMENT,
		    &ids, 1, "SITE_DERIVED_GEOMETRY_PARMS");
	    if (RTN_FAILURE(status)) {
	        zvmessage("WARNING: Unable to write SITE_DERIVED_GEOMETRY_PARMS", "");
	    }
	}
      }
    }


    // If requested, write out a Litwin-style camera model file
    zvp("OUT_CM", cm_filename, &count);
    if (count != 0) {
        if (camera == NULL)
            zvmessage("Unable to write out a Litwin-style camera model file.", "");
        else
            camera->writeToFile(cm_filename);
    }


    zvplabel(unit_out, 0, 1);

    //Calculate the needed memory size for allocation
    // Make buffer big enough to accomodate any possible pixel type
    char *buf = new char[file_models[0]->getNS() * 8];
    if(buf == NULL) {
        zvmessage("unable to allocate input memory array!!!", "");
	zabend();
    }

    // Now simply write the file back out
    // Re-open the files with the options we need (e.g. U_FORMAT)

    file_models[0]->closeFile();
    zvopen(file_models[0]->getUnit(), "OP", "READ", "OPEN_ACT", "SA", NULL);
    file_models[0]->setFileOpen(TRUE);
    // int fileNS = nso;
    int unit_in = file_models[0]->getUnit();
    for(int i=0; i<nso; i++) buf[i]=0;
    for (int i=0; i < nb; i++) {
        for (j=0; j<nlo; j++) {
	    // write output as zero's
	    zvwrit(unit_out, buf, "LINE", j+1, NULL);
	    // copy data from input to output
	    zvread(unit_in, buf, "BAND", i+1, "LINE", j+1, NULL);
	    zvwrit(unit_out, buf, "BAND", i+1, "LINE", j+1, NULL);
	}
    }

    //Free up all dynamically allocated memory for ibuf
    delete[] buf;

    // And close the file

    zvclose(unit_out, "CLOS_ACT", "FREE", NULL);
}

