/* mslrough */
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
#include <vector>


/* buffer sizes in main program */
#define MAX_INPUTS 3
#define MAX_NL MARS_MAX_NL
#define MAX_NS MARS_MAX_NS

#define DEPTH_BANDS 1

void open_inputs(int nids, PigFileModel *file_models[], int unit[3],
                 int band[3], char *type);

class Stat
{
public:
    Stat()
        : min(1.0e10), max(1.0e-10), avg(0), count(0), bin_num(0), bin_width(0)
    {}
    virtual ~Stat() {}
    double min;
    double max;
    double avg;
    double delta;
    double stdev;
    long count;
    long bin_num;
    double bin_width;
    std::vector<double> bin_centers;
    std::vector<unsigned long> bin_counts;
};

////////////////////////////////////////////////////////////////////////

void main44()
{
    int status, count, def;
    const size_t msgLen = 256;
    char msg[msgLen];

    int nids;
    char mission[64], instrument[64];

    // Inputs

    PigFileModel *file_models[MAX_INPUTS];
    PigCameraModel *camera_in[MAX_INPUTS];
    PigPointingModel *pointing_in[MAX_INPUTS];
    int homogeneous_inputs = TRUE;
    PigCoordSystem *cs = NULL;
    PigCoordSystem *img_xyz_cs = NULL;
    PigCoordSystem *uvw_cs = NULL;
    PigCoordSystem *xyz_cs = NULL;
    int xyz_unit[3];
    int xyz_band[3];
    double *xyz[3];			// input image
    double target_uvw[3];               // input used for the UVW vector at the target
    double target_xyz[3];               // input used for the target XYZ point

    // Outputs

    int out_unit;
    int nlo, nso;
    double *depth[DEPTH_BANDS];	  // output image
    bool *depth_flags;            // flag whether or not depth computed for input xyz image point

    zvmessage("MARSDEPTH version 1", "");

    // Get the input file list, and set up initial camera/pointing models
    // for each input.  Although we accept one or three inputs only, mars_setup
    // does lots of other nice things for us.

    mars_setup(nids, file_models, camera_in, pointing_in, NULL, cs,
               mission, instrument, homogeneous_inputs,
               MAX_NL, MAX_NS, MAX_INPUTS);

    PigMission *m = PigMission::getMissionObject(mission);

    // Get some input parameters

    // If depth is not computed for a point from the input xyz image,
    // the depth is set to bad_depth.

    double bad_depth = 0.0;
    zvparmd("BAD_DEPTH", &bad_depth, &count, &def, 1, 0);

    // Stand-off is the distance from camera (model) to the target.
    // It should be a positive value.

    double standoff = 0.2;
    zvparmd("STANDOFF", &standoff, &count, &def, 1, 0);

    // Get the optional image file for its camera model.

    int proto_nids = 0;
    char proto_mission[64], proto_instrument[64];
    PigFileModel *proto_fm;
    PigCameraModel *proto_cm;
    PigPointingModel *proto_pm;
    PigCoordSystem *proto_cs;
    int proto_homogeneous = TRUE;
    char proto_iamge[PIG_MAX_FILENAME_SIZE];
    char *p = proto_iamge;
    zvp("PROTO_IMAGE", proto_iamge, &count);
    if (count == 1) {
        proto_nids = 1;
        mars_read_filelist( proto_nids, &p, &proto_fm, &proto_cm, &proto_pm, 
                            NULL, NULL, proto_cs, proto_mission, proto_instrument, 
                            proto_homogeneous, MAX_NL, MAX_NS, NULL, NULL );
    }

    // Get the required uvw vector given at the command line,
    // and normalize it.

    zvparmd("TARGET_UVW", target_uvw, &count, &def, 3, 0);
    if (count != 3) {
        zvmessage("No 3D UVW surface-normal vector at the target provided!", "");
        zabend();
    } else if (target_uvw[0] == 0.0 && target_uvw[1] == 0.0 && target_uvw[2] == 0.0) {
        zvmessage("UVW surface-normal vector cannot be zero (0,0,0)!", "");
        zabend();
    }
    PigVector tgt_uvw(target_uvw);
    tgt_uvw.normalize();

    // The target surface normal vector (uvw) should point upward.  To 
    // indicate an upward vector, the -normal keyword should be set.  The
    // default target uvw vector is (0,0,-1), hence the -normal keyword
    // is set as default.  If the user enters a down looking target
    // uvw vector (the direction of camera approach), the user should set
    // the -approach keyword to indicate that the provided target uvw
    // vector is in approach direction.

    if (zvptst("APPROACH"))
        tgt_uvw *= -1;

    // Get the optional target point (xyz) given at the command line.

    zvparmd("TARGET_XYZ", target_xyz, &count, &def, 3, 0);
    int tgt_provided = (count == 3);
    if (!tgt_provided) {
        zvmessage("No 3D target point provided!  Will use a point close to the center of the input image.", "");
    } else if (target_xyz[0] == 0.0 && target_xyz[1] == 0.0 && target_xyz[2] == 0.0) {
        zvmessage("Target point cannot be zero (0,0,0)!", "");
        zabend();
    }
    PigPoint tgt_xyz(target_xyz);

    // Get coord system for input XYZ file

    PigCSReference *ref;
    file_models[0]->getDerivedImageCS(ref);
    img_xyz_cs = m->getCoordSystem(ref);

    snprintf(msg, msgLen, "Interpreting XYZ values using the %s coordinate frame: %s",
	    img_xyz_cs->getFrameName(), ref->getFullName());
    zvmessage(msg, "");

    // Get coord system for input target UVW vector

    char frame[64] = "";
    zvp("UVW_CS", frame, &count);
    uvw_cs = m->getCoordSystem(file_models[0], frame);

    snprintf(msg, msgLen, "Interpreting UVW values using the %s coordinate frame: %s",
	    uvw_cs->getFrameName(), ref->getFullName());
    zvmessage(msg, "");

    // Get coord system for input target point.  Note that command-line
    // argument XYZ_CS is ignored, if no target point (XYZ) at the command-line
    // is given.

    if (tgt_provided) {
        char frame[64] = "";
        zvp("XYZ_CS", frame, &count);
        xyz_cs = m->getCoordSystem(file_models[0], frame);

        snprintf(msg, msgLen, "Interpreting TGT values using the %s coordinate frame: %s",
                 xyz_cs->getFrameName(), ref->getFullName());
        zvmessage(msg, "");
    }

    // The coordinate system to use.

    snprintf(msg, msgLen, "Generating depth image using the %s coordinate frame.",
	    cs->getFrameName());
    zvmessage(msg, "");

    // Open input file(s).  An XYZ data could be either 1 3-band file
    // or 3 single band files

    open_inputs(nids, file_models, xyz_unit, xyz_band, "XYZ");

    // Open output file.
    // OUT is a 1-band float file

    zvpcnt("OUT", &count);
    zvunit(&out_unit, "OUT", 1, NULL);
    zvopen(out_unit, "op", "write",
	       "u_ns", file_models[0]->getNS(), "u_nl",file_models[0]->getNL(),
	       "u_nb", DEPTH_BANDS,
	       "open_act", "sa", "u_org", "bsq",
	       "u_format", "doub", "o_format", "real", NULL);
    zvplabel(out_unit, 0, 1);

    // write output label

    PigLabelModel *labelModel = m->createLabelModel(out_unit);

    // pick the coordinate system to use.

    labelModel->setDepth(file_models, nids, cs, bad_depth);

    // get input image dimensions

    nlo = file_models[0]->getNL();
    nso = file_models[0]->getNS();

    // Allocate memory for input XYZ, and output depth and depth flags.
    // The entire images must be in memory for the subroutine.

    for (int i=0; i<3; i++) {
	xyz[i] = (double *)malloc(nlo * nso * sizeof(double));
	if (xyz[i] == NULL) {
	    snprintf(msg, msgLen, "Unable to allocate memory for XYZ input %d", i);
	    zvmessage(msg, "");
	    zabend();
	}
    }

    for (int i=0; i<DEPTH_BANDS; i++) {
	depth[i] = (double *)malloc(nlo * nso * sizeof(double));
	if (depth[i] == NULL) {
	    snprintf(msg, msgLen, "Unable to allocate memory for output band %d", i);
	    zvmessage(msg, "");
	    zabend();
	}
    }

    depth_flags = (bool *)malloc(nlo * nso * sizeof(bool));
    if (depth_flags == NULL) {
	snprintf(msg, msgLen, "Unable to allocate memory for output depth flags");
	zvmessage(msg, "");
        zabend();
    }

    // Read in the XYZ file(s)...

    for (int band = 0; band < 3; band++) {
	for (int line = 0; line < nlo; line++) {
	    zvread(xyz_unit[band], (xyz[band]) + (line * nso),
			"BAND", xyz_band[band], "LINE", line+1, NULL);
	}
    }

    // Convert coord systems if necessary.

    if (cs != img_xyz_cs) {
	for (int line = 0; line < nlo; line++) {
	    for (int samp = 0; samp < nso; samp++) {
		int index = line * nso + samp;

		PigPoint old_xyz(*(xyz[0]+index), *(xyz[1]+index), *(xyz[2]+index));

		// If the point is 0,0,0 (meaning not valid), leave it alone

		if (old_xyz.getX() != 0.0 ||
		    old_xyz.getY() != 0.0 ||
		    old_xyz.getZ() != 0.0) {

		    PigPoint new_xyz = cs->convertPoint(old_xyz, img_xyz_cs);
		    *(xyz[0]+index) = new_xyz.getX();
		    *(xyz[1]+index) = new_xyz.getY();
		    *(xyz[2]+index) = new_xyz.getZ();
		}
	    }
	}
    }

    if (!tgt_provided) {
        // If target not provided through command-line argument,
        // choose a target after reading XYZ data and its coord conversion.

        int indx = (nlo/2)*nso + (nso/2);
        tgt_xyz.setXYZ(*(xyz[0]+indx), *(xyz[1]+indx), *(xyz[2]+indx));
        if (tgt_xyz.getX() == 0.0 && tgt_xyz.getY() == 0.0 && tgt_xyz.getZ() == 0.0) {
            zvmessage("Failed to choose a good target point.  Provide one at the command-line!", "");
            zabend();
        }
        xyz_cs = img_xyz_cs;
    } else if (cs != xyz_cs) {
        // If target is already provided (at the command line), then 
        // take care of coord conversion if necessary.

        tgt_xyz = cs->convertPoint(tgt_xyz, xyz_cs);
    }

    // Convert UVW vector coord system if necessary.

    if (cs != uvw_cs)
        tgt_uvw = cs->convertVector(tgt_uvw, uvw_cs);

    // Change its orientation and position camera pointing model as 
    // if it is looking at the target point.  Do not orientation and
    // position by calling proto_cm->setCameraOrientation() and
    // proto_cm->setCameraPosition().

    if (proto_nids == 1) {
        proto_pm->setCameraOrientation(tgt_uvw*(-1.0), cs);
        proto_pm->setCameraPosition((tgt_uvw*standoff)+tgt_xyz, cs);
    }

    // Get Field Of View limit in degrees and convert it to the type
    // of limit we want to use.   FOV is ignored it no optional input 
    // image file for cmod is provided.

    double fov_deg = 30.0;
    double fov;
    if (proto_nids == 1) {
        zvparmd("FOV", &fov_deg, &count, &def, 1, 0);
        fov = cos(PigDeg2Rad(fov_deg));
    }

    // Do depth analysis.  Remember that UVW vector is normalized.

    double d = -(tgt_uvw % tgt_xyz);

    // Currently, stand-off is only for camera model and FOV
    // filteration.  Set it to zero if no camera model.
 
    if (proto_nids < 1)
        standoff = 0.0;

    Stat stat;
    double dist_total = 0.0;
    for (int line = 0; line < nlo; line++) {
	for (int samp = 0; samp < nso; samp++) {
	    int index = line * nso + samp;

            // Initialize depth buffer.

            *(depth[0]+index) = bad_depth;
            *(depth_flags+index) = false;

            PigPoint surf_pt(*(xyz[0]+index), *(xyz[1]+index), *(xyz[2]+index));

	    if (surf_pt.getX() == 0.0 && surf_pt.getY() == 0.0 && surf_pt.getZ() == 0.0)
                continue;

            if (proto_nids == 1) {
	        PigVector new_look = surf_pt - proto_pm->getCameraPosition(cs);
	        new_look.normalize();

	        if ((new_look %  proto_pm->getCameraOrientation(cs)) < fov)
	            continue;

	        double ln = 0.0;
            double sm = 0.0;
	        proto_cm->XYZtoLS(surf_pt, 0, &ln, &sm, cs);

	        // Check if point is within the input image cmod.
		    
	        if (proto_fm->testPixelLocation(ln, sm) != 0)
	            continue;
            }

            double dist = tgt_uvw % surf_pt + d + standoff;
            *(depth[0]+index) = dist;
            *(depth_flags+index) = true;
            if (dist < stat.min) stat.min = dist;
            if (dist > stat.max) stat.max = dist;
            dist_total += dist;
            stat.count++;
        }
    }


    // Compute statistical data if any depth computation done.

    snprintf(msg, msgLen, "DEPTH STAT:  count = %ld", stat.count);
    zvmessage(msg, "");

    if (stat.count > 0) {
        stat.avg = dist_total / stat.count;
        stat.delta = stat.max - stat.min;

        snprintf(msg, msgLen, "DEPTH STAT:  max   = %f", stat.max);    zvmessage(msg, "");
        snprintf(msg, msgLen, "DEPTH STAT:  min   = %f", stat.min);    zvmessage(msg, "");
        snprintf(msg, msgLen, "DEPTH STAT:  delta = %f", stat.delta);    zvmessage(msg, "");
        snprintf(msg, msgLen, "DEPTH STAT:  avg   = %f", stat.avg);    zvmessage(msg, "");

        // Next, prepare binning stuff for histogram information.

        zvparm("HIST_BINS", &stat.bin_num, &count, &def, 1, 0);
        if (stat.delta <= 0)  // actually should not be negative here
            stat.bin_num = 1;
        if (stat.bin_num > 0) {
            stat.bin_width = stat.delta / stat.bin_num;
            stat.bin_centers.resize(stat.bin_num, 0.0);
            stat.bin_counts.resize(stat.bin_num, 0);
            for (int i=0; i<stat.bin_num; i++)
                stat.bin_centers[i] = stat.min + stat.bin_width * (i+0.5);
        }

        //  Repeat the same loop above, this time for the computation
        //  of the standard deviation.  Here, we compute std. dev. on
        //  the entire population (not just some samples); therefore,
        //  at the end we divide by p_count not (p_count-1).
        //
        //  Also, do the histogram binning in the loop.

        double sum = 0;
        for (int line = 0; line < nlo; line++) {
            for (int samp = 0; samp < nso; samp++) {
                int index = line * nso + samp;

                if ( !(*(depth_flags+index)) )
                    continue;

                double dpth = *(depth[0]+index);
                double diff = dpth - stat.avg;
                sum += diff*diff;

                int i;
                for (i = 0; i < stat.bin_num-1; i++) {
                    if (dpth < stat.min + (i+1)*stat.bin_width) {
                        stat.bin_counts[i]++;
                        break;
                    }
                }
                if (i == (stat.bin_num-1))
                    stat.bin_counts[i]++;
            }
        }
        stat.stdev = sqrt(sum/stat.count);

        snprintf(msg, msgLen, "DEPTH STAT:  stdev = %f", stat.stdev);    zvmessage(msg, "");
        if (stat.bin_num>0) {
            snprintf(msg, msgLen, "DEPTH STAT:  bin num   = %ld", stat.bin_num);    zvmessage(msg, "");
            snprintf(msg, msgLen, "DEPTH STAT:  bin width = %f", stat.bin_width);    zvmessage(msg, "");
            for(int i=0; i<stat.bin_num; i++) {
                snprintf(msg, msgLen, "DEPTH STAT:  bin center/count  =   %f  %10ld",
                        stat.bin_centers[i], stat.bin_counts[i]);
                zvmessage(msg, "");
            }
        }
    }

    // Write out the depth file...

    for (int band=0; band<DEPTH_BANDS; band++) {
	for (int line = 0; line < nlo; line++) {
	    zvwrit(out_unit, depth[band] + (line * nso),
			"BAND", band+1, "LINE", line+1, NULL);
	}
    }

    zvclose(out_unit, NULL);

}

////////////////////////////////////////////////////////////////////////
// Open one set of inputs, either XYZ or UVW...
////////////////////////////////////////////////////////////////////////

void open_inputs(int nids, PigFileModel *file_models[], int unit[3],
		int band[3], char *type)
{
    int i;
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
        for (i = 0; i < 3; i++) {

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
	snprintf(msg, msgLen, "MARSDEPTH requires either 1 3-band file or 3 single band files as input for %s", type);
	zvmessage(msg, "");
	zabend();
    }
}

