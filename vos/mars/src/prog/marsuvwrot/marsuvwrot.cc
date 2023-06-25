/* marsrough */
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

#include <iostream>
using namespace std;

/* buffer sizes in main program */
#define MAX_INPUTS 3
#define MAX_NL MARS_MAX_NL
#define MAX_NS MARS_MAX_NS

void open_inputs(int nids, PigFileModel *file_models[], int unit[3],
		int band[3], const char *type);

////////////////////////////////////////////////////////////////////////

void main44()
{
    int i, band, line;
    int count, count2, def;
    const int MSG_LEN = 256;
    char msg[MSG_LEN];

    int nids;
    char mission[64], instrument[64];

    // Inputs

    PigFileModel *file_models[MAX_INPUTS];
    PigCameraModel *camera_in[MAX_INPUTS];
    PigPointingModel *pointing_in[MAX_INPUTS];
    int homogeneous_inputs = TRUE;
    PigCoordSystem *cs, *uvw_cs;
    int uvw_unit[3];
    int uvw_band[3];

    // Outputs
    int out_unit;
    int nlo, nso;

    int use_current;
    double nominal_uvw[3];

    double axis[3];
    double angle;

    double roll;
    double pitch;
    double yaw;

    int use_vector_diff;
    PigVector vector1;
    PigVector vector2;
    double tmp_vec[3];

    int use_quat;
    double input_quat[4];

    int use_xyz;
    PigPoint rot_origin;
    double rot_origin_array[3];

    double angle_rad, roll_rad, pitch_rad, yaw_rad;
    PigQuaternion rotation;
    PigCoordSystem *rot_cs, *rover_cs, *site_cs;

    double *uvw[3];			// input image
    double *rot_uvw[3];			// output image

    zvmessage("MARSUVWROT version 2020-05-26", "");

    // Get the input file list, and set up initial camera/pointing models
    // for each input.  Although we accept one or three inputs only, mars_setup
    // does lots of other nice things for us.

    mars_setup(nids, file_models, camera_in, pointing_in, NULL, cs,
                mission, instrument, homogeneous_inputs,
                MAX_NL, MAX_NS, MAX_INPUTS);

    PigMission *m = PigMission::getMissionObject(mission);

    use_xyz = zvptst("XYZ");

    // Get coord system for input UVW file

    PigCSReference *ref;
    file_models[0]->getDerivedImageCS(ref);
    uvw_cs = m->getCoordSystem(ref);

    snprintf(msg, MSG_LEN, "Interpreting %s values using the %s coordinate frame: %s",
                use_xyz ? "XYZ" : "UVW", uvw_cs->getFrameName(),
		ref->getFullName());
    zvmessage(msg, "");

    // Get the coordinate system to use.
    snprintf(msg, MSG_LEN, "Generating Projected %s using the %s coordinate frame.",
	    use_xyz ? "XYZ" : "UVW", cs->getFrameName());
    zvmessage(msg, "");

    // Get the coordinate system for the roll/pitch/yaw and axis/angle
    // (generally Rover)

    char rot_cs_name[50];
    zvp("ROT_COORD", rot_cs_name, &def);
    rot_cs = m->getCoordSystem(file_models[0], rot_cs_name);
    rover_cs = m->getCoordSystem(file_models[0], "ROVER");
    site_cs = m->getCoordSystem(file_models[0], "SITE");

    // Get parameters

    use_current = zvptst("USE_CURRENT");
    if (use_current) {
	zvparmd("NOMINAL_UVW", nominal_uvw, &count, &def, 3, 0);
	if (count == 0) {
	    zvmessage("Must specify NOMINAL_UVW if USE_CURRENT is on","");
	    zabend();
	}
    }

    //use_axis_angle = FALSE;
    angle = 0.0;
    zvparmd("AXIS", axis, &count, &def, 3, 0);
    zvparmd("ANGLE", &angle, &count2, &def, 1, 0);
    //if (count != 0 && count2 != 0)
    //  use_axis_angle = TRUE;
    if ((count == 0 && count2 != 0) || (count != 0 && count2 == 0)) {
	zvmessage("Must have both AXIS and ANGLE, or neither!", "");
	zabend();
    }

    zvparmd("ROLL", &roll, &count, &def, 1, 0);
    zvparmd("PITCH", &pitch, &count, &def, 1, 0);
    zvparmd("YAW", &yaw, &count, &def, 1, 0);

    angle_rad = PigDeg2Rad(angle);
    roll_rad = PigDeg2Rad(roll);
    pitch_rad = PigDeg2Rad(pitch);
    yaw_rad = PigDeg2Rad(yaw);

    use_vector_diff = FALSE;
    zvparmd("VECTOR1", tmp_vec, &count, &def, 3, 0);
    if (count == 3) {
	use_vector_diff = TRUE;
        vector1 = PigVector(tmp_vec);
    }

    zvparmd("VECTOR2", tmp_vec, &count, &def, 3, 0);
    if (count == 3) {
	if (!use_vector_diff) {
	    zvmessage("VECTOR1 must be specified if VECTOR2 is","");
	    zabend();
	}
        vector2 = PigVector(tmp_vec);
    }
    else {
	if (use_vector_diff) {
	    zvmessage("VECTOR2 must be specified if VECTOR1 is","");
	    zabend();
	}
    }

    use_quat = FALSE;
    zvparmd("QUAT", input_quat, &count, &def, 4, 0);
    if (count == 4)
	use_quat = TRUE;

    zvparmd("ROT_ORIGIN", rot_origin_array, &count, &def, 3, 0);
    rot_origin = PigPoint(rot_origin_array);

    // Open input file(s).  An UVW data could be either 1 3-band file
    // or 3 single band files

    open_inputs(nids, file_models, uvw_unit, uvw_band, use_xyz?"XYZ":"UVW");

    // Open output files.
    // OUT is a single 3-band float file
    //!!!! Should be able to output 3 single-band files too

    zvpcnt("OUT", &count);
    zvunit(&out_unit, "OUT", 1, NULL);
    zvopen(out_unit, "op", "write",
	       "u_ns", file_models[0]->getNS(), "u_nl",file_models[0]->getNL(),
	       "u_nb", 3,
	       "open_act", "sa", "u_org", "bsq",
	       "u_format", "doub", "o_format", "real", NULL);
    zvplabel(out_unit, 0, 1);

    // write output label

    PigLabelModel *labelModel = m->createLabelModel(out_unit);
    // pick the coordinate system to use.
    if (use_xyz)
        labelModel->setXYZ(file_models, nids, cs, "Rotated XYZ", 0.0,
			file_models[0]->getStereoProductId());
    else
        labelModel->setUVW(file_models, nids, cs, "Rotated UVW");

    count = 0;

    // get input image dimensions
    nlo = file_models[0]->getNL();
    nso = file_models[0]->getNS();

    // Allocate memory for input UVW, and output UVW.  The
    // entire images must be in memory for the subroutine.

    for (i=0; i<3; i++) {
	uvw[i] = (double *)malloc(nlo * nso * sizeof(double));
	if (uvw[i] == NULL) {
	    snprintf(msg, MSG_LEN, "Unable to allocate memory for %s input %d",
			use_xyz?"XYZ":"UVW", i);
	    zvmessage(msg, "");
	    zabend();
	}

	rot_uvw[i] = (double *)malloc(nlo * nso * sizeof(double));
	if (rot_uvw[i] == NULL) {
	    snprintf(msg, MSG_LEN, "Unable to allocate memory for rot_uvw output %d", i);
	    zvmessage(msg, "");
	    zabend();
	}
    }

    // Read in the UVW file(s)...

    for (band = 0; band < 3; band++) {
	for (line = 0; line < nlo; line++) {
	    zvread(uvw_unit[band], (uvw[band]) + (line * nso),
			"BAND", uvw_band[band], "LINE", line+1, NULL);
	}
    }

    // Convert coord systems if necessary...

    if (cs != uvw_cs) {
	for (line = 0; line < nlo; line++) {
	    for (int samp = 0; samp < nso; samp++) {
		int index = line * nso + samp;
		PigVector old_uvw(*(uvw[0]+index), *(uvw[1]+index),
					*(uvw[2]+index));

		// If the point is 0,0,0 (meaning not valid), leave it alone

		if (old_uvw.getX() != 0.0 ||
		    old_uvw.getY() != 0.0 ||
		    old_uvw.getZ() != 0.0) {

		    PigPoint new_uvw;
		    if (use_xyz)
		        new_uvw = cs->convertPoint(old_uvw, uvw_cs);
		    else
		        new_uvw = cs->convertVector(old_uvw, uvw_cs);
		    *(uvw[0]+index) = new_uvw.getX();
		    *(uvw[1]+index) = new_uvw.getY();
		    *(uvw[2]+index) = new_uvw.getZ();
		}
	    }
	}
    }

    // Do the work...

    // Create rotation quat.  This goes in four stages.

    // First, get the current tilt, and compare it to the nominal normal
    // to determine the amount of embedding.  Current tilt is simply a
    // vertical vector in the Rover frame converted to Site frame.

    if (use_current) {

	PigVector rvr_tilt(0.0, 0.0, -1.0);
	PigVector site_tilt = site_cs->convertVector(rvr_tilt, rover_cs);

	// Nominal tilt is assumed to be in Site frame.  Cross product gives
	// us the axis; dot product gives us the angle.

	PigVector nominal(nominal_uvw);
	nominal.normalize();			// just in case
	PigVector tilt_axis = nominal * site_tilt;
	tilt_axis.normalize();
	double tilt_angle = acos(nominal % site_tilt);

	snprintf(msg, MSG_LEN, "Embedding tilt (%s frame):  axis = (%lf, %lf, %lf)  angle = %lf (deg)",
		site_cs->getFrameName(),
		tilt_axis.getX(), tilt_axis.getY(), tilt_axis.getZ(),
		PigRad2Deg(tilt_angle));
	zvmessage(msg, "");

	PigQuaternion tilt_q(tilt_axis, tilt_angle);

	// Rotation is in the output frame.  Note that quat2 * quat1 means
	// rotate by quat1 first, then quat2.

	tilt_q = cs->convertQuat(tilt_q, site_cs);
	rotation = tilt_q * rotation;

	PigVector rax = rover_cs->convertVector(tilt_axis, site_cs);
	snprintf(msg, MSG_LEN, "Embedding tilt (%s frame):  axis = (%lf, %lf, %lf)  angle = %lf (deg)",
		rover_cs->getFrameName(),
		rax.getX(), rax.getY(), rax.getZ(),
		PigRad2Deg(tilt_angle));
	zvmessage(msg, "");

    }

    // Second, apply the axis-angle parameters to the rotation

    if (angle_rad != 0.0) {
        PigQuaternion aa_rotq(axis, angle_rad);
	aa_rotq = cs->convertQuat(aa_rotq, rot_cs);

	rotation = aa_rotq * rotation;
    }

    // Third, apply the Euler angle rotations

    if (roll_rad != 0.0 || pitch_rad != 0.0 || yaw_rad != 0.0) {
	PigQuaternion euler_rotq;
	euler_rotq.setEulerAngles(roll_rad, pitch_rad, yaw_rad);
	euler_rotq = cs->convertQuat(euler_rotq, rot_cs);

	rotation = euler_rotq * rotation;
    }

    // Fourth, apply vector difference

    if (use_vector_diff) {
	vector1.normalize();
	vector2.normalize();
	PigVector vd_axis = vector1 * vector2;
	vd_axis.normalize();		// probably redundant
	double vd_angle = acos(vector1 % vector2);
        PigQuaternion vd_rotq(vd_axis, vd_angle);
	snprintf(msg, MSG_LEN,"Vector Difference angle: %f degrees, axis=(%f, %f, %f)",
		PigRad2Deg(vd_angle), vd_axis.getX(),
		vd_axis.getY(), vd_axis.getZ());
	zvmessage(msg, "");

	rotation = vd_rotq * rotation;
    }

    // Fifth, and finally, apply the input quaternion

    if (use_quat) {
	PigQuaternion inp_quat(input_quat[0], input_quat[1],
			       input_quat[2], input_quat[3]);
	rotation = inp_quat * rotation;
    }

    rotation.normalize();

    double qv[4];
    rotation.getComponents(qv);
    snprintf(msg, MSG_LEN, "Final quaternion (%s): (%lf,%lf,%lf,%lf)",
		 cs->getFrameName(), qv[0], qv[1], qv[2], qv[3]);
    zvmessage(msg, "");
    snprintf(msg, MSG_LEN, "Axis:  (%lf,%lf,%lf)  Angle:  %lf (deg)",
		rotation.getU().getX(), rotation.getU().getY(),
		rotation.getU().getZ(), PigRad2Deg(rotation.getTheta()));
    zvmessage(msg, "");

    // Go through each point...

    for (line = 0; line < nlo; line++) {
	for (int samp = 0; samp < nso; samp++) {
	    int index = line * nso + samp;
	    PigVector orig_uvw(*(uvw[0]+index), *(uvw[1]+index),
					*(uvw[2]+index));

	    // Rotate the UVW or XYZ by the given rotation.

	    PigVector new_uvw;

	    // Leave it alone if it's all 0's

	    if (orig_uvw.getX() != 0.0 || orig_uvw.getY() != 0.0 ||
		orig_uvw.getZ() != 0.0) {

	        if (use_xyz) {
	            new_uvw = (rotation * (orig_uvw - rot_origin)) + rot_origin;
	        } else {
	            new_uvw = rotation * orig_uvw;
	            new_uvw.normalize();
	        }
	    }

	    // Save it

	    *(rot_uvw[0]+index) = new_uvw.getX();
	    *(rot_uvw[1]+index) = new_uvw.getY();
	    *(rot_uvw[2]+index) = new_uvw.getZ();

	}
    }

    // Write out the projected uvw file(s)...

    for (int band=0; band < 3; band++) {
        for (line = 0; line < nlo; line++) {
	    zvwrit(out_unit, rot_uvw[band] + (line * nso), "BAND",band+1, "LINE", line+1, NULL);
	}
    }

    zvclose(out_unit, NULL);

}

////////////////////////////////////////////////////////////////////////
// Open one set of inputs, either XYZ or UVW...
////////////////////////////////////////////////////////////////////////

void open_inputs(int nids, PigFileModel *file_models[], int unit[3],
		int band[3], const char *type)
{
    int i;
    const int MSG_LEN = 256;
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
	snprintf(msg, MSG_LEN, "MARSUVWPROJ requires either 1 3-band file or 3 single band files as input for %s", type);
	zvmessage(msg, "");
	zabend();
    }
}

