////////////////////////////////////////////////////////////////////////
// PigPointMSLmastCamera
//
// Pointing model for MSL MastCam, NavCam, & ChemCam cameras.
//
// The kinematics are copied from the FSW.  The math is slightly different
// but amounts to the same thing as used on MER/PHX.
//
// We do not inherit from PigPointPanTiltCamera because we do not want
// a cs object to be provided to pointCamera.  The "az/el" values are joint
// angles so they're not really expressed in any specific coord system.
// So, having a CS parameter implies the ability to do a conversion that
// does not make sense here.
////////////////////////////////////////////////////////////////////////

#include "PigPointMSLmastCamera.h"
#include "PigMSL.h"
#include "PigCameraModel.h"

////////////////////////////////////////////////////////////////////////
// Constructor
////////////////////////////////////////////////////////////////////////

PigPointMSLmastCamera::PigPointMSLmastCamera(PigCameraModel *cm,
					   PigMission *mission, 
					   const char *instrument)
                     : PigPointingModel(cm, mission, instrument)
{
    _azimuth = _elevation = 0.0;

    // This is changed in pointCamera(PigFile *)
    _pointing_cs = cm->getCoordSystem();

    // Read the .point file
    // Note:  same file for All MAST cameras.
    // Naming convention for pointing files: "host_id_mast.point"
    // where host_id = MSL, MSLSIM, etc.
    char point_file[255];
    sprintf(point_file, "param_files/%s_mast.point",
				((PigMSL *)_mission)->getHostID());

    read_point_info(point_file, ((PigMSL *)_mission)->getHostID());
}

////////////////////////////////////////////////////////////////////////
// Destructor
////////////////////////////////////////////////////////////////////////

PigPointMSLmastCamera::~PigPointMSLmastCamera()
{
	// nothing to do...
}

////////////////////////////////////////////////////////////////////////
// Point a camera model, given an image file.  This does not have to be the
// same image as was given in the constructor, although presumably the
// mission/camera names should match!
////////////////////////////////////////////////////////////////////////

void PigPointMSLmastCamera::pointCamera(PigFileModel *file)
{
    if (pointCameraViaLabel(file))
        return;

    PigFileModelMSL *fileMSL = (PigFileModelMSL*) file;
    _pointing_cs = _mission->getCoordSystem(file, NULL);

    _camera_model->setInitialCoordSystemNoTrans(_pointing_cs);

    // Az/El values are stored in radians in a label

    if (!fileMSL->checkAzimuth())
	printWarning("Warning: no MAST Azimuth value found, 0 assumed");
    double azimuth = fileMSL->getAzimuth(0.0);

    if (!fileMSL->checkElevation())
	printWarning("Warning: no MAST Elevation value found, 0 assumed");
    double elevation = fileMSL->getElevation(0.0);

    pointCamera(PigRad2Deg(azimuth), PigRad2Deg(elevation));
// printf("TODD: focus = %d\n", file->getInstrumentFocusPosition(0));	//!!!!
// printf("TODD: subframe 1-based line = %d, samp = %d\n", file->getFirstLine(0), file->getFirstLineSample(0));	//!!!!
}

////////////////////////////////////////////////////////////////////////
// Do the actual work of pointing the camera.
//
// Az and el are in degrees.  They are JOINT angles, which for MSL do not
// corrspond to anything useful (they are angles from the hard stop).
// Straight ahead/level is not 0,0 but is az=181 el=91 (nominal).  el=0 is
// one degree past stright down, and the el range is 0-182.  Likewise az=0
// is one degree past backward, and the az range is 0-362.  The hard stops
// go one degree extra on each extreme.
////////////////////////////////////////////////////////////////////////

#define EPS 1.0e-10

void PigPointMSLmastCamera::pointCamera(const double az,
		          	       const double el)
{
    _azimuth = az;
    _elevation = el;

    double elevation = PigDeg2Rad(el);
    double azimuth = PigDeg2Rad(az);

    // The following is cribbed from FSW, module sgnc_rsm_kin.c
    // It has been converted to use PigVectors et al but is otherwise identical

    /* let's make home_az home_el to be initial condition, i.e. let's */
    /* rotate el_axis around az_axis on delta angle home_az-el_azimuth */
    /* (really, - (el_azimuth-home_az) ) */

    double th1_d = _home_az - _el_azimuth;
 
    PigQuaternion rot1(_az_axis, th1_d);
    PigVector el_axis_rot = rot1 * _el_axis;

    /* now let's rotate el_point around az_axis by the same angle */

    PigVector el_point_rot = rot1 * (_el_point - _az_point) + _az_point;

    /* Transform reference such that at "Home" th_az and th_el are zeros */

    double th_az = azimuth - _home_az;
    double th_el = elevation - _home_el;
  
    /* let's compute reference point position at home configuration */

    /* let's check that axis are not parallel (they really shouldn't be!) */

    PigVector Vtmp1 = _az_axis * el_axis_rot;

    PigPoint pr = el_point_rot;	// Backup in case they're parallel

    if (Vtmp1.magnitude_sq() > EPS) {
        /* axis not parallel so we can find meaningful pr */
        double det = el_axis_rot % _az_axis;

        double alpha = (_az_point - el_point_rot) % el_axis_rot
			 + ((el_point_rot - _az_point) % _az_axis) * det;
        alpha = alpha / (1.0 - det * det);
        pr = el_point_rot + el_axis_rot * alpha;
    }

    /* Now we need to rotate p_r around az_axis by angle th_az */

    PigQuaternion rot2(_az_axis, th_az);
    PigPoint pos = rot2 * (pr - _az_point) + _az_point;

    /* let's find orientation of the reference point after rot. th1, th2 */
    /* there are two rotations: q2 - rotation around v2 on th2 and          */
    /* q1 - rotation aroud v1 on th1. resulting rotation q = q1 q2;         */
  
    PigQuaternion q1(_az_axis, th_az);

    PigQuaternion q2(el_axis_rot, th_el);
 
    PigQuaternion quat = q1 * q2;

    // Rotate camera from initial to final condition, calculated above.

    _camera_model->resetCameraLocation();

    _camera_model->moveCamera(_rsm_calibration_position,
			      _rsm_calibration_quaternion,
				pos, quat, _pointing_cs);

// printf("TODD: cal pose: xyz=(%f %f %f) quat(s first)=(%f %f %f %f\n", _rsm_calibration_position.getX(), _rsm_calibration_position.getY(), _rsm_calibration_position.getZ(), _rsm_calibration_quaternion.getS(), _rsm_calibration_quaternion.getV().getX(), _rsm_calibration_quaternion.getV().getY(), _rsm_calibration_quaternion.getV().getZ());	//!!!!
// printf("TODD: final pose: xyz=(%f %f %f) quat(s first)=(%f %f %f %f\n", pos.getX(), pos.getY(), pos.getZ(), quat.getS(), quat.getV().getX(), quat.getV().getY(), quat.getV().getZ());	//!!!!
}

////////////////////////////////////////////////////////////////////////
// Point a camera model, given an image or an observation ID.  This
// does not have to be the same image as was given in the constructor,
// although presumably the mission/camera names should match!
// data_source is included mainly to allow the overloading of these
// two functions, but is intended to specify where to get the potining
// data from, e.g. SPICE, database, etc.  Just pass NULL for this subclass.
////////////////////////////////////////////////////////////////////////

//!!!! NOT IMPLEMENTED YET !!!!   Main problem is where to get info from if
// not label!!!!

void PigPointMSLmastCamera::pointCamera(const char *obs_id, 
					    char *data_source)
{
    printFatal("PigPointMSLmastCamera::pointCamera(obs_id, data_source) not implemented yet!!");
}

////////////////////////////////////////////////////////////////////////
// These functions get and set the pointing and camera position, in
// the given coordinates.  They are *not* intended for pointing
// correction, use the get/setPointingParameters() functions for that.
//
// For MSL, we convert the given az/el to the instrument (rover nav) frame,
// then convert them to joint angles and call the normal kinematics.
// This is not entirely correct... but get/setCameraOrientation() are
// not necessarily matched anyway.  The only requirement is that the camera
// be kind of pointed thatta way.  So for the extant uses of setCO(), this
// is good enough.
////////////////////////////////////////////////////////////////////////

void PigPointMSLmastCamera::setCameraOrientation(const PigVector &orientation,
												PigCoordSystem *cs)
{
    if (!checkCameraModel())
	return;

    // The rotations must be done in the "natural" CS frame to mimic the az/el
    // ... NOT in Fixed coordinates, which would introduce a twist!

    PigVector inst_pointing = _pointing_cs->convertVector(orientation, cs);
  
    double az = _pointing_cs->getAz(inst_pointing);
    double el = _pointing_cs->getEl(inst_pointing);

    // Convert to joint angles on the way

    pointCamera(PigRad2Deg(az + _home_az),
		PigRad2Deg(el + _home_el));
}

////////////////////////////////////////////////////////////////////////
// These functions allow access to the "raw" pointing data, in order
// to accomplish pointing corrections or "tweaks".
// For RSM Cameras:
//    params[0] = Joint Azimuth
//    params[1] = Joint Elevation
// Angles are in degrees!
// Note that twist is not allowed.
// Subclasses could define extra parameters, e.g. for a rover where the
// position is also unknown.
////////////////////////////////////////////////////////////////////////

void PigPointMSLmastCamera::getPointingParameters(double params[],
												 const int max_count)
{
    if (max_count >= 1)
	params[0] = _azimuth;
    if (max_count >= 2)
	params[1] = _elevation;
}

void PigPointMSLmastCamera::setPointingParameters(const double params[],
												 const int count)
{
    // Don't re-point if nothing has changed.
    if (count >= 2) {
        if (params[0] != _azimuth || params[1] != _elevation)
            pointCamera(params[0], params[1]);
    }
    else if (count == 1) {
        if (params[0] != _azimuth)
            pointCamera(params[0], _elevation);
    }
}

void PigPointMSLmastCamera::forceCameraRepoint()
{
    pointCamera(_azimuth, _elevation);
}

const char *const PigPointMSLmastCamera::getPointingParamName(int i)
{
    switch (i) {
        case 0:
            return "Joint Azimuth";
        case 1:
            return "Joint Elevation";
        default:
            return "Unknown";
    }
}

void PigPointMSLmastCamera::getPointingErrorEstimate(double errors[],
				const int max_count)
{
    int i;
    int n = max_count;
    if (n > 2) n = 2;		// only 2 params for this class

    for (i=0; i < n; i++)
	errors[i] = _pointing_error[i];
}

////////////////////////////////////////////////////////////////////////
// Read in the calibration pointing parameters for a given "point" file.
////////////////////////////////////////////////////////////////////////

void PigPointMSLmastCamera::read_point_info(char *filename, const char *host_id)
{
    FILE *inClientFile;
    char line[255];

    // open the file
    
    inClientFile = PigModelBase::openConfigFile(filename, NULL);


    // Default pointing values, in case the .point file isn't found
    // or the necessary values are not in the file.
    // These are the as-measured values.
    if (!strcasecmp(host_id, "MSL")) {
	_home_az = 3.167345;
	_home_el = 1.588171;
	_az_point = PigPoint(0.714803, 0.559424, -1.109344);	// RMECH
	_az_axis = PigVector(-0.001, 0.0, 1.0);
	_el_point = PigPoint(0.714668, 0.561701, -0.785576);	// RMECH
	_el_axis = PigVector(-0.022, 1.0, 0.0);
	_el_azimuth = 0.0;
    }

    // For the testbed, use the nominal (as-designed) values.
    else if (!strcasecmp(host_id, "MSLSIM")) {
	_home_az = 3.1590460;
	_home_el = 1.5882497;
	_az_point = PigPoint(0.71398, 0.55905, -0.78589);	// RMECH
	_az_axis = PigVector(0.0, 0.0, 1.0);
	_el_point = PigPoint(0.71398, 0.55905, -0.78589);	// RMECH
	_el_axis = PigVector(0.0, 1.0, 0.0);
	_el_azimuth = 3.1590460;
    }

    _rsm_calibration_position = PigPoint(0.804, 0.55905, -0.78589);	// RNAV
    _rsm_calibration_quaternion = PigQuaternion();	// identity
    _rmech_to_rnav = PigPoint(0.09002, 0.0, 0.0);
    _pointing_error[0] = 0.063;
    _pointing_error[1] = 0.063;
    _pointing_error[2] = 0.06;
    _pointing_error[3] = 0.001;

    if (inClientFile == NULL) {
	    sprintf(line, 
		"Point file %s could not be opened, using default values",
		filename);
		printWarning(line);
    }
    else {
	while (fgets(line, sizeof(line), inClientFile) != NULL) {

	    // Skip comments.  This is not really necessary since a comment
	    // won't match any name below, but it makes me more comfortable.

	    if (line[0] == '#' || line[0] == ';' || line[0] == '!' ||
		(line[0] == '/' && line[1] == '/'))
		continue;			// skip comment line

	    // pull out the parameters

	    double dum[4];

	    if (strncasecmp(line, "home_az", 7) == 0) {
		sscanf(line, "home_az = %lf", &_home_az);
	    }
	    if (strncasecmp(line, "home_el", 7) == 0) {
		sscanf(line, "home_el = %lf", &_home_el);
	    }
	    if (strncasecmp(line, "az_point", 8) == 0) {
		sscanf(line, "az_point = %lf %lf %lf",
		       &dum[0], &dum[1], &dum[2]);
		_az_point.setXYZ(dum);
	    }
	    if (strncasecmp(line, "az_axis", 7) == 0) {
		sscanf(line, "az_axis = %lf %lf %lf",
		       &dum[0], &dum[1], &dum[2]);
		_az_axis.setXYZ(dum);
	    }
	    if (strncasecmp(line, "el_point", 8) == 0) {
		sscanf(line, "el_point = %lf %lf %lf",
		       &dum[0], &dum[1], &dum[2]);
		_el_point.setXYZ(dum);
	    }
	    if (strncasecmp(line, "el_axis", 7) == 0) {
		sscanf(line, "el_axis = %lf %lf %lf",
		       &dum[0], &dum[1], &dum[2]);
		_el_axis.setXYZ(dum);
	    }
	    if (strncasecmp(line, "el_azimuth", 10) == 0) {
		sscanf(line, "el_azimuth = %lf", &_el_azimuth);
	    }

	    if (strncasecmp(line, "rsm_calibration_position", 24) == 0) {
		sscanf(line, "rsm_calibration_position = %lf %lf %lf",
		       &dum[0], &dum[1], &dum[2]);
		_rsm_calibration_position.setXYZ(dum);
	    }

	    if (strncasecmp(line, "rsm_calibration_quaternion", 26) == 0) {
		sscanf(line, "rsm_calibration_quaternion = %lf %lf %lf %lf",
		       &dum[0], &dum[1], &dum[2], &dum[3]);
		_rsm_calibration_quaternion.setComponents(dum);
	    }

	    if (strncasecmp(line, "rmech_to_rnav", 13) == 0) {
		sscanf(line, "rmech_to_rnav = %lf %lf %lf",
		       &dum[0], &dum[1], &dum[2]);
		_rmech_to_rnav.setXYZ(dum);
	    }

	    if (strncasecmp(line, "mast_pointing_error", 19) == 0) {
	        int num = sscanf(line, "mast_pointing_error = %lf %lf %lf %lf",
			&_pointing_error[0], &_pointing_error[1],
			&_pointing_error[2], &_pointing_error[3]);
		if (num < 4)		// [3] is new, may not be in all files
		    _pointing_error[3] = 0.001;
	    }
	}
	fclose(inClientFile);
    }

    // Convert the points from ROVER MECH to ROVER NAV frame
    // Unit vectors are identical

    _az_point = _az_point + _rmech_to_rnav;
    _el_point = _el_point + _rmech_to_rnav;

    _az_axis.normalize();
    _el_axis.normalize();
}

