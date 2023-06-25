////////////////////////////////////////////////////////////////////////
// PigPointMERiddCamera
//
// Pointing model for MER Microscopic Imager(MI) camera located on
// Instrument Deployment Device(IDD) arm.
//
// Currently works like Generic camera with 4 degrees of freedom.
//
////////////////////////////////////////////////////////////////////////

#include "PigPointMERiddCamera.h"
#include "PigMER.h"
#include "PigCameraModel.h"

extern "C" {
#include "mer_idd_kinematic.h"
}

////////////////////////////////////////////////////////////////////////
// Constructor
////////////////////////////////////////////////////////////////////////

PigPointMERiddCamera::PigPointMERiddCamera(PigCameraModel *cm,
					   PigMission *mission, 
					   const char *instrument)
  : PigPointingModel(cm, mission, instrument)
{
    _iddJoint1 = _iddJoint2 = _iddJoint3 = _iddJoint4 = 0.0;

    // This is changed in pointCamera(PigFile *)
    if (_camera_model)
        _pointing_cs = _mission->getCoordSystem(_camera_model->getCoordSystem(),
					"INSTRUMENT");
    else
	_pointing_cs = _mission->getCoordSystem("INSTRUMENT");

    // Read the .point file
    // Naming convention for pointing files: "host_id_idd.point"
    // where host_id = MER1, MER2, SSTB1, SIM2 etc.
    char point_file[255];
    sprintf(point_file, "param_files/%s_idd.point",
				((PigMER *)_mission)->getHostID());

    read_point_info(point_file, ((PigMER *)_mission)->getHostID());

    // Get the pathname for the config files...
    char fn[PIG_MAX_FILENAME_SIZE];
    char parms_path[PIG_MAX_FILENAME_SIZE];
    char msg[1024];
    sprintf(fn, "idd_params/%s", ((PigMER *)_mission)->getHostID());
    FILE *f = PigModelBase::openConfigFile(fn, _parms_path);
    if (f == NULL) {
        sprintf(msg, "Could not find IDD parms path for %s!!", fn);
	printError(msg);
	sprintf(_parms_path, "/tmp/%s", ((PigMER *)_mission)->getHostID());
    } else {
        fclose(f);
    }
  sprintf(msg, "Using IDD parms path %s", _parms_path);
  printInfo(msg);
}

////////////////////////////////////////////////////////////////////////
// Destructor
////////////////////////////////////////////////////////////////////////

PigPointMERiddCamera::~PigPointMERiddCamera()
{
	// nothing to do...
}

////////////////////////////////////////////////////////////////////////
// Point a camera model, given an image.  This does not have to be the
// same image as was given in the constructor, although presumably the
// mission/camera names should match!
////////////////////////////////////////////////////////////////////////

void PigPointMERiddCamera::pointCamera(PigFileModel *file)
{
   if (pointCameraViaLabel(file))
       return;

    PigFileModelMER* fileMER = (PigFileModelMER*) file;
    _pointing_cs = _mission->getCoordSystem(file, NULL);
    _camera_model->setInitialCoordSystemNoTrans(_pointing_cs);

    if (!fileMER->checkIddJoint1Angle())
	printWarning("Warning: no IddJoint1Angle, 0 assumed");
    double iddJoint1Angle = fileMER->getIddJoint1Angle(0);

    if (!fileMER->checkIddJoint2Angle())
	printWarning("Warning: no IddJoint2Angle, 90 assumed");
    double iddJoint2Angle = fileMER->getIddJoint2Angle(90);

    if (!fileMER->checkIddJoint3Angle())
	printWarning("Warning: no IDDJoint3Angle, 0 assumed");
    double iddJoint3Angle = fileMER->getIddJoint3Angle(0);

    if (!fileMER->checkIddJoint4Angle())
	printWarning("Warning: no IDDJoint4Angle, 0 assumed");
    double iddJoint4Angle = fileMER->getIddJoint4Angle(0);

    if (!fileMER->checkIddJoint5Angle())
	printWarning("Warning: no IDDJoint5Angle, 0 assumed");
    double iddJoint5Angle = fileMER->getIddJoint5Angle(0);

    pointCamera(PigRad2Deg(iddJoint1Angle), PigRad2Deg(iddJoint2Angle),
		PigRad2Deg(iddJoint3Angle), PigRad2Deg(iddJoint4Angle), 
		PigRad2Deg(iddJoint5Angle));

}

////////////////////////////////////////////////////////////////////////
// Point the camera using four angles
////////////////////////////////////////////////////////////////////////
void PigPointMERiddCamera::pointCamera(const double iddJoint1,
				       const double iddJoint2,
				       const double iddJoint3,
				       const double iddJoint4,
				       const double iddJoint5)
{

   // saving the local parameters to member variables
    _iddJoint1 = iddJoint1;
    _iddJoint2 = iddJoint2;
    _iddJoint3 = iddJoint3;
    _iddJoint4 = iddJoint4;
    _iddJoint5 = iddJoint5;

  IddJoint joint;

  joint[0] = PigDeg2Rad(iddJoint1);
  joint[1] = PigDeg2Rad(iddJoint2);
  joint[2] = PigDeg2Rad(iddJoint3);
  joint[3] = PigDeg2Rad(iddJoint4);
  joint[4] = PigDeg2Rad(iddJoint5);


  IddPrmsDh dh[IDD_DOF];
  IddPrmsFrame frame;
  IddPrmsTool ptool[IDD_TOOLS];

  IddTool tool = MI;
  
  //!!!! we are making assumption that _pointing_cs is ROVER_FRAME
  PigQuaternion rover_quat = _pointing_cs->getQuaternion();
  double rover_quat_d[4];
  rover_quat.getComponents(rover_quat_d);

  // IMPORTANT NOTE:  The engine wants quaternions in FLIGHT SW ORDER,
  // which means cosine (scalar) term last.  We put it first.  So, swap
  // them around...

  double rover_quat_rev[4];
  rover_quat_rev[0] = rover_quat_d[1];
  rover_quat_rev[1] = rover_quat_d[2];
  rover_quat_rev[2] = rover_quat_d[3];
  rover_quat_rev[3] = rover_quat_d[0];

  mer_idd_setup_kin_params(_parms_path, dh, &frame, ptool);

  IddPose pose;
  merQuaternion rvrQtool;
  // Apply forward kinematics
  mer_idd_fwd_kin(&pose, &rvrQtool, joint, dh, &frame, ptool, tool);

  PigPoint rotation_point(pose.x, pose.y, pose.z);
  PigQuaternion head_rover_q(rvrQtool.q[3], rvrQtool.q[0],
			     rvrQtool.q[1], rvrQtool.q[2]);

  _camera_model->resetCameraLocation();

  _camera_model->moveCamera(_idd_rotation_point, _idd_calibration_quaternion,
			    rotation_point, head_rover_q, _pointing_cs);
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

void PigPointMERiddCamera::pointCamera(const char *obs_id, 
				       char *data_source)
{
    printFatal("PigPointMERiddCamera::pointCamera(obs_id, data_source) not implemented yet!!");
}

////////////////////////////////////////////////////////////////////////
// These functions get and set the pointing and camera position, in
// the given coordinates.  They are *not* intended for pointing
// correction, use the get/setPointingParameters() functions for that.
////////////////////////////////////////////////////////////////////////

void PigPointMERiddCamera::setCameraOrientation(const PigVector 
						     &orientation,
						     PigCoordSystem *cs)
{
    if (!checkCameraModel())
	return;
    printFatal("PigPointMERiddCamera::setCameraOrientation(orientation, cs) not implemented yet!!");
    // Need to implement inverse kinematics
}

////////////////////////////////////////////////////////////////////////
// These functions allow access to the "raw" pointing data, in order
// to accomplish pointing corrections or "tweaks".
// For the IDD camera:
//    params[0] = iddJoint1
//    params[1] = iddJoint2
//    params[2] = iddJoint3
//    params[3] = iddJoint4
//    params[4] = iddJoint5
//
// Angles are in degrees!
// Subclasses could define extra parameters, e.g. for a rover where the
// position is also unknown.
////////////////////////////////////////////////////////////////////////

void PigPointMERiddCamera::getPointingParameters(double params[],
						  const int max_count)
{
    if (max_count >= 1)
	params[0] = _iddJoint1;
    if (max_count >= 2)
	params[1] = _iddJoint2;
    if (max_count >= 3)
	params[2] = _iddJoint3;
    if (max_count >= 4)
	params[3] = _iddJoint4;
    if (max_count >= 5)
	params[4] = _iddJoint5;
}

void PigPointMERiddCamera::setPointingParameters(const double params[],
						const int count)
{
    // Don't re-point if nothing has changed.
    if (count >= 5) {
        if (params[0] != _iddJoint1 || params[1] != _iddJoint2 ||
	    params[2] != _iddJoint3 || params[3] != _iddJoint4 ||
	    params[4] != _iddJoint5)
            pointCamera(params[0], params[1], params[2],
                        params[3], params[4]);
    }
    if (count >= 4) {
        if (params[0] != _iddJoint1 || params[1] != _iddJoint2
                || params[2] != _iddJoint3
                || params[3] != _iddJoint4 )
            pointCamera(params[0], params[1], params[2],
                        params[3], _iddJoint5);
    }
    else if (count == 3) {
        if (params[0] != _iddJoint1 || params[1] != _iddJoint2
                || params[2] != _iddJoint3)
            pointCamera(params[0], params[1], params[2],
                _iddJoint4, _iddJoint5);
    }
    else if (count == 2) {
        if (params[0] != _iddJoint1 || params[1] != _iddJoint2)
            pointCamera(params[0], params[1], _iddJoint3, 
			_iddJoint4, _iddJoint5);
    }
    else if (count == 1) {
        if (params[0] != _iddJoint1)
            pointCamera(params[0], _iddJoint2,
                _iddJoint3, _iddJoint4, _iddJoint5);
    }
}

const char *const PigPointMERiddCamera::getPointingParamName(int i)
{
    switch (i) {
        case 0:
            return "iddJoint1";
        case 1:
            return "iddJoint2";
        case 2:
            return "iddJoint3";
        case 3:
            return "iddJoint4";
        case 4:
            return "iddJoint5";
        default:
            return "Unknown";
    }
}

////////////////////////////////////////////////////////////////////////
// Read in the calibration pointing parameters for a given "point" file.
////////////////////////////////////////////////////////////////////////

void PigPointMERiddCamera::read_point_info(char *filename, const char *host_id)
{
    FILE *inClientFile;
    char line[255];

    // open the file
    
    inClientFile = PigModelBase::openConfigFile(filename, NULL);

    // Default pointing values, in case the .point file isn't found
    // or the requested parameters are not there.
    if (!strcasecmp(host_id, "MER1")) {
        _idd_rotation_point.setXYZ(1.130642, 0.065906, 0.061165);
	double v[4] = {0.768021, 0.11956, 0.183823, -0.601712};
	_idd_calibration_quaternion.setComponents(v);
	v[0] = 1.0; v[1] = 0.0; v[2] = 0.0; v[3] = 0.0;  
	_idd_gimbal_to_rover_quaternion.setComponents(v);
	_pointing_error[0] = _pointing_error[1] = _pointing_error[2] = 
	_pointing_error[3] = _pointing_error[4] = 0.18;  
    }
    if (!strcasecmp(host_id, "MER2")) {
        _idd_rotation_point.setXYZ(0.766387, -0.058079, 0.325119);
	double v[4] = {0.707079, 0.000269, -0.00009, 0.707134};
	_idd_calibration_quaternion.setComponents(v);
	v[0] = 1.0; v[1] = 0.0; v[2] = 0.0; v[3] = 0.0;  
	_idd_gimbal_to_rover_quaternion.setComponents(v);
	_pointing_error[0] = _pointing_error[1] = _pointing_error[2] =
	_pointing_error[3] = _pointing_error[4] = 0.18;  
    }
    if (!strcasecmp(host_id, "SIM2")) {
        _idd_rotation_point.setXYZ(1.21299, -0.204705,-0.454242);
	double v[4] = {0.773418,0.0150538, -0.0331539, 0.632849};
	_idd_calibration_quaternion.setComponents(v);
	v[0] = 1.0; v[1] = 0.0; v[2] = 0.0; v[3] = 0.0;  
	_idd_gimbal_to_rover_quaternion.setComponents(v);
	_pointing_error[0] = _pointing_error[1] = _pointing_error[2] =
	_pointing_error[3] = _pointing_error[4] = 0.18;  	
    }
    if (inClientFile == NULL) {
	    sprintf(line, 
		    "Point file %s could not be opened, using default values",
		    filename);
	    printWarning(line);
    }
    else {
	while (fgets(line, sizeof(line), inClientFile) != NULL) {

	    // pull out the parameters

	    double dum[4];

	    if (strncasecmp(line, "idd_rotation_point", 18) == 0) {
		sscanf(line, "idd_rotation_point = %lf %lf %lf",
				&dum[0], &dum[1], &dum[2]);
		_idd_rotation_point.setXYZ(dum);
	    }
	    if (strncasecmp(line, "idd_calibration_quaternion", 26) == 0) {
		sscanf(line, 
		       "idd_calibration_quaternion = %lf %lf %lf %lf",
		       &dum[0], &dum[1], &dum[2], &dum[3]);
		_idd_calibration_quaternion.setComponents(dum);
	    }
	    if (strncasecmp(line, "idd_gimbal_to_rover_quaternion", 30) == 0) {
		sscanf(line, 
		       "idd_gimbal_to_rover_quaternion = %lf %lf %lf %lf",
		       &dum[0], &dum[1], &dum[2], &dum[3]);
		_idd_gimbal_to_rover_quaternion.setComponents(dum);
	    }
	    if (strncasecmp(line, "idd_pointing_error", 18) == 0) {
	        sscanf(line, "idd_pointing_error = %lf %lf %lf %lf %lf",
		       &_pointing_error[0], &_pointing_error[1],
		       &_pointing_error[2], &_pointing_error[3],
		       &_pointing_error[4]);
	    }
	}
	fclose(inClientFile);
    }
}

void PigPointMERiddCamera::forceCameraRepoint( )
{

    pointCamera(_iddJoint1, _iddJoint2, _iddJoint3, _iddJoint4, _iddJoint5);

}
void PigPointMERiddCamera::getPointingErrorEstimate(double errors[],
						     const int max_count)
{
    int i;
    int n = max_count;
    if (n > 5) n = 5;
    for (i=0; i < n; i++)
	errors[i] = _pointing_error[i];
}
