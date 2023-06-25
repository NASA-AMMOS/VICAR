////////////////////////////////////////////////////////////////////////
// PigPointMERpmaCamera
//
// Pointing model for MER Pancam & NavCam cameras located on Mast Assembly.
//
// Normally pointing is done in the Rover frame.
////////////////////////////////////////////////////////////////////////

#include "PigPointMERpmaCamera.h"
#include "PigMER.h"
#include "PigCameraModel.h"

////////////////////////////////////////////////////////////////////////
// Constructor
////////////////////////////////////////////////////////////////////////

PigPointMERpmaCamera::PigPointMERpmaCamera(PigCameraModel *cm,
					   PigMission *mission, 
					   const char *instrument)
                     : PigPointPanTiltCamera(cm, mission, instrument)
{
    // This is changed in pointCamera(PigFile *)
    if (_camera_model)
        _pointing_cs = _mission->getCoordSystem(_camera_model->getCoordSystem(),
                                        "INSTRUMENT");
    else
        _pointing_cs = _mission->getCoordSystem("INSTRUMENT");

    // Read the .point file
    // Note:  same file for All PMA cameras.
    // Naming convention for pointing files: "host_id_pma.point"
    // where host_id = MER1, MER2, SSTB1 etc.
    char point_file[255];
    sprintf(point_file, "param_files/%s_pma.point",
				((PigMER *)_mission)->getHostID());

    read_point_info(point_file, ((PigMER *)_mission)->getHostID());
}

////////////////////////////////////////////////////////////////////////
// Destructor
////////////////////////////////////////////////////////////////////////

PigPointMERpmaCamera::~PigPointMERpmaCamera()
{
	// nothing to do...
}

////////////////////////////////////////////////////////////////////////
// Point a camera model, given an image file.  This does not have to be the
// same image as was given in the constructor, although presumably the
// mission/camera names should match!
////////////////////////////////////////////////////////////////////////

void PigPointMERpmaCamera::pointCamera(PigFileModel *file)
{
    if (pointCameraViaLabel(file))
        return;

    PigFileModelMER *fileMER = (PigFileModelMER*) file;
    _pointing_cs = _mission->getCoordSystem(file, NULL);

    _camera_model->setInitialCoordSystemNoTrans(_pointing_cs);

    // Az/El values are stored in radians in a label

    if (!fileMER->checkAzimuth())
	printWarning("Warning: no PMA Azimuth value found, 0 assumed");
    double azimuth = fileMER->getAzimuth(0.0);

    if (!fileMER->checkElevation())
	printWarning("Warning: no PMA Elevation value found, 0 assumed");
    double elevation = fileMER->getElevation(0.0);

    pointCamera(PigRad2Deg(azimuth), PigRad2Deg(elevation), _pointing_cs);
}

////////////////////////////////////////////////////////////////////////
// Do the actual work of pointing the camera.
//
// Az and el are in degrees.  They are mesured in the given coordinate
// system (if NULL, _pointing_cs, the natural instrument frame, is used)
//
// There are two two kinds of az/el values: Motor values that we are 
// getting from the label, and idealized values, which we use in the
// kinematics calculations.
// The formulas are:
// idealized_az|el = motor_az|el  + pma_gimb_off_azimuth|elevation
//  
// Because Motor values match the label, we want the exposed pointing 
// parameters(used in nav files, MICA, etc.) to be the Motor angles.  
// Thus we add angle offsets only AFTER _azimuth and _elevation have been 
// assigned.  After this assignment is done, we add offsets to compute
// q_el, g_az.  That way only this function pointCamera(az, el, cs) knows
// abouth gimbal offsets.
////////////////////////////////////////////////////////////////////////

void PigPointMERpmaCamera::pointCamera(const double az,
		          	       const double el,
				       PigCoordSystem *cs )
{
    double elevation = el;
    double azimuth = az;

    static PigQuaternion identity;		// no rotation
    static PigVector y_axis(0.0, 1.0, 0.0);
    static PigVector z_axis(0.0, 0.0, 1.0);


    if (cs != NULL)				// convert
	    _pointing_cs->convertAzElDegrees(az, el, azimuth, elevation, cs);

    _azimuth = azimuth;
    _elevation = elevation;

    // Apply az/el offsets from Motor to Idealized angles.  Corresponds 
    // to set_pma_gimb_off command in the flight SW. The formulas are:
    // idealized_az|el = motor_az|el + pma_gimb_off_azimuth|elevation
    
    azimuth = azimuth + PigRad2Deg(_pma_gimb_off_azimuth);
    elevation = elevation + PigRad2Deg(_pma_gimb_off_elevation);
    
    _rotation_point = _pma_rotation_point;

    PigQuaternion q_el(y_axis, 
	       PigDeg2Rad(-elevation) * _pointing_cs->getElevationDirection());
    PigQuaternion q_az(z_axis, PigDeg2Rad(azimuth));

    PigQuaternion head_gimbal_q = q_az * q_el;

    // Combine to get the head to rover quaternion
    PigQuaternion head_rover_q = 
                           _pma_gimbal_to_rover_quaternion * head_gimbal_q;

    _camera_model->resetCameraLocation();

    _camera_model->moveCamera(_rotation_point, _pma_calibration_quaternion,
			      _rotation_point, head_rover_q, _pointing_cs);
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

void PigPointMERpmaCamera::pointCamera(const char *obs_id, 
					    char *data_source)
{
    printFatal("PigPointMERpmaCamera::pointCamera(obs_id, data_source) not implemented yet!!");
}

////////////////////////////////////////////////////////////////////////
// These functions get and set the pointing and camera position, in
// the given coordinates.  They are *not* intended for pointing
// correction, use the get/setPointingParameters() functions for that.
////////////////////////////////////////////////////////////////////////

void PigPointMERpmaCamera::setCameraOrientation(const PigVector &orientation,
												PigCoordSystem *cs)
{
    if (!checkCameraModel())
	return;

    // The rotations must be done in the "natural" CS frame to mimic the az/el
    // ... NOT in Fixed coordinates, which would introduce a twist!
  
    pointCamera(PigRad2Deg(cs->getAz(orientation)), 
				PigRad2Deg(cs->getEl(orientation)), 
				cs);
}

void PigPointMERpmaCamera::getPointingParameters(double params[],
												 const int max_count)
{
    if (max_count >= 1)
	params[0] = _azimuth;
    if (max_count >= 2)
	params[1] = _elevation;
}

void PigPointMERpmaCamera::setPointingParameters(const double params[],
												 const int count)
{
    // Don't re-point if nothing has changed.
    if (count >= 2) {
        if (params[0] != _azimuth || params[1] != _elevation)
            pointCamera(params[0], params[1], _pointing_cs);
    }
    else if (count == 1) {
        if (params[0] != _azimuth)
            pointCamera(params[0], _elevation, _pointing_cs);
    }
}

const char *const PigPointMERpmaCamera::getPointingParamName(int i)
{
    switch (i) {
        case 0:
            return "Azimuth";
        case 1:
            return "Elevation";
        default:
            return "Unknown";
    }
}
////////////////////////////////////////////////////////////////////////
// Read in the calibration pointing parameters for a given "point" file.
////////////////////////////////////////////////////////////////////////

void PigPointMERpmaCamera::read_point_info(char *filename, const char *host_id)
{
    FILE *inClientFile;
    char line[255];

    // open the file
    
    inClientFile = PigModelBase::openConfigFile(filename, NULL);


    // Default pointing values, in case the .point file isn't found
    // or the necessary values are not in the file.
    if (!strcasecmp(host_id, "MER1")) {
      _pma_rotation_point.setXYZ(0.458, 0.028, -1.097);
      double v[4] = {0.000793, 0.149563, -0.0001199, 0.988752};
      _pma_calibration_quaternion.setComponents(v);
      v[0] = 1.0; v[1] = 0.0; v[2] = 0.0; v[3] = 0.0;  
      _pma_gimbal_to_rover_quaternion.setComponents(v);
      _pma_gimb_off_azimuth = 0.0;
      _pma_gimb_off_elevation = 0.0;
    }
    else if (!strcasecmp(host_id, "MER2")) {
      _pma_rotation_point.setXYZ(0.458, 0.028, -1.097);
      double v[4] = {0.988799, -0.000001, -0.149253, -0.00007};
      _pma_calibration_quaternion.setComponents(v);
      v[0] = 1.0; v[1] = 0.0; v[2] = 0.0; v[3] = 0.0;  
      _pma_gimbal_to_rover_quaternion.setComponents(v);
      _pma_gimb_off_azimuth = 0.0;
      _pma_gimb_off_elevation = 0.0;
    }
    else if (!strcasecmp(host_id, "SIM2")) {
      _pma_rotation_point.setXYZ(0.458, 0.028, -1.097);
      double v[4] = {0.993162, -0.00438944, -0.10968, -0.039747};
      _pma_calibration_quaternion.setComponents(v);
      v[0] = 1.0; v[1] = 0.0; v[2] = 0.0; v[3] = 0.0;  
      _pma_gimbal_to_rover_quaternion.setComponents(v);
      _pma_gimb_off_azimuth = 0.0;
      _pma_gimb_off_elevation = 0.0;
    }

    _pointing_error[0] = _pointing_error[1]=0.077;
    _pointing_error[2] = 0.06;

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

	    if (strncasecmp(line, "pma_rotation_point", 18) == 0) {
		sscanf(line, "pma_rotation_point = %lf %lf %lf",
				&dum[0], &dum[1], &dum[2]);
		_pma_rotation_point.setXYZ(dum);
	    }
	    if (strncasecmp(line, "pma_calibration_quaternion", 26) == 0) {
		sscanf(line, 
		       "pma_calibration_quaternion = %lf %lf %lf %lf",
		       &dum[0], &dum[1], &dum[2], &dum[3]);
		_pma_calibration_quaternion.setComponents(dum);
	    }
	    if (strncasecmp(line, "pma_gimbal_to_rover_quaternion", 30) == 0) {
		sscanf(line, 
		       "pma_gimbal_to_rover_quaternion = %lf %lf %lf %lf",
		       &dum[0], &dum[1], &dum[2], &dum[3]);
		_pma_gimbal_to_rover_quaternion.setComponents(dum);
	    }
	    if (strncasecmp(line, "pma_pointing_error", 18) == 0) {
	        sscanf(line, "pma_pointing_error = %lf %lf %lf",
		       &_pointing_error[0], &_pointing_error[1], &_pointing_error[2]);
	    }
	    if (strncasecmp(line, "pma_gimb_off_azimuth", 20) == 0) {
	        sscanf(line, "pma_gimb_off_azimuth = %lf",
		       &_pma_gimb_off_azimuth);
	    }
	    if (strncasecmp(line, "pma_gimb_off_elevation", 22) == 0) {
	        sscanf(line, "pma_gimb_off_elevation = %lf",
		       &_pma_gimb_off_elevation);
	    }
	}
	fclose(inClientFile);
    }
}
