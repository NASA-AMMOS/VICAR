////////////////////////////////////////////////////////////////////////
// PigPointCamera6dof
//
// Pointing model for cameras.
//
// Normally pointing is done in the Payload(Lander) frame.  
////////////////////////////////////////////////////////////////////////

#include "PigMission.h"
#include "PigCameraModel.h"
#include "PigPointCamera6dof.h"

////////////////////////////////////////////////////////////////////////
// Constructor
////////////////////////////////////////////////////////////////////////

PigPointCamera6dof::PigPointCamera6dof(PigCameraModel *cm,
				       PigMission *mission, 
				       const char *instrument)
                              : PigPointCamera7dof(cm, mission, instrument)
{
    // This is changed in pointCamera(PigFile *)
    if (_camera_model)
	_pointing_cs = _mission->getCoordSystem(
		_camera_model->getCoordSystem(), "INSTRUMENT");
    else
	_pointing_cs = _mission->getCoordSystem("INSTRUMENT");
}

////////////////////////////////////////////////////////////////////////
// Destructor
////////////////////////////////////////////////////////////////////////

PigPointCamera6dof::~PigPointCamera6dof()
{
	// nothing to do...
}

////////////////////////////////////////////////////////////////////////
// Do the actual work of pointing the camera.
//
// Az, el, twist are in degrees.  They are mesured in the given coordinate
// system (if NULL, _pointing_cs, the natural instrument frame, is used)
//
// The idea here is that the stored camera model numbers reflect where
// the camera was pointing during the calibration frame.  So, we un-rotate
// using the calibration pointing (az, then el), and then rotate using the
// desired pointing (el, then az).
////////////////////////////////////////////////////////////////////////

void PigPointCamera6dof::pointCamera(const double azimuth,
		          	       const double elevation,
				       const double twist,
				       const double x,
				       const double y,
				       const double z,
				       PigCoordSystem *cs)
{
    // pass in az/el/twist, and x,y,z

    if (_camera_model == NULL) {
	printFatal("Can't point a NULL camera in PigPoint6dof!");
	return;
    }
   
    // saving the local parameters to member variables

    if (cs != NULL) {			// convert
	double az, el;
	PigPoint xyz(x,y,z);
	_pointing_cs->convertAzEl(azimuth, elevation, az, el, cs);
	_azimuth = az;
	_elevation = el;
	_current_location = _pointing_cs->convertPoint(xyz, cs);
    }
    else {
        _azimuth = azimuth;
        _elevation = elevation;
        _current_location.setXYZ(x,y,z);
    }

    _twist = twist;	// Twist really doesn't need to be converted...

    // Do inverse conversion from angles to quaternion
    PigQuaternion q_final(1.0, 0, 0, 0);
    q_final.setEulerAngles(PigDeg2Rad(_twist), PigDeg2Rad(_elevation), PigDeg2Rad(_azimuth));

//printf("6DOF pointing: az=%f el=%f tw=%f x=%f y=%f z=%f\n", _azimuth, _elevation, _twist, _current_location.getX(), _current_location.getY(), _current_location.getZ());	//!!!!
    // Rotate the camera head from calibration to desired location.
    // We could equivalently provide cal az/el as the initial rotation
    // and not include it in the q rotation.

    _camera_model->resetCameraLocation();

//printf("6DOF: cal loc=%f %f %f, cal quat=%f %f %f %f\n", _calibration_location.getX(), _calibration_location.getY(),_calibration_location.getZ(), _calibration_orientation.getS(), _calibration_orientation.getV().getX(), _calibration_orientation.getV().getY(), _calibration_orientation.getV().getZ());	//!!!!
//printf("6DOF: orig camera orient=%f %f %f\n", _camera_model->getCameraOrientation().getX(),_camera_model->getCameraOrientation().getY(),_camera_model->getCameraOrientation().getZ());	//!!!!
//printf("6dOF: q_final: %f %f %f %f (angle %f axis %f %f %f)\n", q_final.getS(), q_final.getV().getX(), q_final.getV().getY(), q_final.getV().getZ(), q_final.getTheta(), q_final.getU().getX(), q_final.getU().getY(), q_final.getU().getZ());	//!!!!

    _camera_model->moveCamera(_calibration_location, _calibration_orientation,
			      _current_location, q_final, _pointing_cs);
//printf("6DOF: finl camera orient=%f %f %f\n", _camera_model->getCameraOrientation().getX(),_camera_model->getCameraOrientation().getY(),_camera_model->getCameraOrientation().getZ());	//!!!!

}

////////////////////////////////////////////////////////////////////////
// Point the camera in a specific direction.  Simpler than 7dof in that
// we just pass the angles in to pointCamera - but the same work is being
// done.
////////////////////////////////////////////////////////////////////////

void PigPointCamera6dof::setCameraOrientation(const PigVector &orientation,
                                                        PigCoordSystem *cs)
{
    if (!checkCameraModel())
        return;

    // Convert to instrument coordinates

    PigVector inst_pointing = _pointing_cs->convertVector(orientation, cs);

    pointCamera(inst_pointing.getAz(),
		inst_pointing.getEl(),
		0.0,				// twist
		_current_location.getX(),
		_current_location.getY(),
		_current_location.getZ(),
		_pointing_cs);
}

////////////////////////////////////////////////////////////////////////
// These functions allow access to the "raw" pointing data, in order
// to accomplish pointing corrections or "tweaks".
//    params[0] = Azimuth
//    params[1] = Elevation
//    params[2] = Twist
//    params[2] = X position
//    params[3] = Y position
//    params[4] = Z position
//
// Angles are in degrees!
// Subclasses could define extra parameters, e.g. for a rover where the
// position is also unknown.
// Everything is measured in Default Coord Frame.
////////////////////////////////////////////////////////////////////////
void PigPointCamera6dof::getPointingParameters(double params[],
					       const int max_count)
{
    if (max_count >= 1)
	params[0] = _azimuth;
    if (max_count >= 2)
	params[1] = _elevation;
    if (max_count >= 3)
	params[2] = _twist;
    if (max_count >= 4)
	params[3] = _current_location.getX();
    if (max_count >= 5)
	params[4] = _current_location.getY();
    if (max_count >= 6)
	params[5] = _current_location.getZ();
}

void PigPointCamera6dof::setPointingParameters(const double params[],
						     const int count)
{
   // Don't re-point if nothing has changed.
    if (count >= 6) {
        if (params[0] != _azimuth ||
	    params[1] != _elevation ||
	    params[2] != _twist ||
            params[3] != _current_location.getX() ||
            params[4] != _current_location.getY() ||
            params[5] != _current_location.getZ())
            pointCamera(params[0], 
			params[1], 
			params[2],
                        params[3], 
			params[4],
			params[5],
			_pointing_cs);
    }
    else if (count >= 5) {
        if (params[0] != _azimuth ||
	    params[1] != _elevation ||
	    params[2] != _twist ||
            params[3] != _current_location.getX() ||
            params[4] != _current_location.getY())
            pointCamera(params[0], 
			params[1], 
			params[2],
                        params[3], 
			params[4],
			_current_location.getZ(),
	                _pointing_cs);
    }
    else if (count == 4) {
         if (params[0] != _azimuth ||
	     params[1] != _elevation ||
	     params[2] != _twist ||
             params[3] != _current_location.getX())
            pointCamera(params[0], 
			params[1], 
			params[2],
                        params[3],
			_current_location.getY(),
			_current_location.getZ(),
                        _pointing_cs);
    }
    else if (count == 3) {
        if (params[0] != _azimuth || 
	    params[1] != _elevation ||
            params[2] != _twist)
            pointCamera(params[0], 
			params[1], 
			params[2],
			_current_location.getX(),
			_current_location.getY(), 
			_current_location.getZ(),
                        _pointing_cs);
    }
    else if (count == 2) {
        if (params[0] != _azimuth ||
	    params[1] != _elevation)
            pointCamera(params[0], 
			params[1], 
			_twist, 
			_current_location.getX(),
			_current_location.getY(), 
			_current_location.getZ(),
                        _pointing_cs);
    }
    else if (count == 1) {
        if (params[0] != _azimuth)
            pointCamera(params[0], 
			_elevation, 
			_twist,
			_current_location.getX(),
			_current_location.getY(),
			_current_location.getZ(),
                        _pointing_cs);
    }
}

const char *const PigPointCamera6dof::getPointingParamName(int i)
{
    switch (i) {
        case 0:
            return "Azimuth";
        case 1:
            return "Elevation";
        case 2:
            return "Twist";
        case 3:
            return "X";
        case 4:
            return "Y";
        case 5:
            return "Z";	    
        default:
            return "Unknown";
    }
}
////////////////////////////////////////////////////////////////////////
// Read in the calibration pointing parameters for a given "point" file.
////////////////////////////////////////////////////////////////////////

void PigPointCamera6dof::read_point_info(char *filename, const char *host_id)
{
    FILE *inClientFile;
    char line[255];

    // open the file
    
    inClientFile = PigModelBase::openConfigFile(filename, NULL);
    // Default pointing values, in case the .point file isn't found
    // or the necessary values are not in the file.
      _pointing_error[0] = _pointing_error[1] = _pointing_error[2] = 0.0625;
      _pointing_error[3] = _pointing_error[4] = _pointing_error[5] = 0.001;
	

    if (inClientFile == NULL) {
	    sprintf(line, 
		"Point file %s could not be opened, using default values",
		filename);
		printWarning(line);
    }
    else {
	while (fgets(line, sizeof(line), inClientFile) != NULL) {


	    // pull out the parameters
           if (strncasecmp(line, "rac_pointing_error_6dof", 23) == 0) {
                sscanf(line, "rac_pointing_error_6dof = %lf %lf %lf %lf %lf %lf",
                       &_pointing_error[0], &_pointing_error[1],
                       &_pointing_error[2], &_pointing_error[3],
                       &_pointing_error[4], &_pointing_error[5]);
            }
	}
	fclose(inClientFile);
    }
}
