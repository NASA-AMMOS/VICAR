////////////////////////////////////////////////////////////////////////
// PigPointPHXracCamera6dof
//
// Pointing model for PHX RAC cameras.
//
// Normally pointing is done in the Payload(Lander) frame.  
////////////////////////////////////////////////////////////////////////

#include "PigPHX.h"
#include "PigCameraModel.h"
#include "PigPointPHXracCamera6dof.h"

////////////////////////////////////////////////////////////////////////
// Constructor
////////////////////////////////////////////////////////////////////////

PigPointPHXracCamera6dof::PigPointPHXracCamera6dof(PigCameraModel *cm,
					       PigMission *mission, 
					       const char *instrument)
                              : PigPointPHXracCamera(cm, mission, instrument)
{
    // This is changed in pointCamera(PigFile *)
    if (_camera_model)
	_pointing_cs = _mission->getCoordSystem(
		_camera_model->getCoordSystem(), "Payload");
    else
	_pointing_cs = _mission->getCoordSystem(
			_camera_model->getCoordSystem(), "INSTRUMENT");

    // Read the .point file only for this subclass specific info
    // the rest is done in super class
    // Naming convention for pointing files: "host_id_rac.point"
    // where host_id = PHX
    char point_file[255];
    sprintf(point_file, "param_files/PHX_%s_rac.point",
					((PigPHX *)_mission)->getHostID());

    read_point_info_6dof(point_file, ((PigPHX *)_mission)->getHostID());
}

////////////////////////////////////////////////////////////////////////
// Destructor
////////////////////////////////////////////////////////////////////////

PigPointPHXracCamera6dof::~PigPointPHXracCamera6dof()
{
	// nothing to do...
}
////////////////////////////////////////////////////////////////////////
// Point a camera model, given an image file.  This does not have to be the
// same image as was given in the constructor, although presumably the
// mission/camera names should match!
////////////////////////////////////////////////////////////////////////

void PigPointPHXracCamera6dof::pointCamera(PigFileModel *file)
{
    if (pointCameraViaLabel(file))
        return;

    PigFileModelPHX *filePHX = (PigFileModelPHX*) file;
    _pointing_cs = _mission->getCoordSystem(file, NULL);

    _camera_model->setInitialCoordSystemNoTrans(_pointing_cs);

    PigPoint location = filePHX->getArticulationDevLocation(_rac_calibration_location);
    PigQuaternion orientation = filePHX->getArticulationDevOrient(_rac_calibration_orientation);
    
    orientation.getEulerAngles(_twist, _elevation, _azimuth);
    
    _azimuth = PigRad2Deg(_azimuth);
    _elevation = PigRad2Deg(_elevation);
    _twist = PigRad2Deg(_twist);
    
    pointCamera(_azimuth, _elevation, _twist,
                location.getX(), location.getY(), location.getZ(), 
	        _pointing_cs);
    

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

void PigPointPHXracCamera6dof::pointCamera(const double azimuth,
		          	       const double elevation,
				       const double twist,
				       const double x,
				       const double y,
				       const double z,
				       PigCoordSystem *cs)
{
    // pass in az/el/twist, and x,y,z

    if (_camera_model == NULL) {
	printFatal("Can't point a NULL camera in PigPointPHXrac6dof!");
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

    // Rotate the camera head from calibration to desired location.
    // We could equivalently provide cal az/el as the initial rotation
    // and not include it in the q rotation.

    _camera_model->resetCameraLocation();

    _camera_model->moveCamera(_rac_calibration_location, _rac_calibration_orientation,
			      _current_location, q_final, _pointing_cs);

}

////////////////////////////////////////////////////////////////////////
// These functions allow access to the "raw" pointing data, in order
// to accomplish pointing corrections or "tweaks".
// For the RAC camera:
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
// Everything is measured in Payload.
////////////////////////////////////////////////////////////////////////
void PigPointPHXracCamera6dof::getPointingParameters(double params[],
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

void PigPointPHXracCamera6dof::setPointingParameters(const double params[],
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

const char *const PigPointPHXracCamera6dof::getPointingParamName(int i)
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

void PigPointPHXracCamera6dof::read_point_info_6dof(char *filename, const char *host_id)
{
    FILE *inClientFile;
    char line[255];

    // open the file
    
    inClientFile = PigModelBase::openConfigFile(filename, NULL);
    // Default pointing values, in case the .point file isn't found
    // or the necessary values are not in the file.
    // should be the same both for FM and EM
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
