////////////////////////////////////////////////////////////////////////
// PigPointPHXracCamera
//
// Pointing model for PHX RAC cameras.
//
// Normally pointing is done in the Payload(Lander) frame.  
////////////////////////////////////////////////////////////////////////

#include "PigPointPHXracCamera.h"
#include "PigPHX.h"
#include "PigCameraModel.h"

////////////////////////////////////////////////////////////////////////
// Constructor
////////////////////////////////////////////////////////////////////////

PigPointPHXracCamera::PigPointPHXracCamera(PigCameraModel *cm,
					   PigMission *mission, 
					   const char *instrument)
                     : PigPointingModel(cm, mission, instrument)
{
    // This is changed in pointCamera(PigFile *)
    if (_camera_model)
	_pointing_cs = _mission->getCoordSystem(
			_camera_model->getCoordSystem(), "INSTRUMENT");
    else
        _pointing_cs = _mission->getCoordSystem("INSTRUMENT");

    // Read the .point file
    // Naming convention for pointing files: "host_id_rac.point"
    // where host_id = PHX
    char point_file[255];
    sprintf(point_file, "param_files/PHX_%s_rac.point",
				((PigPHX *)_mission)->getHostID());

    read_point_info(point_file, ((PigPHX *)_mission)->getHostID());
}

////////////////////////////////////////////////////////////////////////
// Destructor
////////////////////////////////////////////////////////////////////////

PigPointPHXracCamera::~PigPointPHXracCamera()
{
	// nothing to do...
}

////////////////////////////////////////////////////////////////////////
// Point a camera model, given an image file.  This does not have to be the
// same image as was given in the constructor, although presumably the
// mission/camera names should match!
////////////////////////////////////////////////////////////////////////

void PigPointPHXracCamera::pointCamera(PigFileModel *file)
{
    if (pointCameraViaLabel(file))
        return;

    PigFileModelPHX *filePHX = (PigFileModelPHX*) file;
    _pointing_cs = _mission->getCoordSystem(file, NULL);

    _camera_model->setInitialCoordSystemNoTrans(_pointing_cs);

    PigPoint location = filePHX->getArticulationDevLocation(_rac_calibration_location);
    PigQuaternion orientation = filePHX->getArticulationDevOrient(_rac_calibration_orientation);
    pointCamera(location, orientation, _pointing_cs);

}

///////////////////////////////////////////////////////////////////////
// Point the camera using position of the camera and orientation expressed
// as a unit quaternion.
//
///////////////////////////////////////////////////////////////////////
void PigPointPHXracCamera::pointCamera(PigPoint &position,
				       PigQuaternion &orientation,
				       PigCoordSystem *cs)
{
    PigPoint pos = position;
    PigQuaternion orient = orientation;
    if (cs != NULL) {			// convert
	pos = _pointing_cs->convertPoint(position, cs);
	orient = _pointing_cs->convertQuat(orientation, cs);
    }

    _current_location.setXYZ(pos.getX(), pos.getY(), pos.getZ());
    double quat[4];
    orient.getComponents(quat);
    _current_orientation.setComponents(quat);

    // Rotate the camera head from calibration to desired location.
    // We could equivalently provide cal az/el as the initial rotation
    // and not include it in the q rotation.

    _camera_model->resetCameraLocation();
	
    // move camera from calibration location, orientation to the current
    // location/orientation
    _camera_model->moveCamera(_rac_calibration_location, _rac_calibration_orientation,
			      pos, orient, _pointing_cs);


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

void PigPointPHXracCamera::pointCamera(const char *obs_id, 
					    char *data_source)
{
    printFatal("PigPointPHXracCamera::pointCamera(obs_id, data_source) not implemented yet!!");
}

////////////////////////////////////////////////////////////////////////
// These functions get and set the pointing and camera position, in
// the given coordinates.  They are *not* intended for pointing
// correction, use the get/setPointingParameters() functions for that.
////////////////////////////////////////////////////////////////////////

    // !!!! check this routine for RAC applicability! 
void PigPointPHXracCamera::setCameraOrientation(const PigVector &orientation,
						PigCoordSystem *cs)
{
    if (!checkCameraModel())
	return;

    // The rotations must be done in the "natural" CS frame to mimic the az/el
    // ... NOT in Fixed coordinates, which would introduce a twist!
  
}

////////////////////////////////////////////////////////////////////////
// These functions allow access to the "raw" pointing data, in order
// to accomplish pointing corrections or "tweaks".
// For the RAC camera:
//    params[0] = S
//    params[1] = V1
//    params[2] = V2
//    params[3] = V3
//    params[4] = X position
//    params[5] = Y position
//    params[6] = Z position
//
// Subclasses could define extra parameters, e.g. 6dof where instead of
// quaternion, Euler angles are used to define orientation.
// Everything is measured in Payload.
////////////////////////////////////////////////////////////////////////
void PigPointPHXracCamera::getPointingParameters(double params[],
						 const int max_count)
{

    if (max_count >= 1)
	params[0] = _current_orientation.getS();
    if (max_count >= 2)
	params[1] = _current_orientation.getV().getX();
    if (max_count >= 3)
	params[2] = _current_orientation.getV().getY();
    if (max_count >= 4)
	params[3] = _current_orientation.getV().getZ();
    if (max_count >= 5)
	params[4] = _current_location.getX();
    if (max_count >= 6)
	params[5] = _current_location.getY();
    if (max_count >= 7)
	params[6] = _current_location.getZ();
}

void PigPointPHXracCamera::setPointingParameters(const double params[],
						 const int count)
{

   // Don't re-point if nothing has changed.
    if (count >= 7) {
        if (params[0] != _current_orientation.getS() ||
	    params[1] != _current_orientation.getV().getX() ||
	    params[2] != _current_orientation.getV().getY() ||
	    params[3] != _current_orientation.getV().getZ() ||
            params[4] != _current_location.getX() ||
            params[5] != _current_location.getY() ||
            params[6] != _current_location.getZ()) {
	    PigQuaternion quat(params[0], params[1], params[2], params[3]);
	    quat.normalize();
            PigPoint pos(params[4], params[5], params[6]); 
            pointCamera(pos, quat, _pointing_cs);
	}
    }
    else if (count >= 6) {
        if (params[0] != _current_orientation.getS() ||
	    params[1] != _current_orientation.getV().getX() ||
	    params[2] != _current_orientation.getV().getY() ||
	    params[3] != _current_orientation.getV().getZ() ||
            params[4] != _current_location.getX() ||
            params[5] != _current_location.getY()) {
	    PigQuaternion quat(params[0], params[1], params[2], params[3]);
	    quat.normalize();
            PigPoint pos(params[4], params[5], _current_location.getZ()); 
            pointCamera(pos, quat, _pointing_cs);
	}
    }
    else if (count >= 5) {
        if (params[0] != _current_orientation.getS() ||
	    params[1] != _current_orientation.getV().getX() ||
	    params[2] != _current_orientation.getV().getY() ||
	    params[3] != _current_orientation.getV().getZ() ||
            params[4] != _current_location.getZ()) {
	    PigQuaternion quat(params[0], params[1], params[2], params[3]);
	    quat.normalize();
            PigPoint pos(params[4], _current_location.getY(), _current_location.getZ()); 
            pointCamera(pos, quat, _pointing_cs);
	}
    }
    else if (count == 4) {
        if (params[0] != _current_orientation.getS() ||
	    params[1] != _current_orientation.getV().getX() ||
	    params[2] != _current_orientation.getV().getY() ||
            params[3] != _current_orientation.getV().getZ()) {
	    PigQuaternion quat(params[0], params[1], params[2], params[3]);
	    quat.normalize();
            PigPoint pos(_current_location.getX(), _current_location.getY(), _current_location.getZ()); 
            pointCamera(pos, quat, _pointing_cs);
	}
    }
    else if (count == 3) {
        if (params[0] != _current_orientation.getS() ||
	    params[1] != _current_orientation.getV().getX() ||
            params[2] != _current_orientation.getV().getY()) {
	    PigQuaternion quat(params[0], params[1], params[2], _current_orientation.getV().getZ());
	    quat.normalize();
            PigPoint pos(_current_location.getX(), _current_location.getY(), _current_location.getZ()); 
            pointCamera(pos, quat, _pointing_cs);
	}
    }
    else if (count == 2) {
        if (params[0] != _current_orientation.getS() ||
            params[1] != _current_orientation.getV().getX()) {
	    PigQuaternion quat(params[0], params[1], _current_orientation.getV().getY(), _current_orientation.getV().getZ());
	    quat.normalize();
            PigPoint pos(_current_location.getX(), _current_location.getY(), _current_location.getZ()); 
            pointCamera(pos, quat, _pointing_cs);
	}
    }
    else if (count == 1) {
        if (params[0] != _current_orientation.getS()) {
	    PigQuaternion quat(params[0], _current_orientation.getV().getX(), _current_orientation.getV().getY(), _current_orientation.getV().getZ());
	    quat.normalize();
            PigPoint pos(_current_location.getX(), _current_location.getY(), _current_location.getZ()); 
            pointCamera(pos, quat, _pointing_cs);
	}
    }
}

/////////////////////////////////////////////////////////////////////
// Pointing parameters are quaternion (S,V1, V2, V3) that represents
// orientation and position (X, Y, Z) as measured in Payload Frame
/////////////////////////////////////////////////////////////////////
const char *const PigPointPHXracCamera::getPointingParamName(int i)
{
    switch (i) {
        case 0:
            return "S";
        case 1:
            return "V1";
        case 2:
            return "V2";
        case 3:
            return "V3";
        case 4:
            return "X";
        case 5:
            return "Y";
        case 6:
            return "Z";	    
        default:
            return "Unknown";
    }
}
/////////////////////////////////////////////////////////////////////////
//
//////////////////////////////////////////////////////////////////////////
void PigPointPHXracCamera::forceCameraRepoint( )
{

    pointCamera(_current_location, _current_orientation, _pointing_cs);

}
/////////////////////////////////////////////////////////////////////////
//
//////////////////////////////////////////////////////////////////////////
void PigPointPHXracCamera::getPointingErrorEstimate(double errors[],
                                                     const int max_count)
{
    int i;
    int n = max_count;
    if (n > 7) n = 7;
    for (i=0; i < n; i++)
        errors[i] = _pointing_error[i];
}

////////////////////////////////////////////////////////////////////////
// Read in the calibration pointing parameters for a given "point" file.
////////////////////////////////////////////////////////////////////////

void PigPointPHXracCamera::read_point_info(char *filename, const char *host_id)
{
    FILE *inClientFile;
    char line[255];

    // open the file
    
    inClientFile = PigModelBase::openConfigFile(filename, NULL);

    // Default pointing values, in case the .point file isn't found
    // or the necessary values are not in the file.
    if (!strcasecmp(host_id, "FM")) {
      _rac_calibration_location.setXYZ(0.91959, 0.919661, 0.365448);
      double v[4] = {0.364886, -0.000960473, 0.000864866, -0.931051};
      _rac_calibration_orientation.setComponents(v);
      _pointing_error[0] = _pointing_error[1] = _pointing_error[2] =
      _pointing_error[3] = _pointing_error[4] = _pointing_error[5]  =
      _pointing_error[6] = 0.001;	
    }
    else if (!strcasecmp(host_id, "EM")) {
      _rac_calibration_location.setXYZ(0.91959, 0.919661, 0.365448);
      double v[4] = {0.364886, -0.000960473, 0.000864866, -0.931051};
      _rac_calibration_orientation.setComponents(v);
      _pointing_error[0] = _pointing_error[1] = _pointing_error[2] =
      _pointing_error[3] = _pointing_error[4] = _pointing_error[5]  =
      _pointing_error[6] = 0.001;
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
            // Pointing parameters

	    if (strncasecmp(line, "rac_calibration_position", 24) == 0) {
		sscanf(line, "rac_calibration_position = %lf %lf %lf",
		       &dum[0], &dum[1], &dum[2]);
		_rac_calibration_location.setXYZ(dum);
	    }
	    if (strncasecmp(line, "rac_calibration_orientation", 27) == 0) {
		sscanf(line, "rac_calibration_orientation = %lf %lf %lf %lf",
		       &dum[0], &dum[1], &dum[2], &dum[3]);
		_rac_calibration_orientation.setComponents(dum);
	    }
            if (strncasecmp(line, "rac_pointing_error", 18) == 0) {
                sscanf(line, "rac_pointing_error = %lf %lf %lf %lf %lf %lf %lf",
                       &_pointing_error[0], &_pointing_error[1],
                       &_pointing_error[2], &_pointing_error[3],
                       &_pointing_error[4], &_pointing_error[5],
		       &_pointing_error[6]);
            }

	}
	fclose(inClientFile);
    }
}
