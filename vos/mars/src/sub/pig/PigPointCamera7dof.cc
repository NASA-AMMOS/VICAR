////////////////////////////////////////////////////////////////////////
// PigPointCamera7dof
//
// Pointing model for cameras using 7dof.
//
// Pointing is done in the default frame. 
////////////////////////////////////////////////////////////////////////

#include "PigPointCamera7dof.h"
#include "PigMission.h"
#include "PigCameraModel.h"

////////////////////////////////////////////////////////////////////////
// Constructor
////////////////////////////////////////////////////////////////////////

PigPointCamera7dof::PigPointCamera7dof(PigCameraModel *cm,
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
}

////////////////////////////////////////////////////////////////////////
// Destructor
////////////////////////////////////////////////////////////////////////

PigPointCamera7dof::~PigPointCamera7dof()
{
	// nothing to do...
}
void pointCamera(PigFileModel *file)
{

}
void pointCamera(const char *obs_id, char *data_source)
{

}

///////////////////////////////////////////////////////////////////////
// Point the camera using position of the camera and orientation expressed
// as a unit quaternion.
//
///////////////////////////////////////////////////////////////////////
void PigPointCamera7dof::pointCamera(PigPoint &position,
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
    _camera_model->moveCamera(_calibration_location, _calibration_orientation,
			      pos, orient, _pointing_cs);


}

///////////////////////////////////////////////////////////////////////
// Point the camera in a specific direction.  Just builds a quat and
// then calls pointCamera().  Quat building is virtually identical to
// what PigPointCamera6dof does.
///////////////////////////////////////////////////////////////////////

void PigPointCamera7dof::setCameraOrientation(const PigVector &orientation,
                                                        PigCoordSystem *cs)
{

    if (!checkCameraModel())
	return;

    // Convert to instrument coordinates

    PigVector inst_pointing = _pointing_cs->convertVector(orientation, cs);

    // Convert angles to quaternion.  We do this in two stages to avoid
    // introducing a weird twist.

    static PigVector y_axis(0.0, 1.0, 0.0);
    static PigVector z_axis(0.0, 0.0, 1.0);

    PigQuaternion q_el(y_axis, _pointing_cs->getEl(inst_pointing));
    PigQuaternion q_az(z_axis, _pointing_cs->getAz(inst_pointing));
    PigQuaternion q = q_az * q_el;

    pointCamera(_current_location, q, _pointing_cs);
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

void PigPointCamera7dof::pointCamera(const char *obs_id, 
					    char *data_source)
{
    printFatal("PigPointCamera7dof::pointCamera(obs_id, data_source) not implemented yet!!");
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
void PigPointCamera7dof::getPointingParameters(double params[],
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

void PigPointCamera7dof::setPointingParameters(const double params[],
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
// orientation and position (X, Y, Z) as measured in default Frame
/////////////////////////////////////////////////////////////////////
const char *const PigPointCamera7dof::getPointingParamName(int i)
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
void PigPointCamera7dof::forceCameraRepoint( )
{

    pointCamera(_current_location, _current_orientation, _pointing_cs);

}
/////////////////////////////////////////////////////////////////////////
//
//////////////////////////////////////////////////////////////////////////
void PigPointCamera7dof::getPointingErrorEstimate(double errors[],
                                                  const int max_count)
{
    int i;
    int n = max_count;
    if (n > 7) n = 7;
    for (i=0; i < n; i++)
        errors[i] = _pointing_error[i];
}
