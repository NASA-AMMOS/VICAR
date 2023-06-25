////////////////////////////////////////////////////////////////////////
// PigPointMERpmaCamera3dof
//
// Pointing model for MER Pancam & NavCam cameras located on Mast Assembly.
// Uses superclass to point camera using az and el.  Then twists camera
// model by specified amount around A-vector axis.
//
//
////////////////////////////////////////////////////////////////////////

#include "PigPointMERpmaCamera3dof.h"
#include "PigCameraModel.h"

////////////////////////////////////////////////////////////////////////
// Constructor
////////////////////////////////////////////////////////////////////////

PigPointMERpmaCamera3dof::PigPointMERpmaCamera3dof(PigCameraModel *cm,
						   PigMission *mission, 
						   const char *instrument)
                        : PigPointMERpmaCamera(cm, mission, instrument)
{
    _twist = 0.0;
}

////////////////////////////////////////////////////////////////////////
// Destructor
////////////////////////////////////////////////////////////////////////

PigPointMERpmaCamera3dof::~PigPointMERpmaCamera3dof()
{
	// nothing to do...
}

////////////////////////////////////////////////////////////////////////
// Do the actual work of pointing the camera.
//
// Az, el, twist are in degrees.  They are mesured in the given coordinate
// system (if NULL, _pointing_cs, the natural instrument frame, is used)
////////////////////////////////////////////////////////////////////////

void PigPointMERpmaCamera3dof::pointCamera(const double az,
					   const double el,
					   const double twist,
					   PigCoordSystem *cs )
{
    // First point camera using two degrees of freedom and
    // Pan/Tilt camera model.
    pointCamera(az, el, cs);

    _twist = twist;

    // Now twist the camera by rotating around A-axis.
    PigPoint initial_point = getCameraModel()->getCameraPosition();
    PigVector orientation = getCameraModel()->getCameraOrientation();
    PigQuaternion initial_orientation(orientation, 0.0);
    PigQuaternion final_orientation(orientation, PigDeg2Rad(_twist));

    _camera_model->moveCamera(_rotation_point, initial_orientation,
			      _rotation_point, final_orientation, 
			      _pointing_cs);
}

void PigPointMERpmaCamera3dof::getPointingParameters(double params[],
						 const int max_count)
{
    if (max_count >= 1)
	params[0] = _azimuth;
    if (max_count >= 2)
	params[1] = _elevation;
    if (max_count >= 3)
	params[2] = _twist;
}

void PigPointMERpmaCamera3dof::setPointingParameters(const double params[],
						     const int count)
{
    // Don't re-point if nothing has changed.
    if (count >= 3) {
        if (params[0] != _azimuth || 
	    params[1] != _elevation || 
	    params[2] != _twist)
            pointCamera(params[0], params[1], params[2], _pointing_cs);
    }
    if (count == 2) {
        if (params[0] != _azimuth || params[1] != _elevation)
            pointCamera(params[0], params[1], _twist, _pointing_cs);
    }
    else if (count == 1) {
        if (params[0] != _azimuth)
            pointCamera(params[0], _elevation, _twist, _pointing_cs);
    }
}

const char *const PigPointMERpmaCamera3dof::getPointingParamName(int i)
{
    switch (i) {
        case 0:
            return "Azimuth";
        case 1:
            return "Elevation";
        case 2:
            return "Twist";
        default:
            return "Unknown";
    }
}

void PigPointMERpmaCamera3dof::forceCameraRepoint( )
{

    pointCamera(_azimuth, _elevation, _twist, _pointing_cs);

}

////////////////////////////////////////////////////////////////////////
// Subclasses must set _pointing_error array in their constructor!
// (or override this function).  _pointing_error array has been set
// in the superclass.
////////////////////////////////////////////////////////////////////////

void PigPointMERpmaCamera3dof::getPointingErrorEstimate(double errors[],
							const int max_count)
{
    PigPointPanTiltCamera::getPointingErrorEstimate(errors, 2);
    errors[2] = _pointing_error[2];
}
