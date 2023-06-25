////////////////////////////////////////////////////////////////////////
// PigPointMSLmastCamera3dof
//
// Pointing model for MSL Mastcam, Chemcam, and NavCam cameras located
// on the Mast Assembly.
// Uses superclass to point camera using az and el.  Then twists camera
// model by specified amount around A-vector axis.
//
//
////////////////////////////////////////////////////////////////////////

#include "PigPointMSLmastCamera3dof.h"
#include "PigCameraModel.h"

////////////////////////////////////////////////////////////////////////
// Constructor
////////////////////////////////////////////////////////////////////////

PigPointMSLmastCamera3dof::PigPointMSLmastCamera3dof(PigCameraModel *cm,
						   PigMission *mission, 
						   const char *instrument)
                        : PigPointMSLmastCamera(cm, mission, instrument)
{
    _twist = 0.0;
}

////////////////////////////////////////////////////////////////////////
// Destructor
////////////////////////////////////////////////////////////////////////

PigPointMSLmastCamera3dof::~PigPointMSLmastCamera3dof()
{
	// nothing to do...
}

////////////////////////////////////////////////////////////////////////
// Do the actual work of pointing the camera.
//
// Az, el, twist are in degrees.  They are mesured in the given coordinate
// system (if NULL, _pointing_cs, the natural instrument frame, is used)
////////////////////////////////////////////////////////////////////////

void PigPointMSLmastCamera3dof::pointCamera(const double az,
					   const double el,
					   const double twist)
{
    // First point camera using two degrees of freedom and
    // Pan/Tilt camera model.
    pointCamera(az, el);

    _twist = twist;

    // Now twist the camera by rotating around A-axis.
    PigPoint initial_point = getCameraModel()->getCameraPosition();
    PigVector orientation = getCameraModel()->getCameraOrientation();
    PigQuaternion initial_orientation(orientation, 0.0);
    PigQuaternion final_orientation(orientation, PigDeg2Rad(_twist));

    _camera_model->moveCamera(initial_point, initial_orientation,
			      initial_point, final_orientation, 
			      _pointing_cs);
}

void PigPointMSLmastCamera3dof::getPointingParameters(double params[],
						 const int max_count)
{
    if (max_count >= 1)
	params[0] = _azimuth;
    if (max_count >= 2)
	params[1] = _elevation;
    if (max_count >= 3)
	params[2] = _twist;
}

void PigPointMSLmastCamera3dof::setPointingParameters(const double params[],
						     const int count)
{
    // Don't re-point if nothing has changed.
    if (count >= 3) {
        if (params[0] != _azimuth || 
	    params[1] != _elevation || 
	    params[2] != _twist)
            pointCamera(params[0], params[1], params[2]);
    }
    if (count == 2) {
        if (params[0] != _azimuth || params[1] != _elevation)
            pointCamera(params[0], params[1], _twist);
    }
    else if (count == 1) {
        if (params[0] != _azimuth)
            pointCamera(params[0], _elevation, _twist);
    }
}

const char *const PigPointMSLmastCamera3dof::getPointingParamName(int i)
{
    switch (i) {
        case 0:
            return "Joint Azimuth";
        case 1:
            return "Joint Elevation";
        case 2:
            return "Twist";
        default:
            return "Unknown";
    }
}

void PigPointMSLmastCamera3dof::forceCameraRepoint( )
{

    pointCamera(_azimuth, _elevation, _twist);

}

////////////////////////////////////////////////////////////////////////
// Subclasses must set _pointing_error array in their constructor!
// (or override this function)
////////////////////////////////////////////////////////////////////////

void PigPointMSLmastCamera3dof::getPointingErrorEstimate(double errors[],
							const int max_count)
{
    PigPointMSLmastCamera::getPointingErrorEstimate(errors, 2);
    if (max_count >= 3)
        errors[2] = _pointing_error[2];
}

