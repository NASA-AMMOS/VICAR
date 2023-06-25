////////////////////////////////////////////////////////////////////////
// PigPointM20mastCameraScale
//
// Pointing model for M20 MastcamZ, SuperCam, and NavCam cameras located
// on the Mast Assembly.
// Uses superclass to point camera using az, el, and twist.  Then scales
// camera by the specified factor to zoom in or out slightly.
// A very high inertia is recommended in marsnav to avoid excessive scaling.
//
////////////////////////////////////////////////////////////////////////

#include "PigPointM20mastCameraScale.h"
#include "PigCameraModel.h"

////////////////////////////////////////////////////////////////////////
// Constructor
////////////////////////////////////////////////////////////////////////

PigPointM20mastCameraScale::PigPointM20mastCameraScale(PigCameraModel *cm,
						   PigMission *mission, 
						   const char *instrument)
                        : PigPointM20mastCamera3dof(cm, mission, instrument)
{
    _scale = 1.0;
}

////////////////////////////////////////////////////////////////////////
// Destructor
////////////////////////////////////////////////////////////////////////

PigPointM20mastCameraScale::~PigPointM20mastCameraScale()
{
	// nothing to do...
}

////////////////////////////////////////////////////////////////////////
// Do the actual work of pointing the camera.
//
// Az, el, twist are in degrees.  They are mesured in the given coordinate
// system (if NULL, _pointing_cs, the natural instrument frame, is used).
// Scale is a unitless factor; >1.0 expands the image while <1.0 shrinks it.
////////////////////////////////////////////////////////////////////////

void PigPointM20mastCameraScale::pointCamera(const double az,
					   const double el,
					   const double twist,
					   const double scale)
{
    // First point camera using three degrees of freedom
    pointCamera(az, el, twist);

    _scale = scale;

    // Now scale the camera.  pointCamera() above resets to initial
    // conditions so we can simply apply the scale; we don't have to
    // worry about scaling an already scaled model.

    _camera_model->scaleCamera(_scale, _scale);

}

void PigPointM20mastCameraScale::getPointingParameters(double params[],
						 const int max_count)
{
    if (max_count >= 1)
	params[0] = _azimuth;
    if (max_count >= 2)
	params[1] = _elevation;
    if (max_count >= 3)
	params[2] = _twist;
    if (max_count >= 4)
	params[3] = _scale;
}

void PigPointM20mastCameraScale::setPointingParameters(const double params[],
						     const int count)
{
    // Don't re-point if nothing has changed.
    if (count >= 4) {
        if (params[0] != _azimuth || 
	    params[1] != _elevation || 
	    params[2] != _twist ||
	    params[3] != _scale)
            pointCamera(params[0], params[1], params[2], params[3]);
    }
    if (count == 3) {
        if (params[0] != _azimuth || 
	    params[1] != _elevation || 
	    params[2] != _twist)
            pointCamera(params[0], params[1], params[2], _scale);
    }
    if (count == 2) {
        if (params[0] != _azimuth || params[1] != _elevation)
            pointCamera(params[0], params[1], _twist, _scale);
    }
    else if (count == 1) {
        if (params[0] != _azimuth)
            pointCamera(params[0], _elevation, _twist, _scale);
    }
}

const char *const PigPointM20mastCameraScale::getPointingParamName(int i)
{
    switch (i) {
        case 0:
            return "Joint Azimuth";
        case 1:
            return "Joint Elevation";
        case 2:
            return "Twist";
        case 3:
            return "Scale";
        default:
            return "Unknown";
    }
}

void PigPointM20mastCameraScale::forceCameraRepoint( )
{

    pointCamera(_azimuth, _elevation, _twist);

}

////////////////////////////////////////////////////////////////////////
// Subclasses must set _pointing_error array in their constructor!
// (or override this function)
////////////////////////////////////////////////////////////////////////

void PigPointM20mastCameraScale::getPointingErrorEstimate(double errors[],
							const int max_count)
{
    PigPointM20mastCamera::getPointingErrorEstimate(errors, 2);
    if (max_count >= 3)
        errors[2] = _pointing_error[2];
    if (max_count >= 4)
	errors[3] = _pointing_error[3];
}

