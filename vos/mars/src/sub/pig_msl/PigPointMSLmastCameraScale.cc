////////////////////////////////////////////////////////////////////////
// PigPointMSLmastCameraScale
//
// Pointing model for MSL Mastcam, Chemcam, and NavCam cameras located
// on the Mast Assembly.
// Uses superclass to point camera using az, el, and twist.  Then scales
// camera by the specified factor to zoom in or out slightly.
// A very high inertia is recommended in marsnav to avoid excessive scaling.
//
////////////////////////////////////////////////////////////////////////

#include "PigPointMSLmastCameraScale.h"
#include "PigCameraModel.h"

////////////////////////////////////////////////////////////////////////
// Constructor
////////////////////////////////////////////////////////////////////////

PigPointMSLmastCameraScale::PigPointMSLmastCameraScale(PigCameraModel *cm,
						   PigMission *mission, 
						   const char *instrument)
                        : PigPointMSLmastCamera3dof(cm, mission, instrument)
{
    _scale = 1.0;
}

////////////////////////////////////////////////////////////////////////
// Destructor
////////////////////////////////////////////////////////////////////////

PigPointMSLmastCameraScale::~PigPointMSLmastCameraScale()
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

void PigPointMSLmastCameraScale::pointCamera(const double az,
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

void PigPointMSLmastCameraScale::getPointingParameters(double params[],
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

void PigPointMSLmastCameraScale::setPointingParameters(const double params[],
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

const char *const PigPointMSLmastCameraScale::getPointingParamName(int i)
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

void PigPointMSLmastCameraScale::forceCameraRepoint( )
{

    pointCamera(_azimuth, _elevation, _twist);

}

////////////////////////////////////////////////////////////////////////
// Subclasses must set _pointing_error array in their constructor!
// (or override this function)
////////////////////////////////////////////////////////////////////////

void PigPointMSLmastCameraScale::getPointingErrorEstimate(double errors[],
							const int max_count)
{
    PigPointMSLmastCamera::getPointingErrorEstimate(errors, 2);
    if (max_count >= 3)
        errors[2] = _pointing_error[2];
    if (max_count >= 4)
	errors[3] = _pointing_error[3];
}

