////////////////////////////////////////////////////////////////////////
// PigPointPanTiltCamera
//
// Pointing model for generic pan/tilt cameras, such as Mars Pathfinder IMP
// and M98 SSI.
//
// This superclass supports cameras that are pointed in az/el (instrument
// coords) only.
//
// This class is intended merely as a convenience to hold functions common
// to all pan/tilt cameras.  Subclasses may override anything in here.
//
// Note that this contains pure virtual functions; as such, a subclass
// *must* be used for each mission type.
////////////////////////////////////////////////////////////////////////

#include "PigPointPanTiltCamera.h"

#include "PigCameraModel.h"
#include "PigCoordSystem.h"

#include <iostream>
using namespace std;

////////////////////////////////////////////////////////////////////////
// Constructor
////////////////////////////////////////////////////////////////////////

PigPointPanTiltCamera::PigPointPanTiltCamera(PigCameraModel *cm,
			PigMission *mission, const char *instrument)
	: PigPointingModel(cm, mission, instrument)
{
    _azimuth = _elevation = 0.0;
}

////////////////////////////////////////////////////////////////////////
// Destructor
////////////////////////////////////////////////////////////////////////

PigPointPanTiltCamera::~PigPointPanTiltCamera()
{
	// nothing to do...
}

////////////////////////////////////////////////////////////////////////
// Do the actual work of pointing the camera.
//
// The idea here is that the stored camera model numbers reflect where
// the camera was pointing during the calibration frame.  So, we un-rotate
// using the calibration pointing (az, then el), and then rotate using the
// desired pointing (el, then az).
//
// Az and el are in degrees.  They are mesured in the given coordinate
// system (if NULL, _pointing_cs, the natural instrument frame, is used)
////////////////////////////////////////////////////////////////////////

void PigPointPanTiltCamera::pointCamera(const double az,
										const double el, 
										PigCoordSystem *cs)
{

    static PigQuaternion identity;		// no rotation
    static PigPoint origin(0.0, 0.0, 0.0);
    static PigVector y_axis(0.0, 1.0, 0.0);
    static PigVector z_axis(0.0, 0.0, 1.0);

    double elevation = el;
    double azimuth = az;
    if (cs != NULL)				// convert
	  _pointing_cs->convertAzElDegrees(az, el, azimuth, elevation, cs);

// The elevation rotation is counter-intuitive.  For coordinate frames
// which define elevation as being towards -Z (i.e. usually +Z is down),
// we rotate by +elevation (or -cal_elevation).  However, for frames with
// elevation towards +Z (+Z is up), we rotate by -elevation (or +cal_elevation).
// This seems backwards w.r.t. the azimuth rotation, and I can't explain it
// intuitively (since Z up is the "normal" way of doing things), but that's
// the way the math works.

// Rotation around Z axis to align calibration frame with lander x-z plane
// This is the negative of the azimuth at which the cal frames were taken
// (to "unrotate" the frame).
    PigQuaternion z_cal(z_axis, PigDeg2Rad( - _calibration_azimuth));

// Rotation around Y axis to align calibration frame with lander x axis
// This is the positive (see above) of the elevation at which the cal frames
// were taken (to "unrotate" the frame).
    PigQuaternion y_cal(y_axis, PigDeg2Rad( _calibration_elevation) *
					_pointing_cs->getElevationDirection());

    if (_camera_model == NULL) {
	printFatal("Can't point a NULL camera in PigPointPanTiltCamera!");
	return;
    }

    _azimuth = azimuth;
    _elevation = elevation;

    // Generate quats for azimuth and elevation pointing.  See above for
    // comment on elevation direction

    PigQuaternion q_el(y_axis, PigDeg2Rad(- elevation) *
					_pointing_cs->getElevationDirection());
    PigQuaternion q_az(z_axis, PigDeg2Rad(azimuth));

    // Compute the combined rotation.  First z cal(az), then y cal(el), then
    // elevation(y-axis), finally azimuth(z-axis).  q_az and q_el could be
    // pre-multiplied, but this is usually done only once per frame, so
    // computation time is not an issue.

    PigQuaternion q = q_az * q_el * y_cal * z_cal;

    // Rotate the camera head from calibration to desired location.
    // We could equivalently provide cal az/el as the initial rotation
    // and not include it in the q rotation.

    _camera_model->resetCameraLocation();

    _camera_model->moveCamera(_calibration_rotation_point, identity,
			      _rotation_point, q, _pointing_cs);
}

////////////////////////////////////////////////////////////////////////
// These functions get and set the pointing and camera position, in
// the given coordinates.  They are *not* intended for pointing
// correction, use the get/setPointingParameters() functions for that.
////////////////////////////////////////////////////////////////////////

void PigPointPanTiltCamera::setCameraOrientation(const PigVector &orientation,
						 PigCoordSystem *cs)
{
    if (!checkCameraModel())
	return;

    // The rotations must be done in the "natural" CS frame to mimic the az/el
    // motors... NOT in Fixed coordinates, which would introduce a twist!
    // In order to get the parallax right (and the stereo eye separation),
    // we simply convert the orientation to az/el in Instrument coordinates
    // and re-point the camera.

    PigVector inst_pointing = _pointing_cs->convertVector(orientation, cs);
    pointCamera(PigRad2Deg(_pointing_cs->getAz(inst_pointing)),
				PigRad2Deg(_pointing_cs->getEl(inst_pointing)),
				_pointing_cs);
}

////////////////////////////////////////////////////////////////////////
// These functions allow access to the "raw" pointing data, in order
// to accomplish pointing corrections or "tweaks".
// For Pan/Tilt Cameras:
//    params[0] = Azimuth
//    params[1] = Elevation
// Angles are in degrees!
// Note that twist is not allowed.
// Subclasses could define extra parameters, e.g. for a rover where the
// position is also unknown.
// All values are measured in the instrument "natural" coordinate system
// (_pointing_cs).
////////////////////////////////////////////////////////////////////////

void PigPointPanTiltCamera::getPointingParameters(double params[],
						  const int max_count)
{
    if (max_count >= 1)
	params[0] = _azimuth;
    if (max_count >= 2)
	params[1] = _elevation;
}

void PigPointPanTiltCamera::setPointingParameters(const double params[],
						const int count)
{
    // Don't re-point if nothing has changed.

    if (count >= 2) {
	if (params[0] != _azimuth || params[1] != _elevation)
	    pointCamera(params[0], params[1], _pointing_cs);
    }
    else if (count >= 1) {
	if (params[0] != _azimuth)
	    pointCamera(params[0], _elevation, _pointing_cs);
    }
}
void PigPointPanTiltCamera::forceCameraRepoint( )
{

  pointCamera(_azimuth, _elevation, _pointing_cs);

}
const char *const PigPointPanTiltCamera::getPointingParamName(int i)
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
// Subclasses must set _pointing_error array in their constructor!
// (or override this function)
////////////////////////////////////////////////////////////////////////

void PigPointPanTiltCamera::getPointingErrorEstimate(double errors[],
				const int max_count)
{
    int i;
    int n = max_count;
    if (n > 2) n = 2;		// only 2 params for pan/tilt

    for (i=0; i < n; i++)
	errors[i] = _pointing_error[i];
}

