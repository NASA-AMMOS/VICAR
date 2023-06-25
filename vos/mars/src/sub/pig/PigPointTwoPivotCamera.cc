////////////////////////////////////////////////////////////////////////
// PigPointTwoPivotCamera
//
// Pointing model for generic pan/tilt cameras with two separate pivot
// points (az and el pivots are different), such as Mars '01 testbed and
// MER pancam.
//
// This superclass supports cameras that are pointed in az/el (instrument
// coords) only.
//
// This class is intended merely as a convenience to hold functions common
// to all two-pivot pan/tilt cameras.  Subclasses may override anything
// in here.
//
// Note that this contains pure virtual functions; as such, a subclass
// *must* be used for each mission type.
////////////////////////////////////////////////////////////////////////

#include "PigPointTwoPivotCamera.h"

#include "PigCameraModel.h"
#include "PigCoordSystem.h"

#include <iostream>
using namespace std;

////////////////////////////////////////////////////////////////////////
// Constructor
////////////////////////////////////////////////////////////////////////

PigPointTwoPivotCamera::PigPointTwoPivotCamera(PigCameraModel *cm,
			PigMission *mission, const char *instrument)
	: PigPointPanTiltCamera(cm, mission, instrument)
{
	// nothing to do...
}

////////////////////////////////////////////////////////////////////////
// Destructor
////////////////////////////////////////////////////////////////////////

PigPointTwoPivotCamera::~PigPointTwoPivotCamera()
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
//
// Unlike PanTiltCamera, we cannot compose all of the rotations because
// the pivot points are different.  But the method is the same.
////////////////////////////////////////////////////////////////////////

void PigPointTwoPivotCamera::pointCamera(const double az,
				const double el, PigCoordSystem *cs)
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

    // Compute the combined elevation rotation.  The order is first z cal(az),
    // then y cal(el), then elevation(y-axis), finally azimuth(z-axis).
    // Since y cal and elevation use the same pivot point, they can be combined
    // into one rotation.

    PigQuaternion q = q_el * y_cal;

    // Rotate the camera head from calibration to desired location.
    // We could equivalently provide cal az/el as the initial rotation
    // and not include it in the q rotation.

    _camera_model->resetCameraLocation();

    // Unrotate calibration azimuth

    _camera_model->moveCamera(_calibration_rotation_point, identity,
			      _rotation_point, z_cal, _pointing_cs);

    // Unrotate calibration elevation and rotate elevation

    _camera_model->moveCamera(_calibration_rotation_point_el, identity,
			      _rotation_point_el, q, _pointing_cs);

    // Rotate azimuth

    _camera_model->moveCamera(_calibration_rotation_point, identity,
			      _rotation_point, q_az, _pointing_cs);


}

