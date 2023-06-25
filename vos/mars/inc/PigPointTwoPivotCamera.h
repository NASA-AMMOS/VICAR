////////////////////////////////////////////////////////////////////////
// PigPointTwoPivotCamera.h
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
#ifndef PIGPOINTTWOPIVOTCAMERA_H
#define PIGPOINTTWOPIVOTCAMERA_H

#include "PigPointPanTiltCamera.h"

#include "PigVector.h"
#include "PigQuaternion.h"

class PigPointTwoPivotCamera : public PigPointPanTiltCamera {

  protected:

    // These parameters must be filled in by the subclass before calling
    // pointCamera - either in constructor, or in override of pC().
    // Note that the superclass _rotation_point (and cal_r_p) are reused
    // here for the azimuth direction.

    // PigPoint _rotation_point;		// superclass, azimuth here
    // PigPoint _calibration_rotation_point;	// superclass, azimuth here
    PigPoint _rotation_point_el;
    PigPoint _calibration_rotation_point_el;

  public:

    // Subclasses *must* set _pointing_cs!
    PigPointTwoPivotCamera(PigCameraModel *cm, PigMission *mission,
				       const char *instrument);
    virtual ~PigPointTwoPivotCamera();

    // Point a camera model, given an image or an observation ID.  This
    // does not have to be the same image as was given in the constructor,
    // although presumably the mission/camera names should match!
    // data_source is included mainly to allow the overloading of these
    // two functions, but is intended to specify where to get the potining
    // data from, e.g. SPICE, database, etc.

    virtual void pointCamera(PigFileModel *file) = 0;
    virtual void pointCamera(const char *obs_id, char *data_source) = 0;

    // These functions are specific to pan/tilt cameras (not in base class)
    // Subclasses should override to set parameters, then call this version.
    // Az and el are in degrees.  They are measured in the given coordinate
    // system (if NULL, _pointing_cs, the natural instrument frame, is used)

    virtual void pointCamera(const double azimuth, const double elevation,
							PigCoordSystem *cs);

    virtual const char *const getModelName() { return "TwoPivot"; }

};

#endif

