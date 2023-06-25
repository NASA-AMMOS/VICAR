////////////////////////////////////////////////////////////////////////
// PigPointPanTiltCamera.h
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
#ifndef PIGPOINTPANTILTCAMERA_H
#define PIGPOINTPANTILTCAMERA_H

#include "PigPointingModel.h"

#include "PigVector.h"
#include "PigQuaternion.h"

class PigPointPanTiltCamera : public PigPointingModel {

  protected:

    double _azimuth;
    double _elevation;

    // These parameters must be filled in by the subclass before calling
    // pointCamera - either in constructor, or in override of pC().

    PigPoint _rotation_point;
    PigPoint _calibration_rotation_point;
    double _calibration_azimuth;		// degrees
    double _calibration_elevation;

    // This array must also be filled in by the subclass constructor
    // (or getPointingErrorEstimate() must be overriden)

    double _pointing_error[3];		// az, el, twist expected pointing error
                                        // twist is not allowed by this class
                                        // so it's a responsibility of subclasses
                                        // to implement it properly.

  public:

    // Subclasses *must* set _pointing_cs!
    PigPointPanTiltCamera(PigCameraModel *cm, PigMission *mission,
				       const char *instrument);
    virtual ~PigPointPanTiltCamera();

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

    // These functions get and set the pointing and camera position, in
    // the given coordinates.  They are *not* intended for pointing
    // correction, use the get/setPointingParameters() functions for that.

    virtual void setCameraOrientation(const PigVector &orientation,
									  PigCoordSystem *cs);

    // These functions allow access to the "raw" pointing data, in order
    // to accomplish pointing corrections or "tweaks".
    // For Pan/Tilt cameras:
    //    params[0] = Azimuth
    //    params[1] = Elevation
    // Angles are in degrees!
    // Note that twist is not allowed.

    virtual int getPointingParamCount() { return 2; }
    virtual void getPointingParameters(double params[], const int max_count);
    virtual void setPointingParameters(const double params[], const int count);
    virtual void forceCameraRepoint();
    virtual void getPointingErrorEstimate(double params[], const int max_count);
    virtual const char *const getPointingParamName(int i);      // 0-based

    virtual const char *const getModelName() { return "PanTilt"; }

};

#endif

