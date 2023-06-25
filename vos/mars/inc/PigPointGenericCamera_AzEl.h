////////////////////////////////////////////////////////////////////////
// PigPointGenericCamera_AzEl
//
// Pointing model for a generic camera with 2 degrees of freedom:  Azimuth
// and Elevation.
//
// The "calibration" pointing, as well as initial pointing, is derived from
// the initial camera model.
////////////////////////////////////////////////////////////////////////
#ifndef PIGPOINTGENERICCAMERA_AZEL_H
#define PIGPOINTGENERICCAMERA_AZEL_H

#include "PigPointPanTiltCamera.h"

#include "PigVector.h"
#include "PigQuaternion.h"
#include "PigCoordSystem.h"

class PigPointGenericCamera_AzEl : public PigPointPanTiltCamera {

  protected:

  public:

    PigPointGenericCamera_AzEl(PigCameraModel *cm, PigMission *mission,
				       const char *instrument);
    virtual ~PigPointGenericCamera_AzEl();

    // Point a camera model, given an image or an observation ID.  This
    // does not have to be the same image as was given in the constructor,
    // although presumably the mission/camera names should match!
    // data_source is included mainly to allow the overloading of these
    // two functions, but is intended to specify where to get the potining
    // data from, e.g. SPICE, database, etc.  Just pass NULL for this subclass.

    virtual void pointCamera(PigFileModel *file);
    virtual void pointCamera(const char *obs_id, char *data_source);

    // This is repeated here only to make C++ compiler happy.
    virtual void pointCamera(const double azimuth, const double elevation,
							PigCoordSystem *cs)
	{ PigPointPanTiltCamera::pointCamera(azimuth, elevation, cs); }

    virtual const char *const getModelName() { return "GenericCamera_AzEl"; }

};

#endif
