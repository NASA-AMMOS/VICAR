////////////////////////////////////////////////////////////////////////
// PigPointBodyFixedCamera.h
//
// Pointing model for generic Body Fixed cameras, such as Mars Pathfinder
// Rover cameras.
//
// This class is intended merely as a convenience to hold functions common
// to all body fixed cameras.  Subclasses may override anything in here.
//
// Note that there is no pointing parameters.  So base class
// implementation is OK.
//
// Note that this contains pure virtual functions; as such, a subclass
// *must* be used for each mission type.
////////////////////////////////////////////////////////////////////////
#ifndef PIGPOINTBODYFIXEDCAMERA_H
#define PIGPOINTBODYFIXEDCAMERA_H

#include "PigPointingModel.h"

#include "PigVector.h"
#include "PigQuaternion.h"

class PigPointBodyFixedCamera : public PigPointingModel {

  public:

    // Subclasses *must* set _pointing_cs!
    PigPointBodyFixedCamera(PigCameraModel *cm, PigMission *mission,
			    const char *instrument);
    virtual ~PigPointBodyFixedCamera();

    // Point a camera model, given an image or an observation ID.  This
    // does not have to be the same image as was given in the constructor,
    // although presumably the mission/camera names should match!
    // data_source is included mainly to allow the overloading of these
    // two functions, but is intended to specify where to get the potining
    // data from, e.g. SPICE, database, etc.

    virtual void pointCamera(PigFileModel *file);
    virtual void pointCamera(const char *obs_id, char *data_source);

    // These functions are specific to body fixed cameras (not in base class)
    // Subclasses should override to set parameters, then call this version. 
    // If cs == NULL, _pointing_cs, the natural instrument frame, is used)
    virtual void pointCamera(const PigQuaternion *qt, PigCoordSystem *cs);
    virtual void pointCamera(PigCoordSystem *cs);
    virtual void forceCameraRepoint();

    // These functions set the camera position, in the given coordinates.
    // It is not intended for pointing correction.
    virtual void setCameraOrientation(const PigVector &orientation,
					    PigCoordSystem *cs);
    virtual const char *const getModelName() = 0;

};

#endif

