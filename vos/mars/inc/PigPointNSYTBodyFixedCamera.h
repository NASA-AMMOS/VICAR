////////////////////////////////////////////////////////////////////////
// PigPointNSYTBodyFixedCamera
//
// Pointing model for Mars NSYT Body Fixed Rover cameras.
//
// Note that there is no pointing parameters. So base class
// implementation is OK.

////////////////////////////////////////////////////////////////////////
#ifndef PIGPOINTNSYTBODYFIXEDCAMERA_H
#define PIGPOINTNSYTBODYFIXEDCAMERA_H

#include "PigPointBodyFixedCamera.h"


class PigPointNSYTBodyFixedCamera : public PigPointBodyFixedCamera {

  public:

    PigPointNSYTBodyFixedCamera(PigCameraModel *cm, 
				PigMission *mission, 
				const char *instrument) 
                     : PigPointBodyFixedCamera(cm, mission, instrument) {
        //nothing to do...
    }

    virtual ~PigPointNSYTBodyFixedCamera() {
        // nothing to do...
    }

    virtual const char *const getModelName() { return "NsytBodyFixedCamera"; }

};
#endif

