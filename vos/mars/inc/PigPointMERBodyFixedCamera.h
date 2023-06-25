////////////////////////////////////////////////////////////////////////
// PigPointMERBodyFixedCamera
//
// Pointing model for Mars MER Body Fixed Rover cameras.
//
// Note that there is no pointing parameters. So base class
// implementation is OK.

////////////////////////////////////////////////////////////////////////
#ifndef PIGPOINTMERBODYFIXEDCAMERA_H
#define PIGPOINTMERBODYFIXEDCAMERA_H

#include "PigPointBodyFixedCamera.h"


class PigPointMERBodyFixedCamera : public PigPointBodyFixedCamera {

  public:

    PigPointMERBodyFixedCamera(PigCameraModel *cm, 
				PigMission *mission, 
				const char *instrument) 
                     : PigPointBodyFixedCamera(cm, mission, instrument) {
        //nothing to do...
    }

    virtual ~PigPointMERBodyFixedCamera() {
        // nothing to do...
    }

    virtual const char *const getModelName() { return "MerBodyFixedCamera"; }

};
#endif

