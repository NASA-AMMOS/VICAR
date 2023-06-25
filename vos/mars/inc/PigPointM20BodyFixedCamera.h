////////////////////////////////////////////////////////////////////////
// PigPointM20BodyFixedCamera
//
// Pointing model for Mars M20 Body Fixed Rover cameras.
//
// Note that there is no pointing parameters. So base class
// implementation is OK.

////////////////////////////////////////////////////////////////////////
#ifndef PIGPOINTM20BODYFIXEDCAMERA_H
#define PIGPOINTM20BODYFIXEDCAMERA_H

#include "PigPointBodyFixedCamera.h"


class PigPointM20BodyFixedCamera : public PigPointBodyFixedCamera {

  public:

    PigPointM20BodyFixedCamera(PigCameraModel *cm, 
				PigMission *mission, 
				const char *instrument) 
                     : PigPointBodyFixedCamera(cm, mission, instrument) {
        //nothing to do...
    }

    virtual ~PigPointM20BodyFixedCamera() {
        // nothing to do...
    }

    virtual const char *const getModelName() { return "M20BodyFixedCamera"; }

};
#endif

