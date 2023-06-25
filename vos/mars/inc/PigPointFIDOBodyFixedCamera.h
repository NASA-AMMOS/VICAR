////////////////////////////////////////////////////////////////////////
// PigPointFIDOBodyFixedCamera
//
// Pointing model for Mars FIDO Body Fixed Rover cameras.
//
// Note that there is no pointing parameters. So base class
// implementation is OK.

////////////////////////////////////////////////////////////////////////
#ifndef PIGPOINTFIDOBODYFIXEDCAMERA_H
#define PIGPOINTFIDOBODYFIXEDCAMERA_H

#include "PigPointBodyFixedCamera.h"


class PigPointFIDOBodyFixedCamera : public PigPointBodyFixedCamera {

  public:

    PigPointFIDOBodyFixedCamera(PigCameraModel *cm, 
				PigMission *mission, 
				const char *instrument) 
                     : PigPointBodyFixedCamera(cm, mission, instrument) {
        //nothing to do...
    }

    virtual ~PigPointFIDOBodyFixedCamera() {
        // nothing to do...
    }

    // These are specific to MPF Rover.
    virtual const char *const getModelName() { return "FidoBodyFixedCamera"; }

};
#endif

