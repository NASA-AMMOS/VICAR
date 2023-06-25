////////////////////////////////////////////////////////////////////////
// PigPointMSLBodyFixedCamera
//
// Pointing model for Mars MSL Body Fixed Rover cameras.
//
// Note that there is no pointing parameters. So base class
// implementation is OK.

////////////////////////////////////////////////////////////////////////
#ifndef PIGPOINTMSLBODYFIXEDCAMERA_H
#define PIGPOINTMSLBODYFIXEDCAMERA_H

#include "PigPointBodyFixedCamera.h"


class PigPointMSLBodyFixedCamera : public PigPointBodyFixedCamera {

  public:

    PigPointMSLBodyFixedCamera(PigCameraModel *cm, 
				PigMission *mission, 
				const char *instrument) 
                     : PigPointBodyFixedCamera(cm, mission, instrument) {
        //nothing to do...
    }

    virtual ~PigPointMSLBodyFixedCamera() {
        // nothing to do...
    }

    virtual const char *const getModelName() { return "MslBodyFixedCamera"; }

};
#endif

