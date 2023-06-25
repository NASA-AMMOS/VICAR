////////////////////////////////////////////////////////////////////////
// PigPointColdarmBodyFixedCamera
//
// Pointing model for Coldarm Body Fixed cameras.
//
// Note that there is no pointing parameters. So base class
// implementation is OK.

////////////////////////////////////////////////////////////////////////
#ifndef PIGPOINTCOLDARMBODYFIXEDCAMERA_H
#define PIGPOINTCOLDARMBODYFIXEDCAMERA_H

#include "PigPointBodyFixedCamera.h"


class PigPointColdarmBodyFixedCamera : public PigPointBodyFixedCamera {

  public:

    PigPointColdarmBodyFixedCamera(PigCameraModel *cm, 
				PigMission *mission, 
				const char *instrument) 
                     : PigPointBodyFixedCamera(cm, mission, instrument) {
        //nothing to do...
    }

    virtual ~PigPointColdarmBodyFixedCamera() {
        // nothing to do...
    }

    virtual const char *const getModelName() { return "ColdarmBodyFixedCamera"; }

};
#endif

