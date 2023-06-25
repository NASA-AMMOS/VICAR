////////////////////////////////////////////////////////////////////////
// PigPointPsycheBodyFixedCamera
//
// Pointing model for Psyche Body Fixed cameras.
//
// Note that there is no pointing parameters. So base class
// implementation is OK.

////////////////////////////////////////////////////////////////////////
#ifndef PIGPOINTPSYCHEBODYFIXEDCAMERA_H
#define PIGPOINTPSYCHEBODYFIXEDCAMERA_H

#include "PigPointBodyFixedCamera.h"


class PigPointPsycheBodyFixedCamera : public PigPointBodyFixedCamera {

  public:

    PigPointPsycheBodyFixedCamera(PigCameraModel *cm, 
				PigMission *mission, 
				const char *instrument) 
                     : PigPointBodyFixedCamera(cm, mission, instrument) {
        //nothing to do...
    }

    virtual ~PigPointPsycheBodyFixedCamera() {
        // nothing to do...
    }

    virtual const char *const getModelName() { return "PsycheBodyFixedCamera"; }

};
#endif

