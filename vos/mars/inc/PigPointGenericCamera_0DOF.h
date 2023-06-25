////////////////////////////////////////////////////////////////////////
// PigPointGenericCamera_0DOF
//
// Pointing model for completely generic camera with 0 degrees of freedom
// (no pointing parameters).
//
// Note that there are no pointing parameters. So base class
// implementation is OK.
////////////////////////////////////////////////////////////////////////
#ifndef PIGPOINTGENERICCAMERA_0DOF_H
#define PIGPOINTGENERICCAMERA_0DOF_H

#include "PigPointBodyFixedCamera.h"

class PigPointGenericCamera_0DOF : public PigPointBodyFixedCamera {

  public:

    PigPointGenericCamera_0DOF(PigCameraModel *cm, 
                     PigMission *mission, 
                     const char *instrument) 
                     : PigPointBodyFixedCamera(cm, mission, instrument) {
        //nothing to do...
    }

    virtual ~PigPointGenericCamera_0DOF() {
        // nothing to do...
    }

    // These are specific to Generic camera
    virtual const char *const getModelName() { return "GenericCamera_0DOF"; }

};
#endif

