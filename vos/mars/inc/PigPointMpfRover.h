////////////////////////////////////////////////////////////////////////
// PigPointMpfRover
//
// Pointing model for Mars Pathfinder Rover camera.
//
// Note that there is no pointing parameters. So base class
// implementation is OK.

////////////////////////////////////////////////////////////////////////
#ifndef PIGPOINTMPFROVER_H
#define PIGPOINTMPFROVER_H

#include "PigPointBodyFixedCamera.h"


class PigPointMpfRover : public PigPointBodyFixedCamera {

  public:

    PigPointMpfRover(PigCameraModel *cm, 
                     PigMission *mission, 
                     const char *instrument) 
                     : PigPointBodyFixedCamera(cm, mission, instrument) {
        //nothing to do...
    }

    virtual ~PigPointMpfRover() {
        // nothing to do...
    }

    // These are specific to MPF Rover.
    virtual const char *const getModelName() { return "MpfRover"; }

};
#endif

