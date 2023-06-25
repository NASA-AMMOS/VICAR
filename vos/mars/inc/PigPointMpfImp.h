////////////////////////////////////////////////////////////////////////
// PigPointMpfImp
//
// Pointing model for Mars Pathfinder IMP camera.
//
// Pointing parameters are derived from the image label.  Overrides can
// occur by passing in the az/el directly via routines specific to this
// subclass (by mission-specific code only of course).
////////////////////////////////////////////////////////////////////////
#ifndef PIGPOINTMPFIMP_H
#define PIGPOINTMPFIMP_H

#include "PigPointPanTiltCamera.h"

#include "PigVector.h"
#include "PigQuaternion.h"

class PigPointMpfImp : public PigPointPanTiltCamera {

  protected:

    int _deployed;
    int _deployed_override;

  public:

    PigPointMpfImp(PigCameraModel *cm, PigMission *mission,
				       const char *instrument);
    virtual ~PigPointMpfImp();

    // Point a camera model, given an image or an observation ID.  This
    // does not have to be the same image as was given in the constructor,
    // although presumably the mission/camera names should match!
    // data_source is included mainly to allow the overloading of these
    // two functions, but is intended to specify where to get the potining
    // data from, e.g. SPICE, database, etc.  Just pass NULL for this subclass.

    virtual void pointCamera(PigFileModel *file);
    virtual void pointCamera(const char *obs_id, char *data_source);

    // Override of PigPointPanTiltCamera's pointCamera()
    virtual void pointCamera(const double azimuth, const double elevation,
				PigCoordSystem *cs);

    // These are specific to MPF IMP.  The set... functions override any
    // value the label might have for all future pointings.  The clear...
    // functions clear this override, so the label will be used again.
    // The get functions return the override, or the value from the last
    // pointing if there is no override.

    virtual void setDeployedState(const int state);
    virtual const int getDeployedState() { return _deployed; }
    virtual void clearDeployedState();

    virtual const char *const getModelName() { return "MpfImp"; }

};

#endif

