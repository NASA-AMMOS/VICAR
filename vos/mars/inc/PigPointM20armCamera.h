////////////////////////////////////////////////////////////////////////
// PigPointM20armCamera
//
// Pointing model for M20 arm cameras (Watson, ACI, MCC).
// Uses a 7-DOF model (quat + pos).
//
// Pointing parameters are derived from the image label.  Overrides can
// occur by passing in the az/el directly via routines specific to this
// subclass (by mission-specific code only of course).
////////////////////////////////////////////////////////////////////////
#ifndef PIGPOINTM20ARMCAMERA_H
#define PIGPOINTM20ARMCAMERA_H

#include "PigPointCamera7dof.h"
#include "PigFileModelM20.h"

class PigPointM20armCamera : public PigPointCamera7dof {

  protected:

    // Read in the calibration pointing parameters for a given pointing file
    void read_point_info(char *filename, const char *host_id);

  public:

    PigPointM20armCamera(PigCameraModel *cm, 
			 PigMission *mission,
			  const char *instrument);
    virtual ~PigPointM20armCamera();

    // Point a camera model, given an image or an observation ID.  This
    // does not have to be the same image as was given in the constructor,
    // although presumably the mission/camera names should match!
    // data_source is included mainly to allow the overloading of these
    // two functions, but is intended to specify where to get the potining
    // data from, e.g. SPICE, database, etc.  Just pass NULL for this subclass.

    virtual void pointCamera(PigFileModel *file);

    virtual void pointCamera(const char *obs_id, char *data_source)
	{ PigPointCamera7dof::pointCamera(obs_id, data_source); }
    virtual void pointCamera(PigPoint &position,
                             PigQuaternion &orientation,
                             PigCoordSystem *cs)
	{ PigPointCamera7dof::pointCamera(position, orientation, cs); }

    virtual const char *const getModelName() { return "M20armCamera"; }

};

#endif
