////////////////////////////////////////////////////////////////////////
// PigPointGenericCamera_7dof
//
// Pointing model for a generic camera with 7 degrees of freedom:
// quat-s, quat-v1, quat-v2, quat-v3, x, y, z
//
// The "calibration" pointing, as well as initial pointing, is derived from
// the initial camera model.
////////////////////////////////////////////////////////////////////////
#ifndef PIGPOINTGENERICCAMERA_7DOF_H
#define PIGPOINTGENERICCAMERA_7DOF_H

#include "PigPointCamera7dof.h"

#include "PigVector.h"
#include "PigQuaternion.h"
#include "PigCoordSystem.h"

class PigPointGenericCamera_7dof : public PigPointCamera7dof {

  protected:

    virtual void read_point_file(char *filename, const char *host_id);

  public:

    PigPointGenericCamera_7dof(PigCameraModel *cm, PigMission *mission,
				       const char *instrument);
    virtual ~PigPointGenericCamera_7dof();

    // Point a camera model, given an image or an observation ID.  This
    // does not have to be the same image as was given in the constructor,
    // although presumably the mission/camera names should match!
    // data_source is included mainly to allow the overloading of these
    // two functions, but is intended to specify where to get the potining
    // data from, e.g. SPICE, database, etc.  Just pass NULL for this subclass.

    virtual void pointCamera(PigFileModel *file);
    virtual void pointCamera(const char *obs_id, char *data_source);

    // This is repeated here only to make C++ compiler happy.
    virtual void pointCamera(PigPoint &position, PigQuaternion &orientation,
							PigCoordSystem *cs)
	{ PigPointCamera7dof::pointCamera(position, orientation, cs); }

    virtual const char *const getModelName() { return "GenericCamera_7dof"; }

};

#endif
