////////////////////////////////////////////////////////////////////////
// PigPointGenericCamera_6dof
//
// Pointing model for a generic camera with 6 degrees of freedom:
// az, el, twist, x, y, z
//
// The "calibration" pointing, as well as initial pointing, is derived from
// the initial camera model.
////////////////////////////////////////////////////////////////////////
#ifndef PIGPOINTGENERICCAMERA_6DOF_H
#define PIGPOINTGENERICCAMERA_6DOF_H

#include "PigPointCamera6dof.h"

#include "PigVector.h"
#include "PigQuaternion.h"
#include "PigCoordSystem.h"

class PigPointGenericCamera_6dof : public PigPointCamera6dof {

  protected:

    virtual void read_point_file(char *filename, const char *host_id);

  public:

    PigPointGenericCamera_6dof(PigCameraModel *cm, PigMission *mission,
				       const char *instrument);
    virtual ~PigPointGenericCamera_6dof();

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
	{ PigPointCamera6dof::pointCamera(position, orientation, cs); }

    virtual const char *const getModelName() { return "GenericCamera_6dof"; }

};

#endif
