////////////////////////////////////////////////////////////////////////
// PigPointMSLmahliCamera6dof
//
// Pointing model for Phoenix Lander MAHLI camera.
//
// Pointing parameters are camera orientation expressed using Euler angles
// and camera position both expressed relative to Payload coordinate system.
// Camera position is taken from the label, while Euler angles are derived
// from quaternion in the label.
// The advantages of using Euler angles over quaternion is a much easier 
// interpretation of what the numbers mean and what the tweaking to them
// would mean to camera orientation.  The disadvantages are the singularity
// at nadir direction.
//
////////////////////////////////////////////////////////////////////////
#ifndef PIGPOINTMSLMAHLICAMERA6DOF_H
#define PIGPOINTMSLMAHLICAMERA6DOF_H

#include "PigPointingModel.h"
#include "PigVector.h"
#include "PigQuaternion.h"
#include "PigCoordSystem.h"
#include "PigFileModelMSL.h"
#include "PigPointCamera6dof.h"

class PigPointMSLmahliCamera6dof : public PigPointCamera6dof {

  protected:

    // Read in the calibration pointing parameters for a given pointing file

    void read_point_info(char *filename, const char *host_id);

  public:

    PigPointMSLmahliCamera6dof(PigCameraModel *cm, 
			       PigMission *mission,
			       const char *instrument);
    virtual ~PigPointMSLmahliCamera6dof();


    virtual void pointCamera(PigFileModel *file);

    virtual void pointCamera(const char *obs_id, char *data_source)
	{ PigPointCamera6dof::pointCamera(obs_id, data_source); }
    virtual void pointCamera(PigPoint &position,
                             PigQuaternion &orientation,
                             PigCoordSystem *cs)
	{ PigPointCamera6dof::pointCamera(position, orientation, cs); }

   virtual void pointCamera(const double azimuth,
                             const double elevation,
                             const double twist,
                             const double x,
                             const double y,
                             const double z,
                             PigCoordSystem *cs)
     { PigPointCamera6dof::pointCamera(azimuth, elevation, twist, x, y, z, cs); }


    // These are specific to MSL MAHLI.

    virtual const char *const getModelName() { return "MSLmahliCamera6dof"; }

};

#endif
