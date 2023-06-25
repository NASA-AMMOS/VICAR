////////////////////////////////////////////////////////////////////////
// PigPointM20heliCamera6dof
//
// Pointing model for Ingenuity helicopter cameras (nav, RTE).
// Ingenuity cameras system is conceptually considered as a arm camera
// (a virtual long arm from the take off location to the heli position/
// orientation)
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
#ifndef PIGPOINTM20HELICAMERA6DOF_H
#define PIGPOINTM20HELICAMERA6DOF_H

#include "PigPointM20armCamera6dof.h"

class PigPointM20heliCamera6dof : public PigPointM20armCamera6dof {

  protected:

  public:

    PigPointM20heliCamera6dof(PigCameraModel *cm, 
			       PigMission *mission,
			       const char *instrument) : PigPointM20armCamera6dof(cm, mission, instrument) {};
    virtual ~PigPointM20heliCamera6dof();


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


    // These are specific to M20

    virtual const char *const getModelName() { return "M20heliCamera6dof"; }

};

#endif
