////////////////////////////////////////////////////////////////////////
// PigPointPHXracCamera6dof
//
// Pointing model for Phoenix Lander RAC camera.
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
#ifndef PIGPOINTPHXRACCAMERA6DOF_H
#define PIGPOINTPHXRACCAMERA6DOF_H

#include "PigPointingModel.h"
#include "PigVector.h"
#include "PigQuaternion.h"
#include "PigCoordSystem.h"
#include "PigFileModelPHX.h"
#include "PigPointPHXracCamera.h"

class PigPointPHXracCamera6dof : public PigPointPHXracCamera {

  protected:

    // Camera orientation for PHX RAC is described as quaternion
    // in the label, we convert quaternion to Euler angles and then
    // map yaw to az, pitch to elevation, roll to twist.  This is
    // based on how RAC joint angles move the arm.
    double _azimuth;
    double _elevation;
    double _twist;
    
    // Read in the calibration pointing parameters for a given pointing file

    void read_point_info_6dof(char *filename, const char *host_id);

  public:

    PigPointPHXracCamera6dof(PigCameraModel *cm, 
			     PigMission *mission,
			     const char *instrument);
    virtual ~PigPointPHXracCamera6dof();

    virtual void pointCamera(PigFileModel *file);

    virtual void pointCamera(const char *obs_id, char *data_source)
               { PigPointPHXracCamera::pointCamera(obs_id, data_source); };
    
     virtual void pointCamera(PigPoint &position,
                              PigQuaternion &orientation,
                              PigCoordSystem *cs) 
               {PigPointPHXracCamera::pointCamera(position, orientation, cs); };

    // These functions are specific to the PHX RAC camera (not in base class)
    // "Point" camera based on x/y/z and az/el/twist pointing.  No kinematics or
    // arm motion rules are applied (i.e. the parameters can be anything,
    // including physically impossible positions).  All parameters are in
    // the PAYLOAD frame.

    virtual void pointCamera(const double azimuth,
                             const double elevation,
			     const double twist,
                             const double x,
                             const double y,
                             const double z,
	                     PigCoordSystem *cs);


    // These functions allow access to the "raw" pointing data, in order
    // to accomplish pointing corrections or "tweaks".
    // For the RAC camera:
    //    params[0] = Azimuth
    //    params[1] = Elevation    
    //    params[2] = Twist
    //    params[3] = X position
    //    params[4] = Y position
    //    params[5] = Z position
    // Angles are in degrees!

    // whatever the parameters end up being...
    virtual int getPointingParamCount() { return 6; }
    virtual void getPointingParameters(double params[], const int max_count);
    virtual void setPointingParameters(const double params[], const int count);

    virtual const char *const getPointingParamName(int i);      // 0-based
    // These are specific to PHX RAC.  The set... functions override any
    // value the label might have for all future pointings.  The clear...
    // functions clear this override, so the label will be used again.
    // The get functions return the override, or the value from the last
    // pointing if there is no override.

    virtual const char *const getModelName() { return "PHXracCamera6dof"; }

};

#endif
