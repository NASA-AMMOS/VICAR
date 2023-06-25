////////////////////////////////////////////////////////////////////////
// PigPointCamera6dof
//
// Pointing model for multi-mission camera using 6 degrees of freedom.
//
// Pointing parameters are camera orientation expressed using Euler angles
// and camera position both expressed relative to default coordinate system.
// Camera position is taken from the label, while Euler angles are derived
// from quaternion in the label.
// The advantages of using Euler angles over quaternion is a much easier 
// interpretation of what the numbers mean and what the tweaking to them
// would mean to camera orientation.  The disadvantages are the singularity
// at nadir direction.
//
////////////////////////////////////////////////////////////////////////
#ifndef PIGPOINTCAMERA6DOF_H
#define PIGPOINTCAMERA6DOF_H

#include "PigPointingModel.h"
#include "PigVector.h"
#include "PigQuaternion.h"
#include "PigCoordSystem.h"
#include "PigFileModel.h"
#include "PigPointCamera7dof.h"

class PigPointCamera6dof : public PigPointCamera7dof {

  protected:

    // Camera orientation is described as quaternion
    // in the label, we convert quaternion to Euler angles and then
    // map yaw to az, pitch to elevation, roll to twist.  This is
    // based on how joint angles move the arm.
    double _azimuth;
    double _elevation;
    double _twist;
    
    // Read in the calibration pointing parameters for a given pointing file

    void read_point_info(char *filename, const char *host_id);

  public:

    PigPointCamera6dof(PigCameraModel *cm, 
			     PigMission *mission,
			     const char *instrument);
    virtual ~PigPointCamera6dof();


    virtual void pointCamera(PigFileModel *file) = 0;
    virtual void pointCamera(const char *obs_id, char *data_source) {PigPointCamera7dof::pointCamera(obs_id, data_source);}
    virtual void pointCamera(PigPoint &position,
                             PigQuaternion &orientation,
                             PigCoordSystem *cs){PigPointCamera7dof::pointCamera(position, orientation, cs);}

    // "Point" camera based on x/y/z and az/el/twist pointing.  No kinematics or
    // arm motion rules are applied (i.e. the parameters can be anything,
    // including physically impossible positions).  All parameters are in
    // the Default frame.

    virtual void pointCamera(const double azimuth,
                             const double elevation,
			     const double twist,
                             const double x,
                             const double y,
                             const double z,
	                     PigCoordSystem *cs);

    virtual void setCameraOrientation(const PigVector &orientation,
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
    // The set... functions override any
    // value the label might have for all future pointings.  The clear...
    // functions clear this override, so the label will be used again.
    // The get functions return the override, or the value from the last
    // pointing if there is no override.

    virtual const char *const getModelName() { return "PigPointCamera6dof"; }

};

#endif
