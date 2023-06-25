////////////////////////////////////////////////////////////////////////
// PigPointPHXssi3dof
//
// Pointing model for Phoenix Lander SSI camera.
//
// Currently works like Generic camera with 3 degrees of freedom:  Pan
// Tilt and Twist.  Twist allows to compensate for calibration
// camera model's errors by rotating around A-vector axis
////////////////////////////////////////////////////////////////////////
#ifndef PIGPOINTPHXSSICAMERA3DOF_H
#define PIGPOINTPHXSSICAMERA3DOF_H

#include "PigVector.h"
#include "PigQuaternion.h"
#include "PigCoordSystem.h"

#include "PigPointPHXssiCamera.h"

class PigPointPHXssiCamera3dof : public PigPointPHXssiCamera {

  protected:

  // twist angle in degrees.
  double _twist;

  public:

    PigPointPHXssiCamera3dof(PigCameraModel *cm, PigMission *mission,
				       const char *instrument);
    virtual ~PigPointPHXssiCamera3dof();

     virtual void pointCamera(PigFileModel *file)
               { PigPointPHXssiCamera::pointCamera(file); };

    virtual void pointCamera(const char *obs_id, char *data_source)
               { PigPointPHXssiCamera::pointCamera(obs_id, data_source); };

    virtual void pointCamera(const double azimuth,
                             const double elevation,
                             PigCoordSystem *cs)
               { PigPointPHXssiCamera::pointCamera(azimuth, elevation, cs); };

    // Calls superclass to point camera using az and el
    // Then rotates Camera around A-vector axis by twist angle amount.
    virtual void pointCamera(const double azimuth,
                             const double elevation,
                             const double twist,
                             PigCoordSystem *cs);


        // whatever the parameters end up being...
    virtual int getPointingParamCount() { return 3; }
    virtual void getPointingParameters(double params[], const int max_count);
    virtual void setPointingParameters(const double params[], const int count);
    virtual void forceCameraRepoint();
    virtual const char *const getPointingParamName(int i);      // 0-based
    virtual void getPointingErrorEstimate(double errors[],const int max_count);
    virtual const char *const getModelName() { return "PHXssiCamera3dof"; }
};

#endif
