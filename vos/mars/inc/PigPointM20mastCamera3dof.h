////////////////////////////////////////////////////////////////////////
// PigPointM20mastCamera3dof
//
// Pointing model for M20 MastcamZ, NavCam, and SuperCam cameras.
//
// Currently works like Generic camera with 3 degrees of freedom:  Pan
// Tilt and Twist.  Twist allows to compensate for calibration 
// camera model's errors by rotating around A-vector axis 
////////////////////////////////////////////////////////////////////////
#ifndef PIGPOINTM20MASTCAMERA3DOF_H
#define PIGPOINTM20MASTCAMERA3DOF_H

#include "PigPointingModel.h"

#include "PigVector.h"
#include "PigQuaternion.h"
#include "PigCoordSystem.h"

#include "PigPointM20mastCamera.h"

class PigPointM20mastCamera3dof : public PigPointM20mastCamera {

 protected:

  // twist angle in degrees.
  double _twist;

  public:

    PigPointM20mastCamera3dof(PigCameraModel *cm, 
			     PigMission *mission,
			     const char *instrument);
    virtual ~PigPointM20mastCamera3dof();
 
    virtual void pointCamera(PigFileModel *file) 
               { PigPointM20mastCamera::pointCamera(file); };

    virtual void pointCamera(const char *obs_id, char *data_source)
               { PigPointM20mastCamera::pointCamera(obs_id, data_source); };
 
    virtual void pointCamera(const double azimuth, 
			     const double elevation)
               { PigPointM20mastCamera::pointCamera(azimuth, elevation); }; 

    // Calls superclass to point camera using az and el
    // Then rotates Camera around A-vector axis by twist angle amount.
    virtual void pointCamera(const double azimuth, 
			     const double elevation,
			     const double twist);

    virtual int getPointingParamCount() { return 3; }
    virtual void getPointingParameters(double params[], const int max_count);
    virtual void setPointingParameters(const double params[], const int count);
    virtual void forceCameraRepoint();
    virtual const char *const getPointingParamName(int i);      // 0-based
    virtual void getPointingErrorEstimate(double errors[],const int max_count);
    virtual const char *const getModelName() { return "M20mastCamera3dof"; }
};

#endif
