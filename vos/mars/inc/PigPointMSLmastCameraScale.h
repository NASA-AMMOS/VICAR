////////////////////////////////////////////////////////////////////////
// PigPointMSLmastCameraScale
//
// Pointing model for MSL Pancam & NavCam cameras.
//
// Similar to Generic camera with 3 degrees of freedom:  Pan
// Tilt and Twist, but adds an additional component, Scale.  Scale changes
// the image scale of the camera (essentially zooming in or out slightly).
////////////////////////////////////////////////////////////////////////
#ifndef PIGPOINTMSLMASTCAMERASCALE_H
#define PIGPOINTMSLMASTCAMERASCALE_H

#include "PigPointingModel.h"

#include "PigVector.h"
#include "PigQuaternion.h"
#include "PigCoordSystem.h"

#include "PigPointMSLmastCamera3dof.h"

class PigPointMSLmastCameraScale : public PigPointMSLmastCamera3dof {

 protected:

  // Scale factor.  Unitless, should be near 1.0.  >1 expands the image.
  double _scale;

  public:

    PigPointMSLmastCameraScale(PigCameraModel *cm, 
			     PigMission *mission,
			     const char *instrument);
    virtual ~PigPointMSLmastCameraScale();
 
    virtual void pointCamera(PigFileModel *file) 
               { PigPointMSLmastCamera3dof::pointCamera(file); };

    virtual void pointCamera(const char *obs_id, char *data_source)
               { PigPointMSLmastCamera3dof::pointCamera(obs_id, data_source); };
 
    virtual void pointCamera(const double azimuth, 
			     const double elevation)
               { PigPointMSLmastCamera3dof::pointCamera(azimuth, elevation); }; 

    virtual void pointCamera(const double azimuth,
			     const double elevation,
			     const double twist)
	{ PigPointMSLmastCamera3dof::pointCamera(azimuth, elevation, twist); };

    // Calls superclass to point camera using az and el and twist
    // Then scales camera by the scale amount.
    virtual void pointCamera(const double azimuth, 
			     const double elevation,
			     const double twist,
			     const double scale);

    virtual int getPointingParamCount() { return 4; }
    virtual void getPointingParameters(double params[], const int max_count);
    virtual void setPointingParameters(const double params[], const int count);
    virtual void forceCameraRepoint();
    virtual const char *const getPointingParamName(int i);      // 0-based
    virtual void getPointingErrorEstimate(double errors[],const int max_count);
    virtual const char *const getModelName() { return "MSLmastCameraScale"; }
};

#endif
