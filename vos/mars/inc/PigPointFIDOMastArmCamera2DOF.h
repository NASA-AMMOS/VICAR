////////////////////////////////////////////////////////////////////////
// PigPointFIDOMastArmCamera2DOF
//
// Pointing model for FIDO Pancam camera.
//
// Currently works like Generic camera with 2 degrees of freedom:  Azimuth
// and Elevation.
// !!!!TBD
////////////////////////////////////////////////////////////////////////
#ifndef PIGPOINTFIDOMASTARMCAMERA2DOF_H
#define PIGPOINTFIDOMASTARMCAMERA2DOF_H

#include "PigPointFIDOMastArmCamera.h"


class PigPointFIDOMastArmCamera2DOF : public PigPointFIDOMastArmCamera {

  public:

    PigPointFIDOMastArmCamera2DOF(PigCameraModel *cm, PigMission *mission,
				       const char *instrument);
    virtual ~PigPointFIDOMastArmCamera2DOF();

	// whatever the parameters end up being...
    virtual int getPointingParamCount() { return 2; }
    virtual void getPointingParameters(double params[], const int max_count);
    virtual void setPointingParameters(const double params[], const int count);

    virtual const char *const getPointingParamName(int i);      // 0-based
    virtual const char *const getModelName() {return "FIDOMastArmCamera2DOF"; }
};

#endif
