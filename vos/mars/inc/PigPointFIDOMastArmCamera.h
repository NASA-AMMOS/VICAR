////////////////////////////////////////////////////////////////////////
// PigPointFIDOMastArmCamera
//
// Pointing model for FIDO Pancam camera.
//
// Currently works like Generic camera with 4 degrees of freedom:  Azimuth
// and Elevation.  Physically based pointing model is NOT yet implemented
// !!!!TBD
////////////////////////////////////////////////////////////////////////
#ifndef PIGPOINTFIDOMASTARMCAMERA_H
#define PIGPOINTFIDOMASTARMCAMERA_H

#include "PigPointingModel.h"
#include "PigFileModelFIDO.h"

#include "PigVector.h"
#include "PigQuaternion.h"
#include "PigCoordSystem.h"

// For Fido mast arm, Elevation =_mastJoint2 +_mastJoint3 +_mastJoint4 - 90
// 90 degrees is there because in fido kinematics 0 degrees is at 3 o'clock
// while in our CS it's at 6 o'clock
#define FIDO_ELEVATION(a2, a3, a4) ( (a2) + (a3) + (a4) - 90 )
#define FIDO_JOINT4(el, a2, a3) ( (el) + 90 - (a2) - (a3) )

class PigPointFIDOMastArmCamera : public PigPointingModel {

  protected:

    // Parameters from the pointing file...
    double _mastJoint1;
    double _mastJoint2;
    double _mastJoint3;
    double _mastJoint4;

    // Read in the calibration pointing parameters for a given pointing file

    void read_point_info(char *filename);

  public:

    PigPointFIDOMastArmCamera(PigCameraModel *cm, PigMission *mission,
				       const char *instrument);
    virtual ~PigPointFIDOMastArmCamera();

    // Point a camera model, given an image or an observation ID.  This
    // does not have to be the same image as was given in the constructor,
    // although presumably the mission/camera names should match!
    // data_source is included mainly to allow the overloading of these
    // two functions, but is intended to specify where to get the potining
    // data from, e.g. SPICE, database, etc.  Just pass NULL for this subclass.

    virtual void pointCamera(PigFileModel *file);
    virtual void pointCamera(const char *obs_id, char *data_source);
    
    //point the FIDO Mast Arm Camera via mast's four joint angles
    virtual void pointCamera(const double mastJoint1, const double mastJoint2,
			     const double mastJoint3, const double mastJoint4);

    virtual void pointCamera(const double azimuth, const double elevation);

    // These functions get and set the pointing and camera position, in
    // the given coordinates.  They are *not* intended for pointing
    // correction, use the get/setPointingParameters() functions for that.

    virtual void setCameraOrientation(const PigVector &orientation,
				      PigCoordSystem *cs);
    // These functions allow access to the "raw" pointing data, in order
    // to accomplish pointing corrections or "tweaks".
    // For the RAC camera:
    //    params[0] = MastJoint1
    //    params[1] = MastJoint2
    //    params[2] = MastJoint3
    //    params[3] = MastJoint4
    // Angles are in degrees!

	// whatever the parameters end up being...
    virtual int getPointingParamCount() { return 4; }
    virtual void getPointingParameters(double params[], const int max_count);
    virtual void setPointingParameters(const double params[], const int count);

    virtual const char *const getPointingParamName(int i);      // 0-based
    virtual const char *const getModelName() { return "FIDOMastArmCamera"; }
};

#endif
