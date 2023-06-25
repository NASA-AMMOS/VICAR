////////////////////////////////////////////////////////////////////////
// PigPointMERiddCamera
//
// Pointing model for MER Microscopic Imager(MI) camera located on
// Instrument Deployment Device(IDD) arm.
//
// Currently works like Generic camera with 4 degrees of freedom.
//
////////////////////////////////////////////////////////////////////////
#ifndef PIGPOINTMERIDDCAMERA_H
#define PIGPOINTMERIDDCAMERA_H

#include "PigPointingModel.h"
#include "PigFileModelMER.h"

#include "PigVector.h"
#include "PigQuaternion.h"
#include "PigCoordSystem.h"

class PigPointMERiddCamera : public PigPointingModel {

  protected:

    // User Parameters
    // Parameters from the pointing file...
    PigPoint _idd_rotation_point;
    PigQuaternion _idd_calibration_quaternion;
    PigQuaternion _idd_gimbal_to_rover_quaternion;

    // Pointing parameters
    double _iddJoint1;
    double _iddJoint2;
    double _iddJoint3;
    double _iddJoint4;
    double _iddJoint5;

    double _pointing_error[5];		// point parameters expected errors

    // Read in the calibration pointing parameters from a given pointing file

    char _parms_path[PIG_MAX_FILENAME_SIZE];

    void read_point_info(char *filename, const char *host_id);

  public:

    PigPointMERiddCamera(PigCameraModel *cm, 
			 PigMission *mission,
			 const char *instrument);
    virtual ~PigPointMERiddCamera();

    // Point a camera model, given an image or an observation ID.  This
    // does not have to be the same image as was given in the constructor,
    // although presumably the mission/camera names should match!
    // data_source is included mainly to allow the overloading of these
    // two functions, but is intended to specify where to get the potining
    // data from, e.g. SPICE, database, etc.  Just pass NULL for this subclass.

    virtual void pointCamera(PigFileModel *file);
    virtual void pointCamera(const char *obs_id, char *data_source);
  
    //point the MER IDD Arm Camera via IDD's four joint angles
    virtual void pointCamera(const double iddJoint1, const double iddJoint2,
			     const double iddJoint3, const double iddJoint4,
			     const double iddJoint5);

    // These functions get and set the pointing and camera position, in
    // the given coordinates.  They are *not* intended for pointing
    // correction, use the get/setPointingParameters() functions for that.

    virtual void setCameraOrientation(const PigVector &orientation,
				      PigCoordSystem *cs);

    // These functions allow access to the "raw" pointing data, in order
    // to accomplish pointing corrections or "tweaks".
    // whatever the parameters end up being...
    virtual int getPointingParamCount() { return 5; }
    virtual void getPointingParameters(double params[], const int max_count);
    virtual void setPointingParameters(const double params[], const int count);
    virtual void getPointingErrorEstimate(double params[], const int max_count);
    virtual void forceCameraRepoint();
    virtual const char *const getPointingParamName(int i);      // 0-based
    virtual const char *const getModelName() { return "MERiddCamera"; }
};

#endif
