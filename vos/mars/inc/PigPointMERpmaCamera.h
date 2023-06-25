////////////////////////////////////////////////////////////////////////
// PigPointMERpmaCamera
//
// Pointing model for MER Pancam & NavCam cameras.
//
// Currently works like Generic camera with 2 degrees of freedom:  Azimuth
// and Elevation.  Physically based pointing model is NOT yet implemented
// !!!!TBD
////////////////////////////////////////////////////////////////////////
#ifndef PIGPOINTMERMASTARMCAMERA_H
#define PIGPOINTMERMASTARMCAMERA_H

#include "PigPointingModel.h"
#include "PigFileModelMER.h"

#include "PigVector.h"
#include "PigQuaternion.h"
#include "PigCoordSystem.h"

#include "PigPointPanTiltCamera.h"

class PigPointMERpmaCamera : public PigPointPanTiltCamera {

  protected:

    // Parameters from the pointing file...
    PigPoint _pma_rotation_point;
    PigQuaternion _pma_calibration_quaternion;
    PigQuaternion _pma_gimbal_to_rover_quaternion;
    double _pma_gimb_off_azimuth;
    double _pma_gimb_off_elevation;

    // Read in the calibration pointing parameters for a given pointing file

    void read_point_info(char *filename, const char *host_id);

  public:

    PigPointMERpmaCamera(PigCameraModel *cm, 
			 PigMission *mission,
			 const char *instrument);
    virtual ~PigPointMERpmaCamera();

    // Point a camera model, given an image or an observation ID.  This
    // does not have to be the same image as was given in the constructor,
    // although presumably the mission/camera names should match!
    // data_source is included mainly to allow the overloading of these
    // two functions, but is intended to specify where to get the potining
    // data from, e.g. SPICE, database, etc.  Just pass NULL for this subclass.

    virtual void pointCamera(PigFileModel *file);
    virtual void pointCamera(const char *obs_id, char *data_source);
 
    // Do the actual work of pointing the camera.
    //
    // Az and el are in degrees.  They are mesured in the given coordinate
    // system (if NULL, _pointing_cs, the natural instrument frame, is used)
    //
    // There are two two kinds of az/el values: Motor values that we are 
    // getting from the label, and idealized values, which we use in the
    // kinematics calculations.
    // The formulas are:
    // idealized_az|el = motor_az|el  + pma_gimb_off_azimuth|elevation
    //  
    // Because Motor values match the label, we want the exposed pointing 
    // parameters(used in nav files, MICA, etc.) to be the Motor angles.  
    // Thus we add angle offsets only AFTER _azimuth and _elevation have been 
    // assigned.  After this assignment is done, we add offsets to compute
    // q_el, g_az.  That way only this function pointCamera(az, el, cs) knows
    // abouth gimbal offsets.
    virtual void pointCamera(const double azimuth, 
			     const double elevation,
			     PigCoordSystem *cs); 


    // These functions get and set the pointing and camera position, in
    // the given coordinates.  They are *not* intended for pointing
    // correction, use the get/setPointingParameters() functions for that.

    virtual void setCameraOrientation(const PigVector &orientation,
				      PigCoordSystem *cs);

	// whatever the parameters end up being...
    virtual int getPointingParamCount() { return 2; }
    virtual void getPointingParameters(double params[], const int max_count);
    virtual void setPointingParameters(const double params[], const int count);

    virtual const char *const getPointingParamName(int i);      // 0-based
    virtual const char *const getModelName() { return "MERpmaCamera"; }
};

#endif
