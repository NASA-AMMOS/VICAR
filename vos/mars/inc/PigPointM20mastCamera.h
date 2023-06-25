////////////////////////////////////////////////////////////////////////
// PigPointM20mastCamera
//
// Pointing model for M20 MastCamZ, NavCam, SuperCam cameras.
//
// The kinematics are copied from the FSW.  The math is slightly different
// but amounts to the same thing as used on MER/PHX.
//
// We do not inherit from PigPointPanTiltCamera because we do not want
// a cs object to be provided to pointCamera.  The "az/el" values are joint
// angles so they're not really expressed in any specific coord system.
// So, having a CS parameter implies the ability to do a conversion that
// does not make sense here.
////////////////////////////////////////////////////////////////////////
#ifndef PIGPOINTM20MASTCAMERA_H
#define PIGPOINTM20MASTCAMERA_H

#include "PigPointingModel.h"
#include "PigFileModelM20.h"

#include "PigVector.h"
#include "PigQuaternion.h"
#include "PigCoordSystem.h"

class PigPointM20mastCamera : public PigPointingModel {

  protected:

    double _azimuth;		// Joint angles (pointing params)
    double _elevation;

    // Parameters from the pointing file...
    // Note, the points are converted from rover mech to nav frame when read
    double _home_az;		// (Joint) Az when head is forward, radians
    double _home_el;		// (Joint) El when head is level, radians
    PigPoint _az_point;		// Point on azimuth axis, ROVER NAV frame
    PigVector _az_axis;		// Azimuth axis unit vector, ROVER NAV frame
    PigPoint _el_point;		// Point on elevation axis, ROVER NAV frame
    PigVector _el_axis;		// Elevation axis unit vector, ROVER NAV frame
    double _el_azimuth;		// (Joint) azimuth during elevation survey,
				// middle of backlash, radians
    PigPoint _rsm_calibration_position; // Position of head during cal, RVR NAV
    PigQuaternion _rsm_calibration_quaternion; // Quat of head during cal
    PigPoint _rmech_to_rnav;	// Add this to rmech value to get rnav

    double _pointing_error[4];	// az, el, twist, scale expected pointing error
				// twist and scale are not part of this class;
				// we simply maintain it here to simplify the
				// subclasses

    // Read in the calibration pointing parameters for a given pointing file

    void read_point_info(char *filename, const char *host_id);

  public:

    PigPointM20mastCamera(PigCameraModel *cm, 
			 PigMission *mission,
			 const char *instrument);
    virtual ~PigPointM20mastCamera();

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
    // Az and el are in degrees.  They are JOINT angles, which for M20 do not
    // corrspond to anything useful (they are angles from the hard stop).
    // Straight ahead/level is not 0,0 but is az=181 el=91 (nominal).  el=0 is
    // one degree past stright down, and the el range is 0-182.  Likewise az=0
    // is one degree past backward, and the az range is 0-362.  The hard stops
    // go one degree extra on each extreme.

    virtual void pointCamera(const double azimuth, const double elevation);

    // These functions get and set the pointing and camera position, in
    // the given coordinates.  They are *not* intended for pointing
    // correction, use the get/setPointingParameters() functions for that.

    virtual void setCameraOrientation(const PigVector &orientation,
				      PigCoordSystem *cs);

    // Pointing parameters are joint angles...
    virtual int getPointingParamCount() { return 2; }
    virtual void getPointingParameters(double params[], const int max_count);
    virtual void setPointingParameters(const double params[], const int count);
    virtual void forceCameraRepoint();
    virtual void getPointingErrorEstimate(double params[], const int max_count);
    virtual const char *const getPointingParamName(int i);      // 0-based

    virtual const char *const getModelName() { return "M20mastCamera"; }
};

#endif
